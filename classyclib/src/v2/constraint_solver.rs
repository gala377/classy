use std::collections::{HashMap, VecDeque};

use crate::{
    scope::FlatScope,
    session::Session,
    typecheck::{ast_to_type::PrefexScope, types::DeBruijn},
    v2::{
        constraint_generation::Constraint,
        knowledge::{Database, DefinitionId, GenericConstraint, Id, TypeId},
        method_resolver::{MethodResolutionError, MethodResolver, ResolvedMethod},
        ty::{Type, TypeFolder},
    },
};

pub(super) struct FreshTypeReplacer {
    pub substitutions: HashMap<usize, Type>,
}

impl TypeFolder for FreshTypeReplacer {
    type Error = ();
    fn fold_fresh(&mut self, id: usize) -> Result<Type, ()> {
        match self.substitutions.get(&id) {
            Some(t) => self.fold_type(t.clone()),
            None => Ok(Type::Fresh(id)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CallResolution {
    StaticFunction(Id<DefinitionId>),
    StaticMethod(Id<DefinitionId>),
    FromInstanceInScope {
        instance_index: usize,
        method_def: Id<DefinitionId>,
    },
    Unresolved,
}

pub struct ConstraintSolver<'db, 'sess> {
    constraints: VecDeque<Constraint>,
    database: &'db Database,
    pub substitutions: Vec<(usize, Type)>,
    session: &'sess Session,

    prefex_scope: PrefexScope,

    current_namespace: Vec<String>,

    constraints_in_scope: FlatScope<GenericConstraint>,
    visible_instances: Vec<Id<DefinitionId>>,
    visible_method_blocks: Vec<Id<DefinitionId>>,
    // probably all types of the compilation need to be there
    // as a method can return a type that is not imported
    types: Vec<Id<DefinitionId>>,
    classes: Vec<Id<DefinitionId>>,
    pub call_resolutions: HashMap<usize, CallResolution>,
}

impl<'db, 'sess> ConstraintSolver<'db, 'sess> {
    pub fn new(
        session: &'sess Session,
        database: &'db Database,

        current_namespace: Vec<String>,
        prefex_scope: PrefexScope,
        mut constraints: Vec<Constraint>,

        constraints_in_scope: FlatScope<GenericConstraint>,
        visible_instances: Vec<Id<DefinitionId>>,
        visible_method_blocks: Vec<Id<DefinitionId>>,
        // probably all types of the compilation need to be there
        // as a method can return a type that is not imported
        types: Vec<Id<DefinitionId>>,
        classes: Vec<Id<DefinitionId>>,
    ) -> Self {
        constraints.reverse();
        ConstraintSolver {
            session,
            constraints: constraints.into(),
            database,
            substitutions: Vec::new(),
            current_namespace,
            prefex_scope,
            constraints_in_scope,
            visible_instances,
            visible_method_blocks,
            types,
            classes,
            call_resolutions: HashMap::new(),
        }
    }
    pub fn solve(&mut self) {
        while let Some(constraint) = self.constraints.pop_back() {
            self.solve_constraint(constraint);
        }
    }

    pub fn solve_constraint(&mut self, constraint: Constraint) {
        match constraint {
            Constraint::Eq(Type::Bool, Type::Bool)
            | Constraint::Eq(Type::Int, Type::Int)
            | Constraint::Eq(Type::UInt, Type::UInt)
            | Constraint::Eq(Type::Byte, Type::Byte)
            | Constraint::Eq(Type::Float, Type::Float)
            | Constraint::Eq(Type::String, Type::String)
            | Constraint::Eq(Type::Unit, Type::Unit)
            | Constraint::Eq(Type::Divergent, Type::Divergent) => {}

            Constraint::Eq(Type::Divergent, _) => {}
            Constraint::Eq(_, Type::Divergent) => {}
            Constraint::Eq(Type::Alias(id1), Type::Alias(id2)) if id1 == id2 => {}
            Constraint::Eq(Type::Alias(id), t) => {
                let resolved = self.database.resolve_alias_to_type(id).unwrap();
                self.constraints.push_back(Constraint::Eq(resolved, t));
            }
            Constraint::Eq(t, Type::Alias(id)) => {
                let resolved = self.database.resolve_alias_to_type(id).unwrap();
                self.constraints.push_back(Constraint::Eq(t, resolved));
            }
            Constraint::Eq(Type::Fresh(id1), Type::Fresh(id2)) if id1 == id2 => {}
            Constraint::Eq(Type::Struct { def: def_1, .. }, Type::Struct { def: def_2, .. })
                if def_1 == def_2 => {}

            Constraint::Eq(Type::ADT { def: def_1, .. }, Type::ADT { def: def_2, .. })
                if def_1 == def_2 => {}
            Constraint::Eq(Type::Tuple(t_1), Type::Tuple(t_2)) => {
                for (t1, t2) in t_1.iter().zip(t_2.iter()) {
                    self.constraints
                        .push_back(Constraint::Eq(t1.clone(), t2.clone()));
                }
            }
            Constraint::Eq(Type::Array(t_1), Type::Array(t_2)) => {
                self.constraints.push_back(Constraint::Eq(*t_1, *t_2));
            }

            Constraint::Eq(Type::App { typ, args }, app2 @ Type::App { .. }) => {
                self.constraints
                    .push_back(Constraint::Eq(instance(self.database, args, *typ), app2));
            }
            Constraint::Eq(app @ Type::App { .. }, t) => {
                self.constraints.push_back(Constraint::Eq(t, app));
            }
            Constraint::Eq(Type::Fresh(id), other @ Type::App { .. }) => {
                self.substitutions.push((id, other.clone()));
                self.replace_in_constraints(id, other)
            }
            Constraint::Eq(t, Type::App { typ: app_t, args }) => {
                self.constraints
                    .push_back(Constraint::Eq(t, instance(self.database, args, *app_t)));
            }
            Constraint::Eq(Type::Fresh(id1), other) => {
                self.substitutions.push((id1, other.clone()));
                self.replace_in_constraints(id1, other);
            }
            Constraint::Eq(other, Type::Fresh(id1)) => {
                self.substitutions.push((id1, other.clone()));
                self.replace_in_constraints(id1, other);
            }

            Constraint::Eq(
                Type::Function {
                    args: args_1,
                    ret: ret_1,
                },
                Type::Function {
                    args: args_2,
                    ret: ret_2,
                },
            ) => {
                self.constraints.push_back(Constraint::Eq(*ret_1, *ret_2));
                if args_1.len() != args_2.len() {
                    panic!(
                        "Number of arguments mismatched expected {} got {}",
                        args_2.len(),
                        args_1.len(),
                    );
                }
                for (a1, a2) in args_1.iter().zip(args_2.iter()) {
                    self.constraints
                        .push_back(Constraint::Eq(a1.clone(), a2.clone()));
                }
            }
            Constraint::Eq(
                f @ (Type::Function { .. } | Type::ADT { .. } | Type::Struct { .. }),
                s @ Type::Scheme { .. },
            ) => {
                self.constraints.push_back(Constraint::Eq(s, f));
            }
            Constraint::Eq(
                Type::Scheme { prefex, typ },
                func_1 @ (Type::Function { .. } | Type::ADT { .. } | Type::Struct { .. }),
            ) => {
                let args = prefex.iter().map(|_| self.fresh_type()).collect();
                self.constraints.push_back(Constraint::Eq(
                    func_1,
                    Type::App {
                        typ: Box::new(Type::Scheme { prefex, typ }),
                        args,
                    },
                ));
            }
            Constraint::Eq(Type::Generic(d1, i1), Type::Generic(d2, i2))
                if d1 == d2 && i1 == i2 => {}
            Constraint::Eq(
                Type::Scheme {
                    prefex: p1,
                    typ: t1,
                },
                Type::Scheme {
                    prefex: p2,
                    typ: t2,
                },
            ) if p1 == p2 && t1 == t2 => {}
            Constraint::Eq(Type::Generic(depth_1, index_1), Type::Generic(depth_2, index_2))
                if depth_1 != depth_2 || index_1 != index_2 =>
            {
                panic!("Cannot unify 2 different generic arguments together")
            }

            Constraint::HasProperty {
                ty: Type::Struct { fields, .. },
                property,
                of_type,
            } => {
                let f = fields
                    .iter()
                    .find(|(f, _)| f == &property)
                    .unwrap_or_else(|| panic!("field {property} does not exists on this struct"));
                self.constraints
                    .push_back(Constraint::Eq(f.1.clone(), of_type));
            }
            Constraint::HasProperty {
                ty: Type::Alias(id),
                property,
                of_type,
            } => {
                self.constraints.push_back(Constraint::HasProperty {
                    ty: self.database.resolve_alias_to_type(id).unwrap(),
                    property,
                    of_type,
                });
            }
            Constraint::HasProperty {
                ty: Type::App { typ, args },
                property,
                of_type,
            } => {
                self.constraints.push_back(Constraint::HasProperty {
                    ty: instance(self.database, args, *typ),
                    property,
                    of_type,
                });
            }
            Constraint::HasProperty {
                ty: Type::Scheme { prefex, typ },
                property,
                of_type,
            } => {
                let args = prefex.iter().map(|_| self.fresh_type()).collect();
                self.constraints.push_back(Constraint::HasProperty {
                    ty: Type::App {
                        typ: Box::new(Type::Scheme { prefex, typ }),
                        args,
                    },
                    property,
                    of_type,
                });
            }

            Constraint::HasCase {
                ty: Type::Struct { def, fields },
                case,
                of_type,
            } => {
                let name = self.database.get_definition(def).unwrap().name.clone();
                if case != name {
                    panic!("Cannot unify case {case} with {name}");
                }
                let Type::Struct { fields: pat_f, .. } = of_type else {
                    panic!("Expected a struct type in pattern got {of_type:?}");
                };
                for (fname, ftyp) in pat_f {
                    self.constraints.push_back(Constraint::HasProperty {
                        ty: Type::Struct {
                            def,
                            fields: fields.clone(),
                        },
                        property: fname.clone(),
                        of_type: ftyp.clone(),
                    });
                }
            }

            Constraint::HasCase {
                ty: Type::ADT { constructors, .. },
                case,
                of_type,
            } => {
                let c = constructors
                    .iter()
                    .find(|(c, _)| c == &case)
                    .unwrap_or_else(|| panic!("{case} case does not exists, constraint not met"));
                match (c.1.clone(), of_type) {
                    (Type::Struct { fields: fs_1, .. }, Type::Struct { fields: fs_2, .. }) => {
                        for (n1, t1) in fs_1 {
                            let t2 = fs_2
                                .iter()
                                .find(|(n2, _)| &n1 == n2)
                                .unwrap_or_else(|| {
                                    panic!(
                                        "Field {n1} does not exists in {case} case",
                                        n1 = n1,
                                        case = case
                                    )
                                })
                                .1
                                .clone();
                            self.constraints
                                .push_back(Constraint::Eq(t1.clone(), t2.clone()));
                        }
                    }
                    (Type::Tuple(ts_1), Type::Tuple(ts_2)) => {
                        for (t1, t2) in ts_1.iter().zip(ts_2.iter()) {
                            self.constraints
                                .push_back(Constraint::Eq(t1.clone(), t2.clone()));
                        }
                    }
                    (Type::Unit, Type::Unit) => {}
                    (t1, t2) => panic!("Constructor type did not match {t1:?} != {t2:?}"),
                }
            }

            Constraint::HasCase {
                ty: Type::Alias(id),
                case,
                of_type,
            } => {
                self.constraints.push_back(Constraint::HasCase {
                    ty: self.database.resolve_alias_to_type(id).unwrap(),
                    case,
                    of_type,
                });
            }

            Constraint::HasCase {
                ty: Type::Scheme { prefex, typ },
                case,
                of_type,
            } => {
                let new = Constraint::HasCase {
                    ty: Type::App {
                        args: prefex.iter().map(|_| self.fresh_type()).collect(),
                        typ: Box::new(Type::Scheme { prefex, typ }),
                    },
                    case,
                    of_type,
                };
                self.constraints.push_back(new);
            }

            Constraint::HasCase {
                ty: Type::App { typ, args },
                case,
                of_type,
            } => {
                self.constraints.push_back(Constraint::HasCase {
                    ty: instance(self.database, args, *typ),
                    case,
                    of_type,
                });
            }

            // If no case could be found to this point then look
            // through records
            Constraint::HasCase {
                ty,
                case,
                of_type: Type::Struct { fields, .. },
            } => {
                let t = self
                    .database
                    .get_type_by_unresolved_name(&self.current_namespace, &[], &case)
                    .unwrap_or_else(|| panic!("Could not find type {case}"))
                    .clone();
                for (fname, ftyp) in fields {
                    self.constraints.push_back(Constraint::HasProperty {
                        ty: ty.clone(),
                        property: fname.clone(),
                        of_type: ftyp.clone(),
                    });
                }
                self.constraints.push_back(Constraint::Eq(ty, t));
            }
            Constraint::HasMethod {
                receiver: Type::Alias(id),
                method,
                of_type,
                resolution_id,
            } => {
                let ty = self.database.resolve_alias_to_type(id).unwrap();
                self.constraints.push_back(Constraint::HasMethod {
                    receiver: ty,
                    method,
                    of_type,
                    resolution_id,
                });
            }
            // c @ Constraint::HasMethod { receiver, method, of_type }
            // if not_fully_resolved(receiver) => {
            //    self.delayed.push(c)
            // }
            // Constraint::HasMethod {
            //     receiver: Type::App { typ, args },
            //     method,
            //     of_type,
            //     resolution_id,
            // } => {
            //     self.constraints.push_back(Constraint::HasMethod {
            //         receiver: instance(self.database, args, *typ),
            //         method,
            //         of_type,
            //         resolution_id,
            //     });
            // }
            Constraint::HasMethod {
                receiver: Type::Scheme { prefex, typ },
                method,
                of_type,
                resolution_id,
            } => {
                let args = prefex.iter().map(|_| self.fresh_type()).collect();
                self.constraints.push_back(Constraint::HasMethod {
                    receiver: Type::App {
                        typ: Box::new(Type::Scheme { prefex, typ }),
                        args,
                    },
                    method,
                    of_type,
                    resolution_id,
                });
            }
            Constraint::HasMethod {
                receiver,
                method,
                of_type,
                resolution_id,
            } => match self.resolve_method(&receiver, &method) {
                Ok(ResolvedMethod::Static {
                    def_id,
                    ty: method_type,
                }) => {
                    println!("AAAAAAA");
                    println!(
                        "Resolved method to type {:?} expected {:?}",
                        method_type, of_type
                    );
                    self.call_resolutions
                        .insert(resolution_id, CallResolution::StaticMethod(def_id));
                    self.constraints
                        .push_back(Constraint::Eq(of_type, method_type));
                }
                Ok(ResolvedMethod::FromInstanceInScope {
                    method_id,
                    referenced_constraint,
                    ty: method_type,
                }) => {
                    self.call_resolutions.insert(
                        resolution_id,
                        CallResolution::FromInstanceInScope {
                            instance_index: referenced_constraint,
                            method_def: method_id,
                        },
                    );
                    self.constraints
                        .push_back(Constraint::Eq(of_type, method_type));
                }
                Err(e) => {
                    panic!("Could not resolve method {method} on {receiver:?}: {e:?}")
                }
            },
            Constraint::MethodOrGlobal {
                receiver,
                name,
                of_ty,
                resolution_id,
            } => match self.resolve_method(&receiver, &name) {
                Ok(ResolvedMethod::Static {
                    def_id,
                    ty: method_type,
                }) => {
                    self.call_resolutions
                        .insert(resolution_id, CallResolution::StaticMethod(def_id));
                    self.constraints
                        .push_back(Constraint::Eq(of_ty, method_type));
                }
                Ok(ResolvedMethod::FromInstanceInScope {
                    method_id,
                    referenced_constraint,
                    ty: method_type,
                }) => {
                    self.call_resolutions.insert(
                        resolution_id,
                        CallResolution::FromInstanceInScope {
                            instance_index: referenced_constraint,
                            method_def: method_id,
                        },
                    );
                    self.constraints
                        .push_back(Constraint::Eq(of_ty, method_type));
                }
                Err(_) => {
                    let def = self
                        .database
                        .get_definition_id_by_unresolved_name(&self.current_namespace, &[], &name)
                        .expect("Could not find function {name}");
                    self.call_resolutions
                        .insert(resolution_id, CallResolution::StaticFunction(def));
                    let ty = self.database.get_definitions_type(def).unwrap();
                    self.constraints
                        .push_back(Constraint::Eq(ty.clone(), of_ty));
                }
            },

            c => panic!("Cannot unify constraint {c:?}"),
        }
    }

    fn resolve_method(
        &self,
        receiver: &Type,
        method: &str,
    ) -> Result<ResolvedMethod, MethodResolutionError> {
        let generic_constraints = self.constraints_in_scope.clone();
        let types_in_scope = self.types.clone();
        let method_blocks_in_scope = self.visible_method_blocks.clone();
        let instances_in_scope = self.visible_instances.clone();
        let classes_in_scope = self.classes.clone();
        let mut method_resolver = MethodResolver::within_function(
            self.database,
            &self.prefex_scope,
            generic_constraints,
            instances_in_scope,
            method_blocks_in_scope,
            types_in_scope,
            classes_in_scope,
        );
        method_resolver.resolve_method(receiver, method)
    }

    fn fresh_type(&mut self) -> Type {
        Type::Fresh(self.session.id_provider().next())
    }

    fn replace_in_constraints(&mut self, fresh: usize, with: Type) {
        let mut replacer = FreshTypeReplacer {
            substitutions: {
                let mut m = HashMap::new();
                m.insert(fresh, with);
                m
            },
        };
        self.substitutions = self
            .substitutions
            .iter()
            .map(|(id, t)| (*id, replacer.fold_type(t.clone()).unwrap()))
            .collect();

        for c in self.constraints.iter_mut() {
            *c = match c {
                Constraint::Eq(t1, t2) => Constraint::Eq(
                    replacer.fold_type(t1.clone()).unwrap(),
                    replacer.fold_type(t2.clone()).unwrap(),
                ),
                Constraint::HasCase { ty, case, of_type } => Constraint::HasCase {
                    ty: replacer.fold_type(ty.clone()).unwrap(),
                    case: case.clone(),
                    of_type: replacer.fold_type(of_type.clone()).unwrap(),
                },
                Constraint::HasMethod {
                    receiver,
                    method,
                    of_type,
                    resolution_id,
                } => Constraint::HasMethod {
                    receiver: replacer.fold_type(receiver.clone()).unwrap(),
                    method: method.clone(),
                    of_type: replacer.fold_type(of_type.clone()).unwrap(),
                    resolution_id: *resolution_id,
                },
                Constraint::HasProperty {
                    ty,
                    property,
                    of_type,
                } => Constraint::HasProperty {
                    ty: replacer.fold_type(ty.clone()).unwrap(),
                    property: property.clone(),
                    of_type: replacer.fold_type(of_type.clone()).unwrap(),
                },
                Constraint::MethodOrGlobal {
                    receiver,
                    name,
                    of_ty,
                    resolution_id,
                } => Constraint::MethodOrGlobal {
                    receiver: replacer.fold_type(receiver.clone()).unwrap(),
                    name: name.clone(),
                    of_ty: replacer.fold_type(of_ty.clone()).unwrap(),
                    resolution_id: *resolution_id,
                },
            }
        }
    }
}

fn instance(database: &Database, args: Vec<Type>, scheme: Type) -> Type {
    match &scheme {
        Type::Scheme { prefex, .. } => {
            assert_eq!(
                prefex.len(),
                args.len(),
                "Expected type {} arguments, got {}",
                prefex.len(),
                args.len()
            );
        }
        Type::Alias(id) => {
            let resolved = database.resolve_alias_to_type(*id).unwrap();
            return instance(database, args, resolved);
        }
        t => panic!("Expected a scheme, got {:?}", t),
    };
    let new_t = args.into_iter().enumerate().fold(scheme, |acc, (i, t)| {
        let mut replacer = Instatiator {
            for_gen: i,
            instatiated: t,
            database,
            deruijn: DeBruijn(-1),
        };
        replacer.fold_type(acc).unwrap()
    });
    ShiftDebruijn.fold_type(new_t).unwrap()
}

struct ShiftDebruijn;
impl TypeFolder for ShiftDebruijn {
    type Error = ();
    fn fold_generic(&mut self, index: DeBruijn, pos: usize) -> Result<Type, ()> {
        Ok(Type::Generic(index - 1, pos))
    }
}

struct Instatiator<'tctx> {
    for_gen: usize,
    instatiated: Type,
    deruijn: DeBruijn,
    database: &'tctx Database,
}

impl<'ctx> TypeFolder for Instatiator<'ctx> {
    type Error = ();
    fn fold_scheme(&mut self, prefex: Vec<String>, typ: Type) -> Result<Type, ()> {
        self.deruijn += 1;
        let typ = self.fold_type(typ)?;
        self.deruijn -= 1;
        if self.deruijn == DeBruijn(-1) {
            return Ok(typ);
        }
        Ok(Type::Scheme {
            prefex,
            typ: Box::new(typ),
        })
    }

    fn fold_generic(&mut self, def: DeBruijn, id: usize) -> Result<Type, ()> {
        if def != self.deruijn {
            return Ok(Type::Generic(def, id));
        }
        if self.for_gen == id {
            match &self.instatiated {
                Type::Generic(d, i) => Ok(Type::Generic(self.deruijn.clone() + d.0, *i)),
                t => Ok(t.clone()),
            }
        } else {
            Ok(Type::Generic(def, id))
        }
    }

    fn fold_alias(&mut self, for_type: Id<TypeId>) -> Result<Type, ()> {
        let resolved = self.database.resolve_alias_to_type(for_type).unwrap();
        self.fold_type(resolved)
    }
}
