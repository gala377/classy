use core::panic;
use std::collections::{HashMap, VecDeque};

use crate::typecheck::{
    constraints::Constraint,
    r#type::{Type, TypeFolder},
};

use super::{r#type::DeBruijn, type_context::TypCtx};

pub(super) struct FreshTypeReplacer {
    pub substitutions: HashMap<usize, Type>,
}

impl TypeFolder for FreshTypeReplacer {
    fn fold_fresh(&mut self, id: usize) -> Type {
        match self.substitutions.get(&id) {
            Some(t) => self.fold_type(t.clone()),
            None => Type::Fresh(id),
        }
    }
}

pub(super) struct ConstraintSolver<'ctx> {
    pub substitutions: Vec<(usize, Type)>,
    pub tctx: &'ctx TypCtx,
    pub next_var: usize,
}

impl<'ctx> ConstraintSolver<'ctx> {
    pub fn new(next_var: usize, tctx: &'ctx TypCtx) -> Self {
        Self {
            substitutions: Vec::new(),
            tctx,
            next_var,
        }
    }

    pub fn solve(&mut self, mut constraints: Vec<Constraint>) {
        constraints.reverse();
        let mut constraints: VecDeque<_> = constraints.into();
        while let Some(con) = constraints.pop_back() {
            println!("\n\nSOLVING CONSTRAINTS\n");
            println!(">> {con:#?}");
            for con in constraints.iter() {
                println!("-> {con:#?}");
            }

            self.solve_constraint(con, &mut constraints);
        }
    }

    fn solve_constraint(&mut self, cons: Constraint, constraints: &mut VecDeque<Constraint>) {
        match cons.clone() {
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
                let resolved = self.tctx.resolve_alias(id);
                constraints.push_back(Constraint::Eq(resolved, t));
            }
            Constraint::Eq(t, Type::Alias(id)) => {
                let resolved = self.tctx.resolve_alias(id);
                constraints.push_back(Constraint::Eq(t, resolved));
            }
            Constraint::Eq(Type::Fresh(id1), Type::Fresh(id2)) if id1 == id2 => {}
            Constraint::Eq(Type::Struct { def: def_1, .. }, Type::Struct { def: def_2, .. })
                if def_1 == def_2 => {}

            Constraint::Eq(Type::ADT { def: def_1, .. }, Type::ADT { def: def_2, .. })
                if def_1 == def_2 => {}
            Constraint::Eq(Type::Tuple(t_1), Type::Tuple(t_2)) => {
                for (t1, t2) in t_1.iter().zip(t_2.iter()) {
                    constraints.push_back(Constraint::Eq(t1.clone(), t2.clone()));
                }
            }
            Constraint::Eq(app @ Type::App { .. }, t) => {
                constraints.push_back(Constraint::Eq(t, app));
            }
            Constraint::Eq(t, Type::App { typ: app_t, args }) => {
                for a in &args {
                    if let Some(false) = a.is_ref() {
                        panic!("Cannot apply generic for non ref type {a:?}");
                    }
                }
                let app_t = match app_t.as_ref() {
                    Type::Alias(id) => self.tctx.resolve_alias(*id),
                    t => t.clone(),
                };
                let Type::Scheme { prefex, typ: scheme_t } = app_t else {
                    panic!("Expected a scheme type got {app_t:?}");
                };
                assert!(prefex.len() == args.len());
                let instantiated = instance(self.tctx, args, *scheme_t);
                constraints.push_back(Constraint::Eq(t, instantiated));
            }
            Constraint::Eq(Type::Fresh(id1), other) => {
                self.substitutions.push((id1, other.clone()));
                replace_in_constraints(id1, other, constraints)
            }
            Constraint::Eq(other, Type::Fresh(id1)) => {
                self.substitutions.push((id1, other.clone()));
                replace_in_constraints(id1, other, constraints)
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
                constraints.push_back(Constraint::Eq(*ret_1, *ret_2));
                for (a1, a2) in args_1.iter().zip(args_2.iter()) {
                    constraints.push_back(Constraint::Eq(a1.clone(), a2.clone()));
                }
            }
            Constraint::Eq(
                f @ (Type::Function { .. } | Type::ADT { .. } | Type::Struct { .. }),
                s @ Type::Scheme { .. },
            ) => {
                constraints.push_back(Constraint::Eq(s, f));
            }
            Constraint::Eq(
                Type::Scheme { prefex, typ },
                func_1 @ (Type::Function { .. } | Type::ADT { .. } | Type::Struct { .. }),
            ) => {
                let args = prefex.iter().map(|_| self.fresh_type()).collect();
                constraints.push_back(Constraint::Eq(
                    func_1,
                    Type::App {
                        typ: Box::new(Type::Scheme { prefex, typ }),
                        args,
                    },
                ));
            }

            Constraint::Eq(Type::Array(t_1), Type::Array(t_2)) => {
                constraints.push_back(Constraint::Eq(*t_1, *t_2));
            }
            Constraint::Eq(Type::Generic(d1, i1), Type::Generic(d2, i2))
                if d1 == d2 && i1 == i2 => {}
            Constraint::HasField {
                t: Type::Struct { fields, .. },
                field,
                of_type,
            } => {
                let f = fields
                    .iter()
                    .find(|(f, _)| f == &field)
                    .expect("this field does not exists, constraint not met");
                constraints.push_back(Constraint::Eq(f.1.clone(), of_type));
            }
            Constraint::HasField {
                t: Type::Alias(id),
                field,
                of_type,
            } => {
                let Type::Struct { fields, .. } = self.tctx.resolve_alias(id) else {
                        panic!("Expected a struct type got");
                    };
                let f = fields
                    .iter()
                    .find(|(f, _)| f == &field)
                    .expect("this field does not exists, constraint not met");
                constraints.push_back(Constraint::Eq(f.1.clone(), of_type));
            }
            Constraint::HasCase {
                t: Type::Struct { def, fields },
                case,
                of_type,
            } => {
                let name = self.tctx.name_by_def_id(def);
                if case != name {
                    panic!("Cannot unify case {case} with {name}");
                }
                let Type::Struct { fields: pat_f, .. } = of_type else {
                        panic!("Expected a struct type in pattern got {of_type:?}");
                    };
                for (fname, ftyp) in pat_f {
                    constraints.push_back(Constraint::HasField {
                        t: Type::Struct {
                            def,
                            fields: fields.clone(),
                        },
                        field: fname.clone(),
                        of_type: ftyp.clone(),
                    });
                }
            }
            Constraint::HasCase {
                t: Type::ADT { constructors, .. },
                case,
                of_type,
            } => {
                let c = constructors
                    .iter()
                    .find(|(c, _)| c == &case)
                    .expect(&format!("{case} case does not exists, constraint not met"));
                // TODO: the of_type is a dummy type so the struct does
                // not have an existing id
                constraints.push_back(Constraint::Eq(c.1.clone(), of_type));
            }

            Constraint::HasCase {
                t: Type::Alias(id),
                case,
                of_type,
            } => {
                constraints.push_back(Constraint::HasCase {
                    t: self.tctx.resolve_alias(id),
                    case,
                    of_type,
                });
            }

            Constraint::HasCase {
                t: Type::Scheme { prefex, typ },
                case,
                of_type,
            } => {
                constraints.push_back(Constraint::HasCase {
                    t: Type::App {
                        args: prefex.iter().map(|_| self.fresh_type()).collect(),
                        typ: Box::new(Type::Scheme { prefex, typ }),
                    },
                    case,
                    of_type,
                });
            }

            Constraint::HasCase {
                t: Type::App { typ, args },
                case,
                of_type,
            } => {
                for a in &args {
                    if let Some(false) = a.is_ref() {
                        panic!("Cannot apply generic for non ref type {a:?}");
                    }
                }
                let app_t = match typ.as_ref() {
                    Type::Alias(id) => self.tctx.resolve_alias(*id),
                    t => t.clone(),
                };
                let Type::Scheme { prefex, typ: scheme_t } = app_t else {
                        panic!("Expected a scheme type got {app_t:?}");
                    };
                assert!(prefex.len() == args.len());
                let instantiated = instance(self.tctx, args, *scheme_t);
                constraints.push_back(Constraint::HasCase {
                    t: instantiated,
                    case,
                    of_type,
                });
            }
            // If not case could be found to this point then look
            // through records
            Constraint::HasCase {
                t: Type::Fresh(id),
                case,
                of_type: Type::Struct { fields, .. },
            } => {
                let t = self
                    .tctx
                    .get_type(&case)
                    .expect(&format!("Could not find type {case}"));
                for (fname, ftyp) in fields {
                    constraints.push_back(Constraint::HasField {
                        t: Type::Fresh(id),
                        field: fname.clone(),
                        of_type: ftyp.clone(),
                    });
                }
                constraints.push_back(Constraint::Eq(Type::Fresh(id), t));
            }
            Constraint::HasCase { t, .. } => {
                println!("\n\nERRRRRORRRRRRR\n\n");
                println!("cannot infer ADT type {t:?} full constraint {cons:?}");
                for c in constraints.iter().rev() {
                    println!("-> {c:?}");
                }
                panic!()
            }
            Constraint::Eq(Type::Generic(_, _), t) if t.is_ref().unwrap() => {}
            Constraint::Eq(t, Type::Generic(_, _)) if t.is_ref().unwrap() => {}
            c => panic!("Cannot unify constraint {c:?}"),
        }
    }

    fn fresh_type(&mut self) -> Type {
        let var = self.next_var;
        self.next_var += 1;
        Type::Fresh(var)
    }
}

fn replace_in_constraints(id: usize, for_t: Type, cons: &mut VecDeque<Constraint>) {
    let mut replacer = FreshTypeReplacer {
        substitutions: {
            let mut m = HashMap::new();
            m.insert(id, for_t);
            m
        },
    };
    for c in cons {
        *c = match c {
            Constraint::Eq(t1, t2) => Constraint::Eq(
                replacer.fold_type(t1.clone()),
                replacer.fold_type(t2.clone()),
            ),
            Constraint::HasField { t, field, of_type } => Constraint::HasField {
                t: replacer.fold_type(t.clone()),
                field: field.clone(),
                of_type: replacer.fold_type(of_type.clone()),
            },
            Constraint::HasCase { t, case, of_type } => Constraint::HasCase {
                t: replacer.fold_type(t.clone()),
                case: case.clone(),
                of_type: replacer.fold_type(of_type.clone()),
            },
        }
    }
}
struct Instatiator<'tctx> {
    for_gen: usize,
    instatiated: Type,
    deruijn: DeBruijn,
    tctx: &'tctx TypCtx,
}

impl<'ctx> TypeFolder for Instatiator<'ctx> {
    fn fold_scheme(&mut self, prefex: Vec<super::type_context::Name>, typ: Type) -> Type {
        println!("Instatiating scheme {prefex:?} {typ:?}");
        self.deruijn += 1;
        let typ = self.fold_type(typ);
        self.deruijn -= 1;
        if self.deruijn == DeBruijn(-1) {
            println!("Instatiated scheme to {typ:?}");
            return typ;
        }
        println!("Not a top level so returning scheme of {typ:?}");
        Type::Scheme {
            prefex,
            typ: Box::new(typ),
        }
    }

    fn fold_generic(&mut self, def: DeBruijn, id: usize) -> Type {
        println!(
            "Instatiating generic {def:?} {id} current ({:?}, {})",
            self.deruijn, self.for_gen
        );
        if def != self.deruijn {
            return Type::Generic(def, id);
        }
        if self.for_gen == id {
            match &self.instatiated {
                Type::Generic(d, i) => Type::Generic(self.deruijn.clone() + d.0, *i),
                t => t.clone(),
            }
        } else {
            Type::Generic(def, id)
        }
    }

    fn fold_alias(&mut self, for_type: usize) -> Type {
        println!("Resolving alias {for_type}");
        let resolved = self.tctx.resolve_alias(for_type);
        println!("Resolved alias {resolved:?}");
        self.fold_type(resolved)
    }
}

pub fn instance(tctx: &TypCtx, args: Vec<Type>, scheme_t: Type) -> Type {
    instance_with_starting_index(DeBruijn::zero(), tctx, args, scheme_t)
}

pub fn instance_with_starting_index(
    index: DeBruijn,
    tctx: &TypCtx,
    args: Vec<Type>,
    scheme_t: Type,
) -> Type {
    args.into_iter().enumerate().fold(scheme_t, |acc, (i, t)| {
        let mut replacer = Instatiator {
            for_gen: i,
            instatiated: t,
            tctx,
            deruijn: index.clone(),
        };
        replacer.fold_type(acc)
    })
}
