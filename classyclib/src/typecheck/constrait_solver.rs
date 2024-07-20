use core::panic;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use classy_blackboard::database::{AnswerOrigin, GenericRef};
use classy_blackboard::slg::SlgSolver;
use classy_blackboard::{goal, slg::Answer};

use crate::{
    session::SharedIdProvider,
    typecheck::{
        constraints::Constraint,
        inference::ty_to_blackboard_type,
        types::{Type, TypeFolder},
    },
};

use super::type_context::DefId;
use super::{type_context::TypCtx, types::DeBruijn};

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

// TODO: Problem we have here is that function can have constraints within their
// declaration. we create constraint solver per mutually recursive functions so
// I think it is impossible to say which constraints were present for the given
// constraint. we could somehow make it so that each relevant constraint (i
// think only HasMethod is relevant) has a handle to a collection of constraints
// that were present at declartaion site. This way we know which constraints
// should be present when creating a goal to solve by the blackboard.
pub(super) struct ConstraintSolver<'ctx, 'solver_db> {
    pub substitutions: Vec<(usize, Type)>,
    pub tctx: &'ctx TypCtx,
    pub id_provider: SharedIdProvider,
    pub blackboard_database: &'solver_db classy_blackboard::database::Database,
    forest: classy_blackboard::slg::Forest,
    definitions: Rc<HashMap<GenericRef, DefId>>,
    /// Resolved method calls, the key is a method call id and the value is the
    /// definition id of the method
    pub resolved_nodes: HashMap<usize, DefId>,
    // TODO: This is how the constraints should look like i think
    // We are going to index into the outer vector to get the clauses
    // to inject into the goal for the blackboard to solve.
    // enviromental_clauses: Vec<Vec<Constraint>>,
}

impl<'ctx, 'solver_db> ConstraintSolver<'ctx, 'solver_db> {
    pub fn new(
        tctx: &'ctx TypCtx,
        id_provider: SharedIdProvider,
        blackboard_database: &'solver_db classy_blackboard::database::Database,
        definitions: Rc<HashMap<GenericRef, DefId>>,
    ) -> Self {
        Self {
            substitutions: Vec::new(),
            tctx,
            id_provider,
            blackboard_database,
            forest: classy_blackboard::slg::Forest::new(),
            definitions,
            resolved_nodes: HashMap::new(),
        }
    }

    pub fn solve(&mut self, mut constraints: Vec<Constraint>) {
        constraints.reverse();
        let mut constraints: VecDeque<_> = constraints.into();
        while let Some(con) = constraints.pop_back() {
            println!("\n\n\n\n\n\n\n");

            println!("=> {con:?}");
            for c in constraints.iter().rev() {
                println!("-> {c:?}");
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
            Constraint::Eq(Type::App { typ, args }, app2 @ Type::App { .. }) => {
                constraints.push_back(Constraint::Eq(instance(self.tctx, args, *typ), app2));
            }
            Constraint::Eq(app @ Type::App { .. }, t) => {
                constraints.push_back(Constraint::Eq(t, app));
            }
            Constraint::Eq(Type::Fresh(id), other @ Type::App { .. }) => {
                self.substitutions.push((id, other.clone()));
                replace_in_constraints(id, other, constraints)
            }
            Constraint::Eq(t, Type::App { typ: app_t, args }) => {
                constraints.push_back(Constraint::Eq(t, instance(self.tctx, args, *app_t)));
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
            Constraint::Eq(Type::Generic(_, _), t) if t.is_ref().unwrap() => {}
            Constraint::Eq(t, Type::Generic(_, _)) if t.is_ref().unwrap() => {}
            Constraint::HasField {
                t: Type::Struct { fields, .. },
                field,
                of_type,
            } => {
                let f = fields
                    .iter()
                    .find(|(f, _)| f == &field)
                    .unwrap_or_else(|| panic!("field {field} does not exists on this struct"));
                constraints.push_back(Constraint::Eq(f.1.clone(), of_type));
            }
            Constraint::HasField {
                t: Type::Alias(id),
                field,
                of_type,
            } => {
                constraints.push_back(Constraint::HasField {
                    t: self.tctx.resolve_alias(id),
                    field,
                    of_type,
                });
            }
            Constraint::HasField {
                t: Type::App { typ, args },
                field,
                of_type,
            } => {
                constraints.push_back(Constraint::HasField {
                    t: instance(self.tctx, args, *typ),
                    field,
                    of_type,
                });
            }
            Constraint::HasField {
                t: Type::Scheme { prefex, typ },
                field,
                of_type,
            } => {
                let args = prefex.iter().map(|_| self.fresh_type()).collect();
                constraints.push_back(Constraint::HasField {
                    t: Type::App {
                        typ: Box::new(Type::Scheme { prefex, typ }),
                        args,
                    },
                    field,
                    of_type,
                });
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
                            constraints.push_back(Constraint::Eq(t1.clone(), t2.clone()));
                        }
                    }
                    (Type::Tuple(ts_1), Type::Tuple(ts_2)) => {
                        for (t1, t2) in ts_1.iter().zip(ts_2.iter()) {
                            constraints.push_back(Constraint::Eq(t1.clone(), t2.clone()));
                        }
                    }
                    (Type::Unit, Type::Unit) => {}
                    (t1, t2) => panic!("Constructor type did not match {t1:?} != {t2:?}"),
                }
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
                constraints.push_back(Constraint::HasCase {
                    t: instance(self.tctx, args, *typ),
                    case,
                    of_type,
                });
            }
            // If no case could be found to this point then look
            // through records
            Constraint::HasCase {
                t: Type::Fresh(id),
                case,
                of_type: Type::Struct { fields, .. },
            } => {
                let t = self
                    .tctx
                    .get_type(&case)
                    .unwrap_or_else(|| panic!("Could not find type {case}"));
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
            Constraint::HasMethod {
                nodeid,
                receiver,
                method,
                args,
                ret,
            } => {
                println!("HAS METHOD ON {receiver:?}", receiver = receiver);
                let (receiver_t, existentials) = ty_to_blackboard_type(
                    self.tctx,
                    &receiver,
                    self.blackboard_database,
                    &mut Vec::new(),
                );
                let goal = goal::Goal::Exists(
                    existentials,
                    Box::new(goal::Goal::Domain(goal::DomainGoal::FindMethod {
                        name: method.clone(),
                        on_type: receiver_t.clone(),
                    })),
                );
                // TODO: If the type is scheme we should do forall? Or instance it with a fresh
                // variable
                println!("goal: {:?}", goal);
                let solver = SlgSolver::new(self.blackboard_database, &mut self.forest, goal);
                let result = solver.take(10).collect::<Vec<_>>();
                if result.is_empty() {
                    panic!("Could not find method {method} on {receiver:?}");
                }
                if result.len() > 1 {
                    println!("Ambiguous method {method} on {receiver:?}");
                    println!("Possible candidates");
                    for r in result {
                        println!("  {r:?}");
                    }
                    panic!("Compilation error");
                }
                // TODO: Can we do anything with substitutions?
                let Answer {
                    origin: AnswerOrigin::FromRef(origin),
                    ..
                } = result[0].clone()
                else {
                    panic!("Cannot get origin from answer ");
                };
                let def_id = self.definitions.get(&origin).unwrap();
                let method_type = {
                    println!("Got definition {def_id:?}");
                    println!("All definitions");
                    for (def_id, path) in self.tctx.method_blocks_by_def_id.iter() {
                        println!("  {def_id:?} -> {path:?}");
                    }
                    let path = self.tctx.method_blocks_by_def_id[def_id];
                    println!("All sets");
                    for (t_id, set) in &self.tctx.methods {
                        println!("  {t_id:?} -> {set:?}");
                    }
                    let block = &self.tctx.methods[&path.0][path.1];
                    block.methods[&method]
                };
                println!("Found candidate: {:?}", result[0]);
                let resolved = self.tctx.definitions[&method_type].clone();
                println!("Type of the candidate is {:?}", resolved);
                self.resolved_nodes.insert(nodeid, *def_id);
                constraints.push_back(Constraint::Eq(
                    resolved,
                    Type::Function {
                        args,
                        ret: Box::new(ret),
                    },
                ));
            }

            c => panic!("Cannot unify constraint {c:?}"),
        }
    }

    fn fresh_type(&mut self) -> Type {
        Type::Fresh(self.id_provider.next())
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
                replacer.fold_type(t1.clone()).unwrap(),
                replacer.fold_type(t2.clone()).unwrap(),
            ),
            Constraint::HasField { t, field, of_type } => Constraint::HasField {
                t: replacer.fold_type(t.clone()).unwrap(),
                field: field.clone(),
                of_type: replacer.fold_type(of_type.clone()).unwrap(),
            },
            Constraint::HasCase { t, case, of_type } => Constraint::HasCase {
                t: replacer.fold_type(t.clone()).unwrap(),
                case: case.clone(),
                of_type: replacer.fold_type(of_type.clone()).unwrap(),
            },
            Constraint::HasMethod {
                nodeid,
                receiver,
                method,
                args,
                ret,
            } => Constraint::HasMethod {
                nodeid: *nodeid,
                receiver: replacer.fold_type(receiver.clone()).unwrap(),
                method: method.clone(),
                args: args
                    .iter()
                    .map(|t| replacer.fold_type(t.clone()))
                    .collect::<Result<_, _>>()
                    .unwrap(),
                ret: replacer.fold_type(ret.clone()).unwrap(),
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
    type Error = ();
    fn fold_scheme(
        &mut self,
        prefex: Vec<super::type_context::Name>,
        typ: Type,
    ) -> Result<Type, ()> {
        println!("Instatiating scheme {prefex:?} {typ:?}");
        self.deruijn += 1;
        let typ = self.fold_type(typ)?;
        self.deruijn -= 1;
        if self.deruijn == DeBruijn(-1) {
            println!("Instatiated scheme to {typ:?}");
            return Ok(typ);
        }
        println!("Not a top level so returning scheme of {typ:?}");
        Ok(Type::Scheme {
            prefex,
            typ: Box::new(typ),
        })
    }

    fn fold_generic(&mut self, def: DeBruijn, id: usize) -> Result<Type, ()> {
        println!(
            "Instatiating generic {def:?} {id} current ({:?}, {})",
            self.deruijn, self.for_gen
        );
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

    fn fold_alias(&mut self, for_type: usize) -> Result<Type, ()> {
        println!("Resolving alias {for_type}");
        let resolved = self.tctx.resolve_alias(for_type);
        println!("Resolved alias {resolved:?}");
        self.fold_type(resolved)
    }
}

/// Instance scheme `scheme_t` with `args`. It is important to note
/// that `scheme_t` must be a scheme type or an alias to a scheme.
pub fn instance(tctx: &TypCtx, args: Vec<Type>, scheme_t: Type) -> Type {
    assert!(
        args.iter().map(|t| t.is_ref().unwrap_or(true)).all(|b| b),
        "Cannot instantiate non ref types"
    );
    match &scheme_t {
        // Special case when we just need to unpack a scheme
        Type::Scheme { prefex, typ } if args.is_empty() && prefex.is_empty() => {
            return ShiftDebruijn.fold_type(*typ.clone()).unwrap();
        }
        Type::Scheme { prefex, .. } => {
            assert_eq!(
                prefex.len(),
                args.len(),
                "cannot saturate a type application"
            );
        }
        Type::Alias(id) => {
            let resolved = tctx.resolve_alias(*id);
            return instance(tctx, args, resolved);
        }
        t => {
            assert!(args.is_empty(), "type is empty, cannot instantiate: {t:?}");
        }
    }
    let new_t = args.into_iter().enumerate().fold(scheme_t, |acc, (i, t)| {
        let mut replacer = Instatiator {
            for_gen: i,
            instatiated: t,
            tctx,
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
