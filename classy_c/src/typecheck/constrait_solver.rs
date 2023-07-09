use std::collections::{HashMap, VecDeque};

use crate::typecheck::{
    constraints::Constraint,
    r#type::{Type, TypeFolder},
};

use super::{type_context::{DefId, TypCtx}, r#type::DeBruijn};

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
}

impl<'ctx> ConstraintSolver<'ctx> {
    pub fn new(tctx: &'ctx TypCtx) -> Self {
        Self {
            substitutions: Vec::new(),
            tctx,
        }
    }

    pub fn solve(&mut self, mut constraints: Vec<Constraint>) {
        constraints.reverse();
        let mut constraints: VecDeque<_> = constraints.into();
        while let Some(con) = constraints.pop_back() {
            self.solve_constraint(con, &mut constraints);
        }
    }

    fn solve_constraint(&mut self, cons: Constraint, constraints: &mut VecDeque<Constraint>) {
        match cons {
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
            Constraint::Eq(Type::Tuple(t_1), Type::Tuple(t_2)) => {
                for (t1, t2) in t_1.iter().zip(t_2.iter()) {
                    constraints.push_back(Constraint::Eq(t1.clone(), t2.clone()));
                }
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
            Constraint::Eq(app @ Type::App { .. }, t) => {
                constraints.push_back(Constraint::Eq(t, app));
            }
            Constraint::Eq(t, Type::App { typ: app_t, args }) => {
                let Type::Scheme { prefex, typ: scheme_t } = *app_t else {
                    panic!("Expected a scheme type got {app_t:?}");
                };
                assert!(prefex.len() == args.len());
                let instantiated = args.into_iter().enumerate().fold(*scheme_t, |acc, (i, t)| {
                    let mut replacer = Instatiator {
                        for_gen: i,
                        instatiated: t,
                        tctx: self.tctx,
                        deruijn: DeBruijn::zero(),
                    };
                    replacer.fold_type(acc)
                });
                constraints.push_back(Constraint::Eq(t, instantiated));
            }
            Constraint::Eq(Type::Array(t_1), Type::Array(t_2)) => {
                constraints.push_back(Constraint::Eq(*t_1, *t_2));
            }
            Constraint::Eq(Type::Generic(d1, i1), Type::Generic(d2, i2))
                if d1 == d2 && i1 == i2 => {}
            Constraint::HasField { t, field, of_type } => match t {
                Type::Struct { fields, .. } => {
                    let f = fields
                        .iter()
                        .find(|(f, _)| f == &field)
                        .expect("this field does not exists, constraint not met");
                    constraints.push_back(Constraint::Eq(f.1.clone(), of_type));
                }
                Type::Alias(id) => {
                    let Type::Struct { fields, .. } = self.tctx.resolve_alias(id) else {
                        panic!("Expected a struct type got {t:?}");
                    };
                    let f = fields
                        .iter()
                        .find(|(f, _)| f == &field)
                        .expect("this field does not exists, constraint not met");
                    constraints.push_back(Constraint::Eq(f.1.clone(), of_type));
                }
                _ => panic!("cannot infer struct type"),
            },
            c => panic!("Cannot unify constraint {c:?}"),
        }
    }
}

fn replace_in_constraints(id: usize, for_t: Type, cons: &mut VecDeque<Constraint>) {
    let mut replacer = FreshTypeReplacer {
        substitutions: {
            let mut m = HashMap::new();
            m.insert(id, for_t.clone());
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
        }
    }
}

/// TODO:
/// For now we are ignoring definition ids
/// And that will become a problem when we get to the methods
/// for generic types
struct Instatiator<'tctx> {
    for_gen: usize,
    instatiated: Type,
    deruijn: DeBruijn,
    tctx: &'tctx TypCtx,
}

impl<'ctx> TypeFolder for Instatiator<'ctx> {
    fn fold_scheme(&mut self, prefex: Vec<super::type_context::Name>, typ: Type) -> Type {
        self.deruijn += 1;
        let typ = self.fold_type(typ);
        self.deruijn -= 1;
        Type::Scheme { prefex, typ: Box::new(typ) }
    }

    fn fold_generic(&mut self, def: DeBruijn, id: usize) -> Type {
        if def != self.deruijn {
            return Type::Generic(def, id);
        }
        if self.for_gen == id {
            self.instatiated.clone()
        } else {
            Type::Generic(def, id)
        }
    }

    fn fold_alias(&mut self, for_type: usize) -> Type {
        let resolved = self.tctx.resolve_alias(for_type);
        self.fold_type(resolved)
    }
}
