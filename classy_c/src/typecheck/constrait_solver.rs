use super::{
    inference::Constraint,
    r#type::{Type, TypeFolder},
};

struct TypeReplacer {
    fresh_type_id: usize,
    for_type: Type,
}

impl TypeFolder for TypeReplacer {
    fn fold_fresh(&mut self, id: usize) -> Type {
        if id == self.fresh_type_id {
            self.for_type.clone()
        } else {
            Type::Fresh(id)
        }
    }
}

pub(super) struct ConstraintSolver {
    substitutions: Vec<(usize, Type)>,
}

impl ConstraintSolver {
    pub fn new() -> Self {
        Self {
            substitutions: Vec::new(),
        }
    }

    pub fn solve(&mut self, mut constraints: Vec<Constraint>) {
        constraints.reverse();
        while let Some(con) = constraints.pop() {
            self.solve_constraint(con, &mut constraints);
        }
        println!("SUBSTUITIONS");
        for (id, typ) in self.substitutions.iter() {
            println!("{} -> {:?}", id, typ);
        }
        println!("CONSTRAINS LEFT {}", constraints.len());
        for c in constraints {
            println!("{:?}", c);
        }
    }

    fn solve_constraint(&mut self, cons: Constraint, constraints: &mut Vec<Constraint>) {
        match cons {
            Constraint::Eq(Type::Bool, Type::Bool)
            | Constraint::Eq(Type::Int, Type::Int)
            | Constraint::Eq(Type::UInt, Type::UInt)
            | Constraint::Eq(Type::Float, Type::Float)
            | Constraint::Eq(Type::String, Type::String)
            | Constraint::Eq(Type::Unit, Type::Unit)
            | Constraint::Eq(Type::Divergent, Type::Divergent) => {}
            Constraint::Eq(Type::Fresh(id1), Type::Fresh(id2)) if id1 == id2 => {}
            Constraint::Eq(Type::Struct { def: def_1, .. }, Type::Struct { def: def_2, .. }) if def_1 == def_2 => {}
            Constraint::Eq(Type::Alias(id1),Type::Alias(id2)) if id1 == id2 => {}
            Constraint::Eq(Type::Tuple(t_1), Type::Tuple(t_2)) => {
                for (t1, t2) in t_1.iter().zip(t_2.iter()) {
                    constraints.push(Constraint::Eq(t1.clone(), t2.clone()));
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
            Constraint::Eq(Type::Function { args: args_1, ret: ret_1 }, Type::Function { args: args_2, ret: ret_2 }) => {
                constraints.push(Constraint::Eq(*ret_1, *ret_2));
                for (a1, a2) in args_1.iter().zip(args_2.iter()) {
                    constraints.push(Constraint::Eq(a1.clone(), a2.clone()));
                }
            }
            Constraint::HasField { t, field, of_type } => {
                match t {
                    Type::Struct {  fields, .. } => {
                        let f = fields.iter().find(|(f, _)| f == &field).expect("this field does not exists, constraint not met");
                        constraints.push(Constraint::Eq(f.1.clone(), of_type));
                    }
                    _ => panic!("Expected a struct type got {t:?}")
                }
            },
            c => panic!("Cannot unify constraint {c:?}"),
        }
    }
}

fn replace_in_constraints(id: usize, for_t: Type, cons: &mut [Constraint]) {
    let mut replacer = TypeReplacer {
        fresh_type_id: id,
        for_type: for_t.clone(),
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
