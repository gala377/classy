#![feature(box_patterns)]

pub mod clauses;
pub mod database;
pub mod goal;
pub mod slg;
pub mod ty;

pub mod chain_map;
pub mod fold;
mod normalizer;
mod substitutor;

pub use database::Database;
pub use goal::{DomainGoal, Goal};
pub use ty::Ty;

#[cfg(test)]
mod tests {

    use crate::{
        clauses::Clause,
        database::{Instance, TypeClass, TypeImpl},
        goal::{DomainGoal, Goal},
        slg::{Forest, SlgSolver},
        ty::{Constraint, Ty},
    };

    use super::database::Database;
    #[test]
    fn match_simple_type() {
        let mut database = Database::new();
        let int = database.add_type_impl(super::database::TypeImpl {
            name: "Int".to_string(),
            type_params: vec![],
            constraints: vec![],
            fields: vec![],
        });
        let query = Goal::Domain(DomainGoal::TypeWellFormed { ty: Ty::Ref(int) });
        database.lower_to_clauses();
        let mut forest = Forest::new();
        let mut solver = SlgSolver::new(&database, &mut forest, query);
        let result = solver.solve();
        println!("{result:?}");
        assert!(result.is_some());
    }

    #[test]
    fn matches_generic_types() {
        let mut database = Database::new();
        let foo = database.add_type_impl(TypeImpl {
            name: "Foo".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![],
            fields: vec![],
        });
        let query = Goal::Forall(
            1,
            Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                ty: Ty::App(
                    Box::new(Ty::Ref(foo)),
                    vec![Ty::Generic {
                        scopes: 0,
                        index: 0,
                    }],
                ),
            })),
        );
        database.lower_to_clauses();
        let mut forest = Forest::new();
        let mut solver = SlgSolver::new(&database, &mut forest, query);
        let result = solver.solve();
        println!("{result:?}");
        assert!(result.is_some());
    }

    #[test]
    fn cannot_prove_forall_with_constraint() {
        let mut database = Database::new();
        let show = database.add_class(TypeClass {
            name: "Show".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![],
            members: vec![],
        });
        let foo = database.add_type_impl(TypeImpl {
            name: "Foo".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![Constraint::Class(
                show,
                vec![Ty::Generic {
                    scopes: 0,
                    index: 0,
                }],
            )],
            fields: vec![],
        });
        let query = Goal::Forall(
            1,
            Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                ty: Ty::App(
                    Box::new(Ty::Ref(foo)),
                    vec![Ty::Generic {
                        scopes: 0,
                        index: 0,
                    }],
                ),
            })),
        );
        database.lower_to_clauses();
        let mut forest = Forest::new();
        let mut solver = SlgSolver::new(&database, &mut forest, query);
        let result = solver.solve();
        println!("{result:?}");
        assert!(result.is_none());
    }

    #[test]
    fn can_prove_forall_with_constraint_within_a_domain() {
        let mut database = Database::new();
        let show = database.add_class(TypeClass {
            name: "Show".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![],
            members: vec![],
        });
        let foo = database.add_type_impl(TypeImpl {
            name: "Foo".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![Constraint::Class(
                show,
                vec![Ty::Generic {
                    scopes: 0,
                    index: 0,
                }],
            )],
            fields: vec![],
        });
        let goal = Goal::Forall(
            1,
            Box::new(Goal::Implies(
                vec![Clause::Fact(DomainGoal::InstanceExistsAndWellFormed {
                    head: show,
                    args: vec![Ty::Generic {
                        scopes: 0,
                        index: 0,
                    }],
                })],
                Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::App(
                        Box::new(Ty::Ref(foo)),
                        vec![Ty::Generic {
                            scopes: 0,
                            index: 0,
                        }],
                    ),
                })),
            )),
        );
        database.lower_to_clauses();
        let mut forest = Forest::new();
        let mut solver = SlgSolver::new(&database, &mut forest, goal);
        let result = solver.solve();
        println!("{result:?}");
        assert!(result.is_some());
    }

    #[test]
    fn cannot_prove_forall_domain_if_the_domain_does_not_match() {
        let mut database = Database::new();
        let show = database.add_class(TypeClass {
            name: "Show".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![],
            members: vec![],
        });
        let debug = database.add_class(TypeClass {
            name: "Debug".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![],
            members: vec![],
        });
        let foo = database.add_type_impl(TypeImpl {
            name: "Foo".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![Constraint::Class(
                show,
                vec![Ty::Generic {
                    scopes: 0,
                    index: 0,
                }],
            )],
            fields: vec![],
        });
        let goal = Goal::Forall(
            1,
            Box::new(Goal::Implies(
                vec![Clause::Fact(DomainGoal::InstanceExistsAndWellFormed {
                    head: debug,
                    args: vec![Ty::Generic {
                        scopes: 0,
                        index: 0,
                    }],
                })],
                Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::App(
                        Box::new(Ty::Ref(foo)),
                        vec![Ty::Generic {
                            scopes: 0,
                            index: 0,
                        }],
                    ),
                })),
            )),
        );
        database.lower_to_clauses();
        let mut forest = Forest::new();
        let mut solver = SlgSolver::new(&database, &mut forest, goal);
        let result = solver.solve();
        println!("{result:?}");
        assert!(result.is_none());
    }

    #[test]
    fn finds_all_answers() {
        let mut database = Database::new();
        let show = database.add_class(TypeClass {
            name: "Show".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![],
            members: vec![],
        });
        let int = database.add_type_impl(TypeImpl {
            name: "Int".to_string(),
            type_params: vec![],
            constraints: vec![],
            fields: vec![],
        });
        let string = database.add_type_impl(TypeImpl {
            name: "String".to_string(),
            type_params: vec![],
            constraints: vec![],
            fields: vec![],
        });
        let foo = database.add_type_impl(TypeImpl {
            name: "Foo".to_string(),
            type_params: vec!["a".into()],
            constraints: vec![Constraint::Class(
                show,
                vec![Ty::Generic {
                    scopes: 0,
                    index: 0,
                }],
            )],
            fields: vec![],
        });
        database.add_instance(Instance {
            type_class: show,
            args: vec![Ty::Ref(int)],
            type_params: vec![],
            constraints: vec![],
        });
        database.add_instance(Instance {
            type_class: show,
            args: vec![Ty::Ref(string)],
            type_params: vec![],
            constraints: vec![],
        });
        database.lower_to_clauses();
        let query = Goal::Exists(
            1,
            Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                ty: Ty::App(
                    Box::new(Ty::Ref(foo)),
                    vec![Ty::Generic {
                        scopes: 0,
                        index: 0,
                    }],
                ),
            })),
        );
        let mut forest = Forest::new();
        let solver = SlgSolver::new(&database, &mut forest, query);
        let results = solver.collect::<Vec<_>>();
        println!("{results:?}");
        assert_eq!(results.len(), 2);
        assert!(results[0].subst.mapping[&0] == Ty::Ref(int));
        assert!(results[1].subst.mapping[&0] == Ty::Ref(string));
    }
}
