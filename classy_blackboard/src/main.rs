use classy_blackboard::{
    clauses::Clause,
    database::{Database, Instance, TypeClass, TypeImpl},
    goal::{DomainGoal, Goal},
    slg::{Forest, SlgSolver, Substitution},
    ty::{Constraint, Ty},
};

pub fn main() {
    tracing_subscriber::fmt().pretty().init();
    let mut database = Database::new();
    let foo = database.add_type_impl(TypeImpl {
        name: "Foo".to_string(),
        type_params: vec!["a".into()],
        constraints: vec![],
        fields: vec![],
    });
    let int = database.add_type_impl(TypeImpl {
        name: "Int".to_string(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    let show = database.add_class(TypeClass {
        name: "Show".to_string(),
        type_params: vec!["a".into()],
        constraints: vec![],
        members: vec![],
    });
    database.add_instance(Instance {
        type_class: show,
        args: vec![Ty::Ref(int)],
        type_params: vec![],
        constraints: vec![],
    });
    database.add_instance(Instance {
        type_class: show,
        args: vec![Ty::App(
            foo,
            vec![Ty::Generic {
                scopes: 0,
                index: 0,
            }],
        )],
        type_params: vec!["a".into()],
        constraints: vec![Constraint::Class(
            show,
            vec![Ty::Generic {
                scopes: 0,
                index: 0,
            }],
        )],
    });
    database.lower_to_clauses();
    let mut forest = Forest::new();
    // let goal = Goal::Forall(
    //     1,
    //     Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
    //         ty: Ty::App(
    //             foo,
    //             vec![Ty::Generic {
    //                 scopes: 0,
    //                 index: 0,
    //             }],
    //         ),
    //     })),
    // );
    // let result = solver.solve(goal);
    // print_result(result);

    // let goal = Goal::Domain(DomainGoal::TypeWellFormed {
    //     ty: Ty::App(foo, vec![Ty::Ref(int)]),
    // });
    // let result = solver.solve(goal);
    // print_result(result);

    // let goal = Goal::Exists(
    //     1,
    //     Box::new(Goal::Domain(DomainGoal::InstanceExistsAndWellFormed {
    //         head: show,
    //         args: vec![Ty::App(
    //             foo,
    //             vec![Ty::Generic {
    //                 scopes: 0,
    //                 index: 0,
    //             }],
    //         )],
    //     })),
    // );
    // let result = solver.solve(goal.clone());
    // print_result(result);

    // let result = solver.solve(goal.clone());
    // print_result(result);

    // let result = solver.solve(goal.clone());
    // print_result(result);

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
            Box::new(Goal::Domain(DomainGoal::InstanceExistsAndWellFormed {
                head: show,
                args: vec![Ty::App(
                    foo,
                    vec![Ty::Generic {
                        scopes: 0,
                        index: 0,
                    }],
                )],
            })),
        )),
    );

    let mut solver = SlgSolver::new(&database, &mut forest, goal.clone());
    while let Some(result) = solver.next() {
        print_result(result);
    }
    println!("no more solutions");

    let mut solver = SlgSolver::new(&database, &mut forest, goal.clone());
    while let Some(result) = solver.next() {
        print_result(result);
    }
    println!("no more solutions");
}

fn print_result(result: Substitution) {
    if result.mapping.is_empty() {
        println!("yes");
    } else {
        println!("solution: {:?}", result.mapping);
    }
}
