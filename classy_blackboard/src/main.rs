use classy_blackboard::{
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
    let bar = database.add_type_impl(TypeImpl {
        name: "Bar".into(),
        type_params: vec!["a".into()],
        constraints: vec![],
        fields: vec![],
    });
    let convert = database.add_class(TypeClass {
        name: "Convert".into(),
        type_params: vec!["a".into(), "b".into()],
        constraints: vec![],
        members: vec![],
    });
    database.add_instance(Instance {
        type_class: convert,
        type_params: vec!["a".into()],
        args: vec![
            Ty::Generic {
                scopes: 0,
                index: 0,
            },
            Ty::Ref(int),
        ],
        constraints: vec![],
    });
    database.lower_to_clauses();
    let query = Goal::Exists(
        1,
        Box::new(Goal::Domain(DomainGoal::InstanceExistsAndWellFormed {
            head: convert,
            args: vec![
                Ty::Generic {
                    scopes: 0,
                    index: 0,
                },
                Ty::Ref(int),
            ],
        })),
    );
    let _query = Goal::Domain(DomainGoal::InstanceExistsAndWellFormed {
        head: convert,
        args: vec![Ty::App(bar, vec![Ty::Ref(int)]), Ty::Ref(int)],
    });

    let mut forest = Forest::new();
    let solver = SlgSolver::new(&database, &mut forest, query);
    let results = solver.take(10).collect::<Vec<_>>();
    println!("\n\n\n\n");
    for (i, result) in results.iter().enumerate() {
        print!("result {}: ", i);
        print_result(result);
    }
}

fn print_result(result: &Substitution) {
    if result.mapping.is_empty() {
        println!("yes");
        println!("{:?}", result.origins);
    } else {
        println!("{:?}", result.mapping);
        println!("{:?}", result.origins);
    }
}
