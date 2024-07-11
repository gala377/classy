use classy_blackboard::{
    database::{Database, DefId, Definition, Instance, MethodsBlock, TypeClass, TypeImpl},
    goal::{DomainGoal, Goal},
    slg::{Answer, Forest, SlgSolver},
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
    let allow_debug = database.add_class(TypeClass {
        name: "AllowDebug".to_string(),
        type_params: vec!["a".into()],
        constraints: vec![],
        members: vec![],
    });

    let allow_debug_instance = database.add_instance(Instance {
        type_class: allow_debug,
        args: vec![Ty::Ref(int)],
        type_params: vec![],
        constraints: vec![],
    });
    let debug = database.add_class(TypeClass {
        name: "Debug".to_string(),
        type_params: vec!["a".into()],
        constraints: vec![],
        members: vec![(
            "debug".into(),
            DefId(111),
            Ty::Fn(vec![], Box::new(Ty::Ref(int))),
        )],
    });
    let debug_instanace = database.add_instance(Instance {
        type_class: debug,
        args: vec![Ty::Generic {
            scopes: 0,
            index: 0,
        }],
        type_params: vec!["a".into()],
        constraints: vec![
            Constraint::Class(
                show,
                vec![Ty::Generic {
                    scopes: 0,
                    index: 0,
                }],
            ),
            Constraint::Class(
                allow_debug,
                vec![Ty::Generic {
                    scopes: 0,
                    index: 0,
                }],
            ),
        ],
    });
    database.add_method_block(MethodsBlock {
        name: None,
        on_type: Ty::Generic {
            scopes: 0,
            index: 0,
        },
        type_params: vec!["a".into()],
        constraints: vec![Constraint::Class(
            debug,
            vec![Ty::Generic {
                scopes: 0,
                index: 0,
            }],
        )],
        methods: vec![Definition {
            name: "debug".into(),
            type_params: vec![],
            ty: Ty::Fn(vec![], Box::new(Ty::Ref(int))),
        }],
    });
    let int_show_instance = database.add_instance(Instance {
        type_class: show,
        args: vec![Ty::Ref(int)],
        type_params: vec![],
        constraints: vec![],
    });
    database.add_instance(Instance {
        type_class: show,
        args: vec![Ty::App(
            Box::new(Ty::Ref(foo)),
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
    database.add_method_block(MethodsBlock {
        name: None,
        on_type: Ty::Ref(int),
        type_params: vec![],
        constraints: vec![],
        methods: vec![Definition {
            name: "to_string".into(),
            type_params: vec![],
            ty: Ty::Fn(vec![], Box::new(Ty::Ref(int))),
        }],
    });

    database.add_method_block(MethodsBlock {
        name: None,
        on_type: Ty::Ref(int),
        type_params: vec![],
        constraints: vec![],
        methods: vec![Definition {
            name: "as_string".into(),
            type_params: vec![],
            ty: Ty::Fn(vec![], Box::new(Ty::Ref(int))),
        }],
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
        args: vec![
            Ty::App(Box::new(Ty::Ref(bar)), vec![Ty::Ref(int)]),
            Ty::Ref(int),
        ],
    });

    let mut forest = Forest::new();
    println!("\n\n\n\n\n\n\n\n");
    let solver = SlgSolver::new(&database, &mut forest, query);
    let results = solver.take(10).collect::<Vec<_>>();
    println!("\n\n\n\n");
    for (i, result) in results.iter().enumerate() {
        print!("result {}: ", i);
        print_result(result);
    }
    let query = Goal::Domain(DomainGoal::FindMethod {
        name: "debug".into(),
        on_type: Ty::Ref(int),
    });
    let solver = SlgSolver::new(&database, &mut forest, query);
    let results = solver.take(10).collect::<Vec<_>>();
    println!("\n\n\n\n");

    println!("Int -> {int:?}");
    println!("forall a. Debug(a) -> {debug_instanace:?}");
    println!("Show(Int) -> {int_show_instance:?}");
    println!("AllowDebug(Int) -> {allow_debug_instance:?}");

    if results.is_empty() {
        println!("no results");
    }
    for (i, result) in results.iter().enumerate() {
        print!("result {}: \n", i);
        print_result(result);
    }
}

fn print_result(result: &Answer) {
    if result.subst.mapping.is_empty() {
        println!("yes");
        println!("answer origin: {:?}", result.origin);
        println!("substitution origins: {:?}", result.subst.origins);
        println!("Evidence: {:#?}", result.evidence);
    } else {
        println!("substitutions: {:?}", result.subst.mapping);
        println!("answer origin: {:?}", result.origin);
        println!("substitution origins: {:?}", result.subst.origins);
        println!("Evidence: {:#?}", result.evidence);
    }
}
