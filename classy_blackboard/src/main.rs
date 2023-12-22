use classy_blackboard::{
    database::{Database, TypeImpl},
    goal::{DomainGoal, Goal},
    slg::{Forest, SlgSolver},
    ty::Ty,
};

pub fn main() {
    tracing_subscriber::fmt::init();
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
    database.lower_to_clauses();
    let forest = Forest::new();
    let mut solver = SlgSolver::new(&database, forest);
    let goal = Goal::Forall(
        1,
        Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
            ty: Ty::App(
                foo,
                vec![Ty::Generic {
                    scopes: 0,
                    index: 0,
                }],
            ),
        })),
    );
    let result = solver.solve(goal);
    println!("result: {:?}", result);

    let goal_2 = Goal::Domain(DomainGoal::TypeWellFormed {
        ty: Ty::App(foo, vec![Ty::Ref(int)]),
    });
    let result_2 = solver.solve(goal_2);
    println!("result_2: {:?}", result_2);
}
