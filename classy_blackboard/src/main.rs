use classy_blackboard::{
    clauses::{Constraint, Ty},
    database::Database,
    query,
};

pub fn main() {
    let mut db = Database::new();
    // class Show(a)
    let show = db.add_type_class("Show".to_string(), vec!["a".to_string()], vec![]);
    // class Debug(a)
    let debug = db.add_type_class("Debug".into(), vec!["a".to_string()], vec![]);
    // type Int
    let int = db.add_struct("Int".to_string(), vec![], vec![]);
    // type String
    let string = db.add_struct("String".into(), vec![], vec![]);
    // type Foo(a)
    let foo = db.add_struct(
        "Foo".to_owned(),
        vec!["a".to_owned()],
        vec![Constraint::Class(show, vec![Ty::Generic(0)])],
    );
    // instance for Show(Int)
    db.add_instance_for(show, vec![], vec![], vec![Ty::Ref(int)]);
    // instance for { Show(a) } => Debug(a)
    db.add_instance_for(
        debug,
        vec!["a".to_string()],
        vec![Constraint::Class(show, vec![Ty::Generic(0)])],
        vec![Ty::Generic(0)],
    );
    // instance for { Debug(a) } => Debug(Foo(a))
    db.add_instance_for(
        debug,
        vec!["a".to_string()],
        vec![Constraint::Class(debug, vec![Ty::Generic(0)])],
        vec![Ty::App(foo, vec![Ty::Generic(0)])],
    );
    // Foo(String)?
    let query = query::TypeHolds(Ty::App(foo, vec![Ty::Ref(string)]));
    let result_1 = db.run(&query);
    // Foo(Int)?
    let query = query::TypeHolds(Ty::App(foo, vec![Ty::Ref(int)]));
    let result_2 = db.run(&query);
    // Debug(Int)?
    let query = query::TypeHolds(Ty::App(debug, vec![Ty::Ref(int)]));
    let result_3 = db.run(&query);
    // Debug(String)?
    let query = query::TypeHolds(Ty::App(debug, vec![Ty::Ref(string)]));
    let result_4 = db.run(&query);
    // Debug(Foo(Int))?
    println!("\n\n\nGOAL 5\n\n\n");

    let query = query::TypeHolds(Ty::App(debug, vec![Ty::App(foo, vec![Ty::Ref(int)])]));
    let result_5 = db.run(&query);

    assert!(!result_1);
    assert!(result_2);
    assert!(result_3);
    assert!(!result_4);
    assert!(result_5);
}
