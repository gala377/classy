use std::collections::HashMap;

use classy_blackboard::{
    clauses::{Constraint, Ty, TyRef},
    database::Database,
    slg::{CanonilizedGoal, Forest, SlgSolver},
};

pub fn main() {
    let (db, types) = basic_db_prepare();
    let forest = Forest::new();
    let mut solver = SlgSolver::new(&db, forest);
    let query = Ty::App(types["debug"], vec![Ty::UnBound(0)]);
    let query = CanonilizedGoal::wrap_ty(query);
    while let Some(result) = solver.solve(query.clone()) {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        match line.as_str().trim() {
            "q" => {
                println!("\nFinishing for now");
                return;
            }
            _ => {
                println!("\nNext solution");
                println!("Result: {:#?}", result);
            }
        }
    }
    println!("No more solutions");
}

fn basic_db_prepare() -> (Database, HashMap<String, TyRef>) {
    let mut db = Database::new();
    // class Show(a)
    let show = db.add_type_class(
        "Show".to_string(),
        vec!["a".to_string()],
        vec![],
        HashMap::new(),
    );
    // class Debug(a)
    let debug = db.add_type_class(
        "Debug".into(),
        vec!["a".to_string()],
        vec![],
        HashMap::new(),
    );
    // type Int
    let int = db.add_struct("Int".to_string(), vec![], vec![], HashMap::new());
    // type String
    let string = db.add_struct("String".into(), vec![], vec![], HashMap::new());
    // type Foo(a)
    let foo = db.add_struct(
        "Foo".to_owned(),
        vec!["a".to_owned()],
        vec![Constraint::Class(show, vec![Ty::Generic(0)])],
        HashMap::new(),
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
    let mut names = HashMap::new();
    names.insert("string".into(), string);
    names.insert("show".into(), show);
    names.insert("debug".into(), debug);
    names.insert("int".into(), int);
    names.insert("foo".into(), foo);
    (db, names)
}
