use std::collections::HashMap;

use classy_blackboard::{
    clauses::{Constraint, Ty, TyRef},
    database::Database,
    slg::{Canonilized, Forest, SlgSolver, Substitution},
};

pub fn main() {
    let (db, types) = basic_db_prepare();
    let forest = Forest::new();
    let mut solver = SlgSolver::new(&db, forest);
    let query = Ty::App(types["int"], vec![]);
    let query = Canonilized::wrap_ty(query);
    println!("New line for next solution, press q to quit");
    while let Some(result) = solver.solve(query.clone()) {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        match line.as_str().trim() {
            "q" => {
                println!("Finishing for now");
                return;
            }
            _ => {
                println!("Next solution");
                pretty_print(&db, &result);
            }
        }
    }
    println!("No more solutions");
    /*
        TODO:
        - Check if the instance is well formed
            for example type
                type { Show(a) } => Foo(a) {}

                Foo(String) is not well formed because !Show(String) but we accept

                instance for Show(Foo(String))

                which we shouldn't because we can't prove Show(String).
                So we somehowe need to take this into account and prove types withing the instances
                to be able to use them, but we would need to propagate bounds, for example

                instance for { Show(a) } => Show(Foo(a))

                is well formed because we can prove Show(a) from the context, but trying to solve just
                Foo(?0) will give us an error because we don't know how to prove Show(?0)
    */
}

fn pretty_print(db: &Database, subst: &Substitution) {
    for (key, value) in subst.mapping.iter() {
        let mut buff = String::new();
        db.make_readable(value.clone(), &mut buff);
        println!("?{} = {}", key, buff);
    }
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
    db.add_instance_for(
        show,
        vec![],
        vec![],
        vec![Ty::App(foo, vec![Ty::Ref(string)])],
    );
    let mut names = HashMap::new();
    names.insert("string".into(), string);
    names.insert("show".into(), show);
    names.insert("debug".into(), debug);
    names.insert("int".into(), int);
    names.insert("foo".into(), foo);
    (db, names)
}
