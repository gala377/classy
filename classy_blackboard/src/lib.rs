pub mod clauses;
pub mod database;
pub mod query;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        clauses::{Constraint, Ty, TyRef},
        database::Database,
        query,
    };

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

    #[test]
    fn simple_test_1() {
        let (mut db, names) = basic_db_prepare();
        // Foo(String)?
        let query = query::TypeHolds(Ty::App(names["foo"], vec![Ty::Ref(names["string"])]));
        let result_1 = db.run(&query);
        assert!(!result_1);
    }

    #[test]
    fn simple_test_2() {
        let (mut db, names) = basic_db_prepare();
        // Foo(Int)?
        let query = query::TypeHolds(Ty::App(names["foo"], vec![Ty::Ref(names["int"])]));
        let result_2 = db.run(&query);
        assert!(result_2);
    }

    #[test]
    fn simple_test_3() {
        let (mut db, names) = basic_db_prepare();
        // Debug(Int)?
        let query = query::TypeHolds(Ty::App(names["debug"], vec![Ty::Ref(names["int"])]));
        let result_3 = db.run(&query);
        assert!(result_3);
    }

    #[test]
    fn simple_test_4() {
        let (mut db, names) = basic_db_prepare();
        // Debug(String)?
        let query = query::TypeHolds(Ty::App(names["debug"], vec![Ty::Ref(names["string"])]));
        let result_4 = db.run(&query);
        assert!(!result_4);
    }

    #[test]
    fn simple_test_5() {
        let (mut db, names) = basic_db_prepare();
        // Debug(Foo(Int))?
        let query = query::TypeHolds(Ty::App(
            names["debug"],
            vec![Ty::App(names["foo"], vec![Ty::Ref(names["int"])])],
        ));
        let result_5 = db.run(&query);
        assert!(result_5);
    }
}
