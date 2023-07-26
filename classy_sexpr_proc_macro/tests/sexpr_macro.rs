use classy_sexpr::{Atom, SExpr};
use classy_sexpr_proc_macro::sexpr;

#[test]
fn parses_simple_integer() {
    assert_eq!(sexpr!(1), SExpr::Atom(Atom::Number(1)));
}

#[test]
fn parses_string_literal() {
    assert_eq!(
        sexpr!("hello"),
        SExpr::Atom(Atom::String("hello".to_string()))
    );
}

#[test]
fn parses_symbol() {
    assert_eq!(
        sexpr!(hello),
        SExpr::Atom(Atom::Symbol("hello".to_string()))
    );
}

#[test]
fn parses_bool() {
    assert_eq!(sexpr!(true), SExpr::Atom(Atom::Symbol("true".to_string())));
    assert_eq!(
        sexpr!(false),
        SExpr::Atom(Atom::Symbol("false".to_string()))
    );
}
