use classy_sexpr::{Atom, SExpr, ToSExpr};
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

#[test]
fn interpoaltion_bool() {
    let b = true;
    assert_eq!(sexpr!($b), sexpr!(true));
}

#[test]
fn interpoaltion_string() {
    let b = "hello";
    assert_eq!(sexpr!($b), sexpr!("hello"));
}

#[test]
fn interpoaltion_int() {
    let b: usize = 1;
    assert_eq!(sexpr!($b), sexpr!(1));
}

#[test]
fn interpoaltion_vec() {
    let b: Vec<usize> = vec![1, 2, 3];
    assert_eq!(sexpr!($b), sexpr!((1 2 3)));
}

#[test]
fn expressions_interpolation() {
    assert_eq!(sexpr!((1 ${1 + 1} ${2 * 3})), sexpr!((1 2 6)));
}

#[test]
fn test_splicing() {
    let values = vec![1, 2, 3];
    assert_eq!(
        sexpr!((0 @values 4)),
        sexpr!((
            0
            1
            2
            3
            4
        ))
    )
}

#[test]
fn test_paths() {
    assert_eq!(
        sexpr!(a::b::c),
        SExpr::Atom(Atom::Symbol("a::b::c".to_string()))
    );
}
