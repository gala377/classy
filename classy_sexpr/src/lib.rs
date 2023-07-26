use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq)]
pub enum SExpr {
    List(Vec<SExpr>),
    Atom(Atom),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Atom {
    Symbol(String),
    Number(i64),
    String(String),
}

impl std::fmt::Debug for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::List(l) => {
                write!(f, "(")?;
                for (i, e) in l.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:?}", e)?;
                }
                write!(f, ")")
            }
            SExpr::Atom(a) => write!(f, "{:?}", a),
        }
    }
}

impl std::fmt::Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol(s) => write!(f, "`{}", s),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

pub trait ToSExpr {
    fn to_sexpr(self) -> SExpr;
}

impl ToSExpr for SExpr {
    fn to_sexpr(self) -> SExpr {
        self
    }
}

impl ToSExpr for String {
    fn to_sexpr(self) -> SExpr {
        SExpr::Atom(Atom::String(self))
    }
}

impl ToSExpr for &str {
    fn to_sexpr(self) -> SExpr {
        SExpr::Atom(Atom::String(self.to_string()))
    }
}

macro_rules! impl_numbers {
    ($($t: ty),*) => {
        $(impl ToSExpr for $t {
            fn to_sexpr(self) -> SExpr {
                SExpr::Atom(Atom::Number(self as i64))
            }
        })*
    };
}

impl_numbers!(i8, i16, i32, i64, u8, u16, u32, u64, usize, isize);

impl ToSExpr for bool {
    fn to_sexpr(self) -> SExpr {
        SExpr::Atom(Atom::Symbol(
            if self { "true" } else { "false" }.to_string(),
        ))
    }
}

impl<T: ToSExpr> ToSExpr for Vec<T> {
    fn to_sexpr(self) -> SExpr {
        SExpr::List(self.into_iter().map(|x| x.to_sexpr()).collect())
    }
}

impl<T: ToSExpr, U: ToSExpr> ToSExpr for (T, U) {
    fn to_sexpr(self) -> SExpr {
        SExpr::List(vec![self.0.to_sexpr(), self.1.to_sexpr()])
    }
}

impl<K: ToSExpr + std::cmp::Ord, V: ToSExpr> ToSExpr for HashMap<K, V> {
    fn to_sexpr(self) -> SExpr {
        let mut sorted = self.into_iter().collect::<Vec<_>>();
        sorted.sort_by(|(a, _), (b, _)| a.cmp(b));
        SExpr::List(sorted.into_iter().map(|x| x.to_sexpr()).collect())
    }
}
