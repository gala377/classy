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
