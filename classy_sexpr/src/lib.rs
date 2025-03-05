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
    Hint(String),
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
            Atom::Hint(_) => Ok(()),
        }
    }
}

#[derive(Default)]
struct FormatingOptions {
    indent_elements: Option<usize>,
    indent: usize,
}

impl FormatingOptions {
    fn all_indent(&self) -> usize {
        self.indent + self.indent_elements.unwrap_or_default()
    }
}

pub fn pretty_print(sexpr: &SExpr) {
    let to_print = pretty_string(FormatingOptions::default(), sexpr);
    println!("{}", to_print);
}

fn pretty_string(options: FormatingOptions, sexpr: &SExpr) -> String {
    fn make_indent(indent: usize) -> String {
        "  ".repeat(indent)
    }
    match sexpr {
        SExpr::List(sexprs) => {
            let mut res = String::new();
            res.push('(');
            let end = sexprs.len();
            let mut i = 0;
            while i < end {
                let expr = &sexprs[i];
                match expr {
                    SExpr::Atom(Atom::Hint(hint)) => match hint.as_str() {
                        "en" => {
                            let mut next_options = FormatingOptions::default();
                            next_options.indent_elements = Some(options.all_indent() + 1);
                            res += &pretty_string(next_options, &sexprs[i + 1]);
                            if i + 1 < end - 1 {
                                res += " ";
                            }
                            i += 2;
                            continue;
                        }
                        "en2" => {
                            let mut next_options = FormatingOptions::default();
                            next_options.indent_elements = Some(options.all_indent() + 2);
                            res += &pretty_string(next_options, &sexprs[i + 1]);
                            if i + 1 < end - 1 {
                                res += " ";
                            }
                            i += 2;
                            continue;
                        }
                        "en3" => {
                            let mut next_options = FormatingOptions::default();
                            next_options.indent_elements = Some(options.all_indent() + 3);
                            res += &pretty_string(next_options, &sexprs[i + 1]);
                            if i + 1 < end - 1 {
                                res += " ";
                            }
                            i += 2;
                            continue;
                        }
                        "n" => {
                            let mut next_options = FormatingOptions::default();
                            next_options.indent = options.all_indent() + 1;
                            res += "\n";
                            res += &make_indent(next_options.indent);
                            res += &pretty_string(next_options, &sexprs[i + 1]);
                            if i + 1 < end - 1 {
                                res += " ";
                            }
                            i += 2;
                            continue;
                        }
                        "n2" => {
                            let mut next_options = FormatingOptions::default();
                            next_options.indent = options.all_indent() + 2;
                            res += "\n";
                            res += &make_indent(next_options.indent);
                            res += &pretty_string(next_options, &sexprs[i + 1]);
                            if i + 1 < end - 1 {
                                res += " ";
                            }
                            i += 2;
                            continue;
                        }
                        _ => {}
                    },
                    _ => {
                        let add = if let Some(indent) = options.indent_elements {
                            format!("\n{}", make_indent(indent))
                        } else {
                            String::new()
                        };
                        let mut next_options = FormatingOptions::default();
                        next_options.indent = options.all_indent() + 1;
                        res += &add;
                        res += &pretty_string(next_options, expr);
                        if i < end - 1 {
                            res += " ";
                        }
                    }
                }
                i += 1;
            }
            res.push(')');
            res
        }
        SExpr::Atom(atom) => match atom {
            Atom::Symbol(sym) => format!("{sym}"),
            Atom::Number(num) => format!("{num}"),
            Atom::String(val) => format!("\"{val}\""),
            Atom::Hint(_) => "".to_string(),
        },
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
