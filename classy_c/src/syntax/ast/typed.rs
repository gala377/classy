use std::collections::HashMap;

use crate::typecheck::r#type::Type;

#[derive(Default)]
pub struct Program {
    pub items: Vec<TopLevelItem>,
}

pub enum TopLevelItem {
    FunctionDefinition(FunctionDefinition),
    TypeDefinition(TypeDefinition),
}

pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Expr,
    pub typ: Type,
}

pub struct TypeDefinition {
    pub name: String,
    pub body: Type,
}

pub struct Expr {
    pub inner: ExprKind,
    pub typ: Type,
}

pub enum ExprKind {
    Unit,
    Sequence(Vec<Expr>),
    Assignment {
        lval: Box<Expr>,
        rval: Box<Expr>,
    },
    IntConst(isize),
    StringConst(String),
    FloatConst(f64),
    BoolConst(bool),
    Name(String),
    FunctionCall {
        func: Box<Expr>,
        args: Vec<Expr>,
        kwargs: HashMap<String, Expr>,
    },
    Access {
        val: Box<Expr>,
        field: String,
    },
    Tuple(Vec<Expr>),
    Lambda {
        parameters: Vec<String>,
        body: Box<Expr>,
    },
    TypedExpr {
        expr: Box<Expr>,
        typ: Type,
    },
    StructLiteral {
        strct: String,
        values: HashMap<String, Expr>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    Return(Box<Expr>),
    If {
        cond: Box<Expr>,
        body: Box<Expr>,
        else_body: Option<Box<Expr>>,
    },
    Let {
        name: String,
        typ: Type,
        init: Box<Expr>,
    },
}
