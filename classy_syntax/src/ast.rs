use std::{collections::HashMap, ops::Range};

pub mod fold;
pub mod visitor;

pub use fold::Folder;
pub use visitor::Visitor;

// cargo is actually wrong about this, it confuses
// usage of this function with unstable feature
// we did not enable
#[allow(dead_code)]
fn default<T: Default>() -> T {
    Default::default()
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Program {
    pub items: Vec<TopLevelItem>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TopLevelItem {
    TypeDefinition(TypeDefinition),
    FunctionDefinition(FunctionDefinition),
    MethodsBlock(MethodsBlock),
    ConstDefinition(ConstDefinition),
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ConstDefinition {
    pub id: usize,
    pub name: String,
    pub typ: Typ,
    pub init: Expr,
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: String,
    pub definition: DefinedType,
    pub type_variables: Vec<TypeVariable>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DefinedType {
    Record(Record),
    ADT(ADT),
    Alias(Alias),
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Record {
    pub fields: Vec<TypedName>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ADT {
    pub discriminants: Vec<Discriminant>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Discriminant {
    pub constructor: String,
    pub arguments: DiscriminantKind,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DiscriminantKind {
    Empty,
    Tuple(Vec<Typ>),
    Record(Vec<(String, Typ)>),
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Alias {
    pub for_type: Typ,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypedName {
    pub name: String,
    pub typ: Typ,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionDefinition {
    pub name: String,
    pub typ: Typ,
    pub parameters: Vec<String>,
    pub body: Expr,
    pub attributes: Vec<String>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct MethodsBlock {
    pub name: Option<String>,
    pub typ: Typ,
    pub methods: Vec<FunctionDefinition>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Typ {
    Unit,
    Name(String),
    Application {
        callee: Box<Typ>,
        args: Vec<Typ>,
    },
    Array(Box<Typ>),
    Function {
        generics: Vec<String>,
        args: Vec<Typ>,
        ret: Box<Typ>,
    },
    Tuple(Vec<Typ>),
    Poly(Vec<String>, Box<Typ>),
    ToInfere,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypeVariable {
    pub name: String,
    // todo: for the future
    // pub constraints: Vec<Contraint>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Expr {
    pub id: usize,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
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
        parameters: Vec<TypedName>,
        body: Box<Expr>,
    },
    TypedExpr {
        expr: Box<Expr>,
        typ: Typ,
    },
    StructLiteral {
        strct: Path,
        values: HashMap<String, Expr>,
    },
    AdtTupleConstructor {
        typ: String,
        constructor: String,
        args: Vec<Expr>,
    },
    AdtStructConstructor {
        typ: String,
        constructor: String,
        fields: Vec<(String, Expr)>,
    },
    AdtUnitConstructor {
        typ: String,
        constructor: String,
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
        typ: Typ,
        init: Box<Expr>,
    },
    LetRec {
        definitions: Vec<FunctionDefinition>,
    },
    AnonType {
        fields: Vec<(String, Expr)>,
    },
    ArrayLiteral {
        typ: Typ,
        size: Box<Expr>,
        init: Vec<Expr>,
    },
    IndexAccess {
        lhs: Box<Expr>,
        index: Box<Expr>,
    },
    Match {
        expr: Box<Expr>,
        cases: Vec<(Pattern, Expr, Option<Box<Expr>>)>,
    },
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        args: Vec<Expr>,
        kwargs: HashMap<String, Expr>,
    },
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Pattern {
    pub id: usize,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum PatternKind {
    Name(String),
    Tuple(Vec<Pattern>),
    Struct {
        strct: String,
        fields: HashMap<String, Pattern>,
    },
    TupleStruct {
        strct: String,
        fields: Vec<Pattern>,
    },
    AnonStruct {
        fields: HashMap<String, Pattern>,
    },
    Array(Vec<Pattern>),
    Wildcard,
    Unit,
    String(String),
    Int(isize),
    Bool(bool),
    /// accumulate rest of values in an array
    /// *name
    Rest(String),

    /// Patterns in form Type.Case pattern
    /// used when one wants to explicitly specify the type
    /// to look for cases in
    TypeSpecifier(String, Box<Pattern>),
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Path(pub Vec<String>);

impl Path {
    pub fn try_from_expr(expr: Expr) -> Option<Self> {
        fn match_segment(expr: Expr) -> Option<Vec<String>> {
            match expr.kind {
                ExprKind::Name(val) => Some(vec![val]),
                ExprKind::Access { val, field } => {
                    let mut path = match_segment(*val)?;
                    path.push(field);
                    Some(path)
                }
                _ => None,
            }
        }
        match_segment(expr).map(Path)
    }
}

/// Explicit PartialEq implementations to skip span comparison.
/// Only used in tests.
#[cfg(test)]
impl PartialEq for TypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.type_variables == other.type_variables
            && self.definition == other.definition
    }
}

/// Builders are only used in tests to create AST to test against
pub struct Builder {
    pub res: Program,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            res: Program { items: Vec::new() },
        }
    }

    pub fn empty_struct(mut self, name: impl Into<String>) -> Self {
        self.res.items.push(TopLevelItem::TypeDefinition(
            StructDefBuilder::new().name(name).build(),
        ));
        self
    }

    pub fn struct_def(
        mut self,
        name: impl Into<String>,
        str_builder: impl FnOnce(StructDefBuilder) -> StructDefBuilder,
    ) -> Self {
        let r#struct = StructDefBuilder::new().name(name);
        let r#struct = str_builder(r#struct);
        self.res
            .items
            .push(TopLevelItem::TypeDefinition(r#struct.build()));
        self
    }

    pub fn adt_def(
        mut self,
        name: impl Into<String>,
        adt_builder: impl FnOnce(AdtDefBuilder) -> AdtDefBuilder,
    ) -> Self {
        let adt = AdtDefBuilder::new().name(name);
        let adt = adt_builder(adt);
        self.res
            .items
            .push(TopLevelItem::TypeDefinition(adt.build()));
        self
    }

    pub fn unit_fn(
        self,
        name: impl Into<String>,
        body: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        self.func_def(name, Vec::new(), |args| args, Typ::Unit, body)
    }

    pub fn func_def(
        mut self,
        name: impl Into<String>,
        generics: Vec<String>,
        parameters: impl FnOnce(TypedNameListBuilder) -> TypedNameListBuilder,
        ret: Typ,
        body: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        let parameters = parameters(default()).build();
        let body = body(default()).build();
        let mut arg_types = Vec::new();
        let mut arg_names = Vec::new();
        for TypedName { name, typ } in parameters {
            arg_types.push(typ);
            arg_names.push(name);
        }
        self.res
            .items
            .push(TopLevelItem::FunctionDefinition(FunctionDefinition {
                name: name.into(),
                parameters: arg_names,
                typ: Typ::Function {
                    args: arg_types,
                    generics,
                    ret: Box::new(ret),
                },
                body,
                attributes: Vec::new(),
            }));
        self
    }

    pub fn build(self) -> Program {
        self.res
    }
}

pub struct StructDefBuilder {
    name: String,
    fields: Vec<TypedName>,
    type_variables: Vec<TypeVariable>,
}

impl StructDefBuilder {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            fields: Vec::new(),
            type_variables: Vec::new(),
        }
    }

    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }
    pub fn field(mut self, name: impl Into<String>, typ: impl Into<String>) -> Self {
        self.fields.push(TypedName {
            name: name.into(),
            typ: Typ::Name(typ.into()),
        });
        self
    }

    pub fn type_var(mut self, name: impl Into<String>) -> Self {
        self.type_variables.push(TypeVariable { name: name.into() });
        self
    }

    pub fn build(self) -> TypeDefinition {
        TypeDefinition {
            name: self.name,
            definition: DefinedType::Record(Record {
                fields: self.fields,
            }),
            type_variables: self.type_variables,
            span: 0..0,
        }
    }
}

#[derive(Default)]
pub struct AdtDefBuilder {
    name: String,
    type_vars: Vec<TypeVariable>,
    discriminants: Vec<Discriminant>,
}

impl AdtDefBuilder {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            type_vars: Vec::new(),
            discriminants: Vec::new(),
        }
    }

    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    pub fn discriminant(mut self, name: impl Into<String>, arguments: &[impl AsRef<str>]) -> Self {
        self.discriminants.push(Discriminant {
            constructor: name.into(),
            arguments: DiscriminantKind::Tuple(
                arguments
                    .iter()
                    .map(|typ| Typ::Name(typ.as_ref().into()))
                    .collect(),
            ),
        });
        self
    }

    pub fn empty_discriminant(self, name: impl Into<String>) -> Self {
        let arguments: &[&'static str] = &[];
        self.discriminant(name, arguments)
    }

    pub fn type_var(mut self, name: impl Into<String>) -> Self {
        self.type_vars.push(TypeVariable { name: name.into() });
        self
    }

    pub fn build(self) -> TypeDefinition {
        TypeDefinition {
            name: self.name,
            span: 0..0,
            type_variables: self.type_vars,
            definition: DefinedType::ADT(ADT {
                discriminants: self.discriminants,
            }),
        }
    }
}

#[derive(Default)]
pub struct TypedNameListBuilder {
    res: Vec<TypedName>,
}

impl TypedNameListBuilder {
    pub fn name(mut self, name: impl Into<String>, typ: impl Into<String>) -> Self {
        self.res.push(TypedName {
            name: name.into(),
            typ: Typ::Name(typ.into()),
        });
        self
    }

    pub fn build(self) -> Vec<TypedName> {
        self.res
    }
}

fn fake_expr(kind: ExprKind) -> Expr {
    Expr { id: 0, kind }
}

#[derive(Default)]
pub struct ExprBuilder {
    res: Option<Expr>,
}

impl ExprBuilder {
    pub fn integer(mut self, val: isize) -> Self {
        assert!(self.res.is_none());
        self.res = Some(fake_expr(ExprKind::IntConst(val)));
        self
    }

    pub fn float(mut self, val: f64) -> Self {
        assert!(self.res.is_none());
        self.res = Some(fake_expr(ExprKind::FloatConst(val)));
        self
    }

    pub fn name(mut self, s: impl Into<String>) -> Self {
        assert!(self.res.is_none());
        self.res = Some(fake_expr(ExprKind::Name(s.into())));
        self
    }

    pub fn function_call(
        mut self,
        callee: impl FnOnce(ExprBuilder) -> ExprBuilder,
        args: impl FnOnce(ExprListBuilder) -> ExprListBuilder,
        kwargs: impl FnOnce(KwArgsBuilder) -> KwArgsBuilder,
    ) -> Self {
        assert!(self.res.is_none());
        let func = callee(default()).build();
        let args = args(default()).build();
        let kwargs = kwargs(default()).build();
        self.res = Some(fake_expr(ExprKind::FunctionCall {
            func: Box::new(func),
            args,
            kwargs,
        }));
        self
    }

    pub fn access(
        mut self,
        lhs: impl FnOnce(ExprBuilder) -> ExprBuilder,
        field: impl Into<String>,
    ) -> Self {
        assert!(self.res.is_none());
        let expr = lhs(default()).build();
        self.res = Some(fake_expr(ExprKind::Access {
            val: Box::new(expr),
            field: field.into(),
        }));
        self
    }

    pub fn tuple(mut self, vals: impl FnOnce(ExprListBuilder) -> ExprListBuilder) -> Self {
        assert!(self.res.is_none());
        self.res = Some(fake_expr(ExprKind::Tuple(vals(default()).build())));
        self
    }

    pub fn unit(mut self) -> Self {
        assert!(self.res.is_none());
        self.res = Some(fake_expr(ExprKind::Unit));
        self
    }

    pub fn sequence(mut self, exprs: impl FnOnce(ExprListBuilder) -> ExprListBuilder) -> Self {
        assert!(self.res.is_none());
        self.res = Some(fake_expr(ExprKind::Sequence(exprs(default()).build())));
        self
    }

    pub fn assignment(
        mut self,
        lhs: impl FnOnce(ExprBuilder) -> ExprBuilder,
        rhs: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        assert!(self.res.is_none());
        let lhs = lhs(default()).build();
        let rhs = rhs(default()).build();
        self.res = Some(fake_expr(ExprKind::Assignment {
            lval: Box::new(lhs),
            rval: Box::new(rhs),
        }));
        self
    }

    pub fn lambda_no_types<P: Into<String> + Clone>(
        mut self,
        parameters: &[P],
        body: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        assert!(self.res.is_none());
        let body = body(default()).build();
        self.res = Some(fake_expr(ExprKind::Lambda {
            parameters: parameters
                .iter()
                .cloned()
                .map(|name| TypedName {
                    name: name.into(),
                    typ: Typ::ToInfere,
                })
                .collect(),
            body: Box::new(body),
        }));
        self
    }

    pub fn lambda(
        mut self,
        parameters: impl FnOnce(TypedNameListBuilder) -> TypedNameListBuilder,
        body: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        assert!(self.res.is_none());
        let body = body(default()).build();
        self.res = Some(fake_expr(ExprKind::Lambda {
            parameters: parameters(default()).build(),
            body: Box::new(body),
        }));
        self
    }

    pub fn typed_expr(
        mut self,
        expr: impl FnOnce(ExprBuilder) -> ExprBuilder,
        typ: impl Into<String>,
    ) -> Self {
        assert!(self.res.is_none());
        let expr = expr(default()).build();
        self.res = Some(fake_expr(ExprKind::TypedExpr {
            expr: Box::new(expr),
            typ: Typ::Name(typ.into()),
        }));
        self
    }

    pub fn r#return(mut self, expr: impl FnOnce(ExprBuilder) -> ExprBuilder) -> Self {
        assert!(self.res.is_none());
        let expr = expr(default()).build();
        self.res = Some(fake_expr(ExprKind::Return(Box::new(expr))));
        self
    }

    pub fn r#while(
        mut self,
        cond: impl FnOnce(ExprBuilder) -> ExprBuilder,
        body: impl FnOnce(ExprListBuilder) -> ExprListBuilder,
    ) -> Self {
        let cond = cond(default()).build();
        let body = fake_expr(ExprKind::Sequence(body(default()).build()));
        self.res = Some(fake_expr(ExprKind::While {
            cond: Box::new(cond),
            body: Box::new(body),
        }));
        self
    }

    pub fn r#if(
        mut self,
        cond: impl FnOnce(ExprBuilder) -> ExprBuilder,
        body: impl FnOnce(ExprListBuilder) -> ExprListBuilder,
        r#else: impl FnOnce(ExprListBuilder) -> ExprListBuilder,
    ) -> Self {
        let cond = cond(default()).build();
        let body = fake_expr(ExprKind::Sequence(body(default()).build()));
        let r#else = r#else(default()).build();
        let r#else = if r#else.is_empty() {
            None
        } else {
            Some(Box::new(fake_expr(ExprKind::Sequence(r#else))))
        };
        self.res = Some(fake_expr(ExprKind::If {
            cond: Box::new(cond),
            body: Box::new(body),
            else_body: r#else,
        }));
        self
    }

    pub fn r#else_if(
        mut self,
        cond: impl FnOnce(ExprBuilder) -> ExprBuilder,
        body: impl FnOnce(ExprListBuilder) -> ExprListBuilder,
        r#else: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        let cond = cond(default()).build();
        let body = fake_expr(ExprKind::Sequence(body(default()).build()));
        let r#else = r#else(default()).build();
        let r#else = Box::new(r#else);
        self.res = Some(fake_expr(ExprKind::If {
            cond: Box::new(cond),
            body: Box::new(body),
            else_body: Some(r#else),
        }));
        self
    }

    pub fn try_build(self) -> Option<Expr> {
        self.res
    }

    pub fn r#let(
        mut self,
        name: impl Into<String>,
        init: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        let init = init(default()).build();
        self.res = Some(fake_expr(ExprKind::Let {
            name: name.into(),
            typ: Typ::ToInfere,
            init: Box::new(init),
        }));
        self
    }

    pub fn struct_literal(
        mut self,
        strct: impl Into<String>,
        values: impl FnOnce(KwArgsBuilder) -> KwArgsBuilder,
    ) -> Self {
        let values = values(default()).build();
        self.res = Some(fake_expr(ExprKind::StructLiteral {
            strct: Path(vec![strct.into()]),
            values,
        }));
        self
    }

    pub fn build(self) -> Expr {
        self.res.unwrap()
    }
}

#[derive(Default)]
pub struct ExprListBuilder {
    res: Vec<Expr>,
}

impl ExprListBuilder {
    pub fn build(self) -> Vec<Expr> {
        self.res
    }

    pub fn add_expr(mut self, f: impl FnOnce(ExprBuilder) -> ExprBuilder) -> Self {
        let expr = f(default()).build();
        self.res.push(expr);
        self
    }
}

#[derive(Default)]
pub struct KwArgsBuilder {
    res: HashMap<String, Expr>,
}

impl KwArgsBuilder {
    pub fn build(self) -> HashMap<String, Expr> {
        self.res
    }

    pub fn add(
        mut self,
        name: impl Into<String>,
        expr: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> KwArgsBuilder {
        let expr = expr(default()).build();
        self.res.insert(name.into(), expr);
        self
    }
}
impl Expr {
    pub fn pretty(&self) -> String {
        match &self.kind {
            ExprKind::Unit => "".to_string(),
            ExprKind::Sequence(exprs) => {
                let mut res = "seq { ".to_string();
                for expr in exprs {
                    res.push_str(&expr.pretty());
                    res.push_str("; ");
                }
                res.push_str("}");
                res
            }
            ExprKind::Assignment { lval, rval } => {
                format!("{} = {}", lval.pretty(), rval.pretty())
            }
            ExprKind::IntConst(i) => i.to_string(),
            ExprKind::StringConst(s) => {
                let mut res = "\"".to_string();
                res.push_str(&s);
                res.push_str("\"");
                res
            }
            ExprKind::FloatConst(f) => {
                let res = f.to_string();
                res
            }
            ExprKind::BoolConst(b) => {
                let res = b.to_string();
                res
            }
            ExprKind::Name(n) => n.clone(),
            ExprKind::FunctionCall { func, args, kwargs } => {
                let mut res = func.pretty();
                res.push_str("(");
                for arg in args {
                    res.push_str(&arg.pretty());
                    res.push_str(", ");
                }
                for (name, arg) in kwargs {
                    res.push_str(&format!("{} = {}, ", name, arg.pretty()));
                }
                res.push_str(")");
                res
            }
            ExprKind::Access { val, field } => {
                let mut res = val.pretty();
                res.push_str(".");
                res.push_str(&field);
                res
            }
            ExprKind::Tuple(args) => {
                let mut res = "(".to_string();
                for arg in args {
                    res.push_str(&arg.pretty());
                    res.push_str(", ");
                }
                res.push_str(")");
                res
            }
            ExprKind::Lambda { parameters, body } => {
                let mut res = "fn (".to_string();
                for TypedName { name, .. } in parameters {
                    res.push_str(&name);
                }
                res.push_str(") ");
                res.push_str(&body.pretty());
                res
            }
            ExprKind::TypedExpr { expr, .. } => expr.pretty(),
            ExprKind::StructLiteral { strct, values } => {
                let mut res = strct.0.join(".");
                res.push_str(" { ");
                for (name, val) in values {
                    res.push_str(&format!("{} = {}, ", name, val.pretty()));
                }
                res.push_str("}");
                res
            }
            ExprKind::AdtTupleConstructor {
                typ,
                constructor,
                args,
            } => {
                let mut res = format!("{}::{}", typ, constructor);
                res.push_str("(");
                for arg in args {
                    res.push_str(&arg.pretty());
                    res.push_str(", ");
                }
                res.push_str(")");
                res
            }
            ExprKind::AdtStructConstructor {
                typ,
                constructor,
                fields,
            } => {
                let mut res = format!("{}::{}", typ, constructor);
                res.push_str("{ ");
                for (name, val) in fields {
                    res.push_str(&format!("{} = {}, ", name, val.pretty()));
                }
                res.push_str("}");
                res
            }
            ExprKind::AdtUnitConstructor { typ, constructor } => {
                format!("{}::{}", typ, constructor)
            }
            ExprKind::While { cond, body } => {
                let mut res = "while ".to_string();
                res.push_str(&format!("({})", &cond.pretty()));
                res.push_str(" ");
                res.push_str(&body.pretty());
                res
            }
            ExprKind::Return(e) => {
                let mut res = "return ".to_string();
                res.push_str(&e.pretty());
                res
            }
            ExprKind::If {
                cond,
                body,
                else_body,
            } => {
                let mut res = "if ".to_string();
                res.push_str(&cond.pretty());
                res.push_str(" ");
                res.push_str(&body.pretty());
                if let Some(else_body) = else_body {
                    res.push_str(" else ");
                    res.push_str(&else_body.pretty());
                }
                res
            }
            ExprKind::Let { name, init, .. } => {
                let mut res = "let ".to_string();
                res.push_str(&name);
                res.push_str(" = ");
                res.push_str(&init.pretty());
                res
            }
            ExprKind::LetRec { definitions } => {
                let mut res = "let rec ".to_string();
                for def in definitions {
                    res.push_str(&def.name);
                    res.push_str(" = ");
                    res.push_str(&def.body.pretty());
                }
                res
            }
            ExprKind::AnonType { fields } => {
                let mut res = "type { ".to_string();
                for (name, val) in fields {
                    res.push_str(&format!("{} = {}, ", name, val.pretty()));
                }
                res.push_str("}");
                res
            }
            ExprKind::ArrayLiteral { size, init, .. } => {
                let mut res = "array[".to_string();
                res.push_str(&size.pretty());
                res.push_str("] = [");
                for val in init {
                    res.push_str(&val.pretty());
                    res.push_str(", ");
                }
                res.push_str("]");
                res
            }
            ExprKind::IndexAccess { lhs, index } => {
                let mut res = lhs.pretty();
                res.push_str("[");
                res.push_str(&index.pretty());
                res.push_str("]");
                res
            }
            ExprKind::Match { expr, cases } => {
                let mut res = "match ".to_string();
                res.push_str(&expr.pretty());
                res.push_str(" {");
                for (_, body, guard) in cases {
                    res.push_str(&format!("PATTERN => {}", body.pretty()));
                    if let Some(guard) = guard {
                        res.push_str(" if ");
                        res.push_str(&guard.pretty());
                    }
                    res.push_str("; ");
                }
                res.push_str("}");
                res
            }
            ExprKind::MethodCall {
                receiver,
                method,
                args,
                kwargs,
            } => {
                let mut res = receiver.pretty();
                res.push_str(&format!(".{}(", method));
                for arg in args {
                    res.push_str(&arg.pretty());
                    res.push_str(", ");
                }
                for (name, arg) in kwargs {
                    res.push_str(&format!("{} = {}, ", name, arg.pretty()));
                }
                res.push_str(")");
                res
            }
        }
    }
}
