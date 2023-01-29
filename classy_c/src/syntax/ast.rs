use std::ops::Range;

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

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TopLevelItem {
    TypeDefinition(TypeDefinition),
    FunctionDefinition(FunctionDefinition),
}
#[derive(Debug)]
pub struct TypeDefinition {
    pub name: String,
    pub definition: DefinedType,
    pub type_variables: Vec<TypeVariable>,
    pub span: Range<usize>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DefinedType {
    Record(Record),
    ADT(ADT),
    Alias(Alias),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Record {
    pub fields: Vec<TypedName>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ADT {
    pub discriminants: Vec<Discriminant>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Discriminant {
    pub constructor: String,
    pub arguments: Vec<Typ>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Alias {
    pub for_type: Typ,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypedName {
    pub name: String,
    pub typ: Typ,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionDefinition {
    pub name: String,
    pub typ: Typ,
    pub parameters: Vec<String>,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Typ {
    Unit,
    Name(String),
    Array(Box<Typ>),
    Function { args: Vec<Typ>, ret: Box<Typ> },
    Tuple(Vec<Typ>),
    ToInfere,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeVariable {
    pub name: String,
    // todo: for the future
    // pub constraints: Vec<Contraint>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Unit,
    Sequence(Vec<Expr>),
    Assignment {
        lval: Box<Expr>,
        rval: Box<Expr>,
    },
    IntConst(isize),
    StringConst(String),
    FloatConst(f64),
    Name(String),
    FunctionCall {
        func: Box<Expr>,
        args: Vec<Expr>,
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
#[cfg(test)]
pub struct Builder {
    res: Program,
}

#[cfg(test)]
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
        self.func_def(name, |args| args, Typ::Unit, body)
    }

    pub fn func_def(
        mut self,
        name: impl Into<String>,
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
                    ret: Box::new(ret),
                },
                body,
            }));
        self
    }

    pub fn build(self) -> Program {
        self.res
    }
}

#[cfg(test)]
pub struct StructDefBuilder {
    name: String,
    fields: Vec<TypedName>,
    type_variables: Vec<TypeVariable>,
}

#[cfg(test)]
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
            arguments: arguments
                .iter()
                .map(|typ| Typ::Name(typ.as_ref().into()))
                .collect(),
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

#[derive(Default)]
pub struct ExprBuilder {
    res: Option<Expr>,
}

impl ExprBuilder {
    pub fn integer(mut self, val: isize) -> Self {
        assert!(self.res.is_none());
        self.res = Some(Expr::IntConst(val));
        self
    }

    pub fn float(mut self, val: f64) -> Self {
        assert!(self.res.is_none());
        self.res = Some(Expr::FloatConst(val));
        self
    }

    pub fn name(mut self, s: impl Into<String>) -> Self {
        assert!(self.res.is_none());
        self.res = Some(Expr::Name(s.into()));
        self
    }

    pub fn function_call(
        mut self,
        callee: impl FnOnce(ExprBuilder) -> ExprBuilder,
        args: impl FnOnce(ExprListBuilder) -> ExprListBuilder,
    ) -> Self {
        assert!(self.res.is_none());
        let func = callee(default()).build();
        let args = args(default()).build();
        self.res = Some(Expr::FunctionCall {
            func: Box::new(func),
            args,
        });
        self
    }

    pub fn access(
        mut self,
        lhs: impl FnOnce(ExprBuilder) -> ExprBuilder,
        field: impl Into<String>,
    ) -> Self {
        assert!(self.res.is_none());
        let expr = lhs(default()).build();
        self.res = Some(Expr::Access {
            val: Box::new(expr),
            field: field.into(),
        });
        self
    }

    pub fn tuple(mut self, vals: impl FnOnce(ExprListBuilder) -> ExprListBuilder) -> Self {
        assert!(self.res.is_none());
        self.res = Some(Expr::Tuple(vals(default()).build()));
        self
    }

    pub fn unit(mut self) -> Self {
        assert!(self.res.is_none());
        self.res = Some(Expr::Unit);
        self
    }

    pub fn sequence(mut self, exprs: impl FnOnce(ExprListBuilder) -> ExprListBuilder) -> Self {
        assert!(self.res.is_none());
        self.res = Some(Expr::Sequence(exprs(default()).build()));
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
        self.res = Some(Expr::Assignment {
            lval: Box::new(lhs),
            rval: Box::new(rhs),
        });
        self
    }

    pub fn lambda<P: Into<String> + Clone>(
        mut self,
        parameters: &[P],
        body: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        assert!(self.res.is_none());
        let body = body(default()).build();
        self.res = Some(Expr::Lambda {
            parameters: parameters
                .iter()
                .cloned()
                .map(|name| TypedName {
                    name: name.into(),
                    typ: Typ::ToInfere,
                })
                .collect(),
            body: Box::new(body),
        });
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

    pub fn add(mut self, f: impl FnOnce(ExprBuilder) -> ExprBuilder) -> Self {
        let expr = f(default()).build();
        self.res.push(expr);
        self
    }
}
