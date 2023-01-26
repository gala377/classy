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
    Block(Vec<Expr>),
    Assignment { lval: Box<Expr>, rval: Box<Expr> },
    IntConst(isize),
    StringConst(String),
    FloatConst(f64),
    Name(String),
    FunctionCall { func: Box<Expr>, args: Vec<Expr> },
    Access { val: Box<Expr>, field: String },
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
        self.res = Some(Expr::IntConst(val));
        self
    }

    pub fn build(self) -> Expr {
        self.res.unwrap()
    }
}
