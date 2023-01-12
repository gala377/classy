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
pub enum DefinedType {
    Record(Record),
    ADT(ADT),
    Alias(Alias),
}

#[derive(Debug)]
pub struct Record {
    pub fields: Vec<TypedName>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ADT {
    pub discriminants: Vec<Discriminant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Discriminant {
    pub constructor: String,
    pub arguments: Vec<Typ>,
}

#[derive(Debug)]
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
    pub parameters: Vec<TypedName>,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Typ {
    Name(String),
    Array(Box<Typ>),
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

    IntConst(isize),
    StringConst(String),
    FloatConst(f64),
    Name(String),
}

/// Explicit PartialEq implementations to skip span comparison.
/// Only used in tests.
#[cfg(test)]
impl PartialEq for TypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        if self.name != other.name {
            return false;
        }
        match (&self.definition, &other.definition) {
            (DefinedType::Record(r1), DefinedType::Record(r2)) => r1.fields == r2.fields,
            (DefinedType::ADT(a1), DefinedType::ADT(a2)) => a1.discriminants == a2.discriminants,
            (DefinedType::Alias(a1), DefinedType::Alias(a2)) => a1.for_type == a2.for_type,
            _ => false,
        }
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

    pub fn func_def(
        mut self,
        name: impl Into<String>,
        parameters: impl FnOnce(TypedNameListBuilder) -> TypedNameListBuilder,
        body: impl FnOnce(ExprBuilder) -> ExprBuilder,
    ) -> Self {
        let parameters = parameters(default()).build();
        let body = body(default()).build();
        self.res
            .items
            .push(TopLevelItem::FunctionDefinition(FunctionDefinition {
                name: name.into(),
                parameters,
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
}

#[cfg(test)]
impl StructDefBuilder {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            fields: Vec::new(),
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

    pub fn build(self) -> TypeDefinition {
        TypeDefinition {
            name: self.name,
            definition: DefinedType::Record(Record {
                fields: self.fields,
            }),
            type_variables: Vec::new(),
            span: 0..0,
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
