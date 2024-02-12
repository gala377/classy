use std::{collections::HashMap, ops::Range};

pub mod fold;
pub mod visitor;

use classy_sexpr::ToSExpr;
use classy_sexpr_proc_macro::sexpr;
pub use fold::Folder;
pub use visitor::Visitor;

// cargo is actually wrong about this, it confuses
// usage of this function with unstable feature
// we did not enable
#[allow(dead_code)]
fn default<T: Default>() -> T {
    Default::default()
}

/// Represents an identifier in source
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    Unresolved {
        path: Vec<String>,
        identifier: String,
    },
    Global {
        package: usize,
        definition: usize,
    },
    Local(String),
}

impl Name {
    pub fn pretty(&self) -> String {
        format!("{self:?}")
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub namespace: Option<Namespace>,
    pub items: Vec<TopLevelItem>,
}

#[derive(Debug, Clone)]
pub struct TopLevelItem {
    pub id: usize,
    pub export: bool,
    pub kind: TopLevelItemKind,
}

#[derive(Debug, Clone)]
pub enum TopLevelItemKind {
    TypeDefinition(TypeDefinition),
    FunctionDefinition(FunctionDefinition),
    MethodsBlock(MethodsBlock<FunctionDefinition>),
    ConstDefinition(ConstDefinition),
    ClassDefinition(ClassDefinition),
    InstanceDefinition(InstanceDefinition),
    NameImport(Name),
    MethodsImport(Name),
    InstanceImport(Name),
}

/// instance $name? for $type_boound { $body }
#[derive(Debug, Clone)]
pub struct InstanceDefinition {
    pub name: Option<String>,
    pub free_variables: Vec<String>,
    pub bounds: Vec<TypeBound>,
    pub instanced_class: TypeBound,
    pub body: Vec<InstanceDefinitionItem>,
}

#[derive(Debug, Clone)]
pub enum InstanceDefinitionItem {
    FunctionDefinition(FunctionDefinition),
    MethodsBlock(MethodsBlock<FunctionDefinition>),
}

/// class $bounds? => $name($args) { $body }
#[derive(Clone, Debug)]
pub struct ClassDefinition {
    pub name: String,
    pub bounds: Vec<TypeBound>,
    /// Always generic types
    pub args: Vec<String>,
    pub body: Vec<ClassDefinitionItem>,
}

#[derive(Clone, Debug)]
pub enum ClassDefinitionItem {
    MethodBlock(MethodsBlock<FuncDecl>),
    Function(FuncDecl),
}

#[derive(Clone, Debug)]
pub struct FuncDecl {
    pub name: String,
    pub typ: Typ,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub name: Name,
}

impl Namespace {
    pub fn joined_with(&self, name: &str) -> String {
        let mut path = match &self.name {
            Name::Unresolved { path, identifier } => {
                let mut path = path.clone();
                path.push(identifier.clone());
                path
            }
            _ => panic!("cannot join namespace with non-unresolved name"),
        };
        path.push(name.into());
        path.join("::")
    }

    pub fn joined(&self) -> String {
        match &self.name {
            Name::Unresolved { path, identifier } if path.is_empty() => identifier.clone(),
            Name::Unresolved { path, identifier } => format!("{}::{}", path.join("::"), identifier),
            _ => panic!("cannot join namespace with non-unresolved name"),
        }
    }

    pub fn as_segments(&self) -> Vec<String> {
        match &self.name {
            Name::Unresolved { path, identifier } => {
                let mut path = path.clone();
                path.push(identifier.clone());
                path
            }
            _ => panic!("namespace name should stay unresolved"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstDefinition {
    pub name: String,
    pub typ: Typ,
    pub init: Expr,
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: String,
    pub definition: DefinedType,
    pub type_variables: Vec<String>,
    pub constraints: Vec<TypeBound>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum DefinedType {
    Record(Record),
    ADT(ADT),
    Alias(Alias),
}

#[derive(Debug, Clone)]
pub struct Record {
    pub fields: Vec<TypedIdentifier>,
}

#[derive(Debug, Clone)]
pub struct ADT {
    pub discriminants: Vec<Discriminant>,
}

#[derive(Debug, Clone)]
pub struct Discriminant {
    pub constructor: String,
    pub arguments: DiscriminantKind,
}

#[derive(Debug, Clone)]
pub enum DiscriminantKind {
    Empty,
    Tuple(Vec<Typ>),
    Record(Vec<(String, Typ)>),
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub for_type: Typ,
}

#[derive(Debug, Clone)]
pub struct TypedIdentifier {
    pub name: String,
    pub typ: Typ,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub typ: Typ,
    pub parameters: Vec<String>,
    pub body: Expr,
    pub attributes: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MethodsBlock<T: std::fmt::Debug + Clone> {
    pub name: Option<String>,
    pub typ: Typ,
    pub methods: Vec<Method<T>>,
}

#[derive(Debug, Clone)]
pub struct Method<T: Clone + std::fmt::Debug> {
    pub id: usize,
    pub item: T,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Typ {
    Unit,
    Name(Name),
    Application {
        callee: Box<Typ>,
        args: Vec<Typ>,
    },
    Array(Box<Typ>),
    Function {
        args: Vec<Typ>,
        ret: Box<Typ>,
    },
    Tuple(Vec<Typ>),
    Poly {
        free_variables: Vec<String>,
        bounds: Vec<TypeBound>,
        typ: Box<Typ>,
    },
    ToInfere,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypeVariable {
    pub name: String,
    // todo: for the future
    // pub constraints: Vec<Contraint>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: usize,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
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
    Name(Name),
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
        parameters: Vec<TypedIdentifier>,
        body: Box<Expr>,
    },
    TypedExpr {
        expr: Box<Expr>,
        typ: Typ,
    },
    StructLiteral {
        strct: Name,
        values: HashMap<String, Expr>,
    },
    AdtTupleConstructor {
        typ: Name,
        constructor: String,
        args: Vec<Expr>,
    },
    AdtStructConstructor {
        typ: Name,
        constructor: String,
        fields: Vec<(String, Expr)>,
    },
    AdtUnitConstructor {
        typ: Name,
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
    Var(String),
    Tuple(Vec<Pattern>),
    Struct {
        strct: Name,
        fields: HashMap<String, Pattern>,
    },
    TupleStruct {
        strct: Name,
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
    TypeSpecifier(Name, Box<Pattern>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeBound {
    pub head: Name,
    pub args: Vec<Typ>,
}

impl Expr {
    pub fn pretty(&self) -> String {
        match &self.kind {
            ExprKind::Unit => "()".to_string(),
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
            ExprKind::Name(n) => n.pretty(),
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
                for TypedIdentifier { name, .. } in parameters {
                    res.push_str(&name);
                }
                res.push_str(") ");
                res.push_str(&body.pretty());
                res
            }
            ExprKind::TypedExpr { expr, .. } => expr.pretty(),
            ExprKind::StructLiteral { strct, values } => {
                let mut res = strct.pretty();
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
                let mut res = format!("{}.{}", typ.pretty(), constructor);
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
                let mut res = format!("{}.{}", typ.pretty(), constructor);
                res.push_str("{ ");
                for (name, val) in fields {
                    res.push_str(&format!("{} = {}, ", name, val.pretty()));
                }
                res.push_str("}");
                res
            }
            ExprKind::AdtUnitConstructor { typ, constructor } => {
                format!("{}.{}", typ.pretty(), constructor)
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
                ..
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

impl classy_sexpr::ToSExpr for SourceFile {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self.namespace {
            Some(namespace) => {
                let items = self.items;
                sexpr!(($namespace @items))
            }
            None => sexpr!(${self.items}),
        }
    }
}

impl classy_sexpr::ToSExpr for TopLevelItem {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        if self.export {
            sexpr!((export ${self.kind}))
        } else {
            sexpr!(${self.kind})
        }
    }
}

impl classy_sexpr::ToSExpr for TopLevelItemKind {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            TopLevelItemKind::TypeDefinition(def) => sexpr!($def),
            TopLevelItemKind::FunctionDefinition(def) => sexpr!($def),
            TopLevelItemKind::MethodsBlock(def) => sexpr!($def),
            TopLevelItemKind::ClassDefinition(def) => sexpr!($def),
            TopLevelItemKind::ConstDefinition(def) => sexpr!($def),
            TopLevelItemKind::InstanceDefinition(def) => sexpr!($def),
            TopLevelItemKind::NameImport(name) => sexpr!((import $name)),
            TopLevelItemKind::InstanceImport(name) => sexpr!((import instance $name)),
            TopLevelItemKind::MethodsImport(name) => sexpr!((import methods $name)),
        }
    }
}

impl ToSExpr for InstanceDefinition {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let InstanceDefinition {
            name,
            free_variables,
            bounds,
            instanced_class,
            body,
        } = self;
        let name = name.map(|x| sexpr!(#x));
        sexpr!((instance @name for $free_variables $bounds $instanced_class $body))
    }
}

impl ToSExpr for InstanceDefinitionItem {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            InstanceDefinitionItem::FunctionDefinition(f) => sexpr!($f),
            InstanceDefinitionItem::MethodsBlock(m) => sexpr!($m),
        }
    }
}

impl ToSExpr for Namespace {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!((namespace ${self.name}))
    }
}

impl classy_sexpr::ToSExpr for FunctionDefinition {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let FunctionDefinition {
            name,
            parameters,
            typ,
            body,
            attributes,
        } = self;
        let parameters = parameters
            .into_iter()
            .map(|name| sexpr!(#name))
            .collect::<Vec<_>>();
        sexpr!(
            (fn
                $attributes
                (type $typ)
                #name $parameters $body)
        )
    }
}

impl classy_sexpr::ToSExpr for ConstDefinition {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let ConstDefinition {
            name, typ, init, ..
        } = self;
        sexpr!(
            (const #name $typ $init)
        )
    }
}

impl classy_sexpr::ToSExpr for TypeDefinition {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let TypeDefinition {
            name,
            definition,
            type_variables,
            ..
        } = self;
        sexpr!(
            (type #name $type_variables $definition)
        )
    }
}

impl classy_sexpr::ToSExpr for TypeVariable {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let name = self.name;
        sexpr!(#name)
    }
}

impl classy_sexpr::ToSExpr for DefinedType {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            DefinedType::Record(r) => sexpr!($r),
            DefinedType::ADT(a) => sexpr!($a),
            DefinedType::Alias(a) => sexpr!($a),
        }
    }
}

impl classy_sexpr::ToSExpr for Record {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!(
            (record @{self.fields})
        )
    }
}

impl classy_sexpr::ToSExpr for ADT {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!((
            adt
            @{self.discriminants}
        ))
    }
}

impl classy_sexpr::ToSExpr for Alias {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!((alias ${self.for_type}))
    }
}

impl classy_sexpr::ToSExpr for Discriminant {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let cons = self.constructor;
        sexpr!((
            #cons
            ${self.arguments}
        ))
    }
}

impl classy_sexpr::ToSExpr for DiscriminantKind {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            DiscriminantKind::Empty => sexpr!(unit),
            DiscriminantKind::Tuple(inner) => sexpr!((tuple @ inner)),
            DiscriminantKind::Record(fields) => sexpr!((record @ fields)),
        }
    }
}

impl classy_sexpr::ToSExpr for TypedIdentifier {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let name = self.name;
        sexpr!((#name ${self.typ}))
    }
}
impl classy_sexpr::ToSExpr for Typ {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            Typ::Unit => sexpr!(unit),
            Typ::Name(n) => sexpr!($n),
            Typ::Application { callee, args } => sexpr!(($callee $args)),
            Typ::Array(inner) => sexpr!((array $inner)),
            Typ::Function { args, ret } => sexpr!((fn $args $ret)),
            Typ::Tuple(inner) => sexpr!((tuple @ inner)),
            Typ::Poly {
                free_variables,
                bounds,
                typ,
            } => sexpr!((poly $free_variables $bounds $typ)),
            Typ::ToInfere => sexpr!(infere),
        }
    }
}

impl classy_sexpr::ToSExpr for Expr {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!(${self.kind})
    }
}

impl classy_sexpr::ToSExpr for TypeBound {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!((${self.head} @{self.args}))
    }
}

impl classy_sexpr::ToSExpr for ExprKind {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            ExprKind::Unit => sexpr!(()),
            ExprKind::Sequence(s) => sexpr!($s),
            ExprKind::Assignment { lval, rval } => sexpr!((assign $lval $rval)),
            ExprKind::IntConst(i) => sexpr!($i),
            ExprKind::StringConst(s) => sexpr!($s),
            ExprKind::FloatConst(f) => {
                let f = f.to_string();
                sexpr!((float $f))
            }
            ExprKind::BoolConst(b) => sexpr!($b),
            ExprKind::Name(n) => sexpr!($n),
            ExprKind::FunctionCall { func, args, kwargs } => sexpr!((call $func $args $kwargs)),
            ExprKind::Access { val, field } => sexpr!((access $val #field)),
            ExprKind::Tuple(t) => sexpr!((tuple @ t)),
            ExprKind::Lambda { parameters, body } => sexpr!((lambda $parameters $body)),
            ExprKind::TypedExpr { expr, typ } => sexpr!((typed $expr $typ)),
            ExprKind::StructLiteral { strct, values } => sexpr!((struct $strct $values)),
            ExprKind::AdtTupleConstructor {
                typ,
                constructor,
                args,
            } => sexpr!((adt $typ #constructor $args)),
            ExprKind::AdtStructConstructor {
                typ,
                constructor,
                fields,
            } => sexpr!((adt $typ #constructor $fields)),
            ExprKind::AdtUnitConstructor { typ, constructor } => {
                sexpr!((adt $typ $constructor))
            }
            ExprKind::While { cond, body } => sexpr!((while $cond $body)),
            ExprKind::Return(r) => sexpr!((return $r)),
            ExprKind::If {
                cond,
                body,
                else_body,
            } => sexpr!((if $cond $body @else_body)),
            ExprKind::Let { name, typ, init } => sexpr!((let #name $typ $init)),
            ExprKind::LetRec { definitions } => sexpr!((letrec @ definitions)),
            ExprKind::AnonType { fields } => sexpr!((type @ fields)),
            ExprKind::ArrayLiteral { typ, size, init } => sexpr!((array $typ $size $init)),
            ExprKind::IndexAccess { lhs, index } => sexpr!((index $lhs $index)),
            ExprKind::Match { expr, cases } => {
                let cases = cases
                    .into_iter()
                    .map(|(pat, body, guard)| match guard {
                        Some(g) => sexpr!(($pat $g $body)),
                        None => sexpr!(($pat $body)),
                    })
                    .collect::<Vec<_>>();
                sexpr!((match $expr $cases))
            }
            ExprKind::MethodCall {
                receiver,
                method,
                args,
                kwargs,
                ..
            } => sexpr!((method $receiver #method $args $kwargs)),
        }
    }
}

impl classy_sexpr::ToSExpr for Pattern {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!(${self.kind})
    }
}

impl classy_sexpr::ToSExpr for PatternKind {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            PatternKind::Var(n) => sexpr!(#n),
            PatternKind::Tuple(t) => sexpr!((tuple @ t)),
            PatternKind::Struct { strct, fields } => sexpr!((struct $strct $fields)),
            PatternKind::TupleStruct { strct, fields } => sexpr!((struct $strct $fields)),
            PatternKind::AnonStruct { fields } => sexpr!((struct @ fields)),
            PatternKind::Array(inner) => sexpr!((array $inner)),
            PatternKind::Wildcard => sexpr!(_),
            PatternKind::Unit => sexpr!(()),
            PatternKind::String(s) => sexpr!($s),
            PatternKind::Int(i) => sexpr!($i),
            PatternKind::Bool(b) => sexpr!($b),
            PatternKind::Rest(s) => sexpr!((rest #s)),
            PatternKind::TypeSpecifier(name, rest) => sexpr!(($name $rest)),
        }
    }
}

impl ToSExpr for Name {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            Name::Unresolved { path, identifier } => {
                if path.is_empty() {
                    return sexpr!(#identifier);
                }
                let id = path.join("::").to_string() + "::" + &identifier;
                sexpr!(#id)
            }
            Name::Global {
                package,
                definition,
            } => sexpr!((global $package $definition)),
            Name::Local(name) => sexpr!(#name),
        }
    }
}

impl<T: std::fmt::Debug + Clone + ToSExpr> ToSExpr for MethodsBlock<T> {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let MethodsBlock { name, typ, methods } = self;
        let name = name.map(|n| sexpr!(#n));
        sexpr!((methods @name $typ $methods))
    }
}

impl<T: std::fmt::Debug + Clone + ToSExpr> ToSExpr for Method<T> {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        sexpr!(${self.item})
    }
}

impl ToSExpr for ClassDefinition {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let ClassDefinition {
            name,
            bounds,
            args,
            body,
            ..
        } = self;
        let args = args.into_iter().map(|s| sexpr!(#s)).collect::<Vec<_>>();
        sexpr!((class #name @args $bounds $body))
    }
}

impl ToSExpr for ClassDefinitionItem {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            ClassDefinitionItem::MethodBlock(b) => sexpr!($b),
            ClassDefinitionItem::Function(f) => sexpr!($f),
        }
    }
}

impl ToSExpr for FuncDecl {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        let FuncDecl { name, typ } = self;
        sexpr!((fn #name $typ))
    }
}
