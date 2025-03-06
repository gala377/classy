//! Resolved AST. Simplified ast that has all the calls
//! names and expr types resolved.

use std::collections::HashMap;

use classy_sexpr::{SExpr, ToSExpr};
use classy_sexpr_proc_macro::sexpr;
use classy_syntax::ast::{self, transformer::AstExprTransformer};

use crate::{
    scope::{FlatScope, FlatScopeExt},
    typecheck::ast_to_type::PrefexScope,
    v2::knowledge::{InstanceInfo, MethodBlockInfo},
};

use super::{
    constraint_generation::ExprId,
    constraint_solver::CallResolution,
    knowledge::{
        Database, DefinitionId, DefinitionKind, GenericConstraint, Id, LocalId, PackageId, TypeId,
        CURRENT_PACKAGE_ID,
    },
    ty::Type,
};

#[derive(Clone, Debug)]
pub struct RastTree {
    pub bodies: HashMap<LocalId<DefinitionId>, Expr>,
}

#[derive(Clone, Debug)]
pub enum ResolvedName {
    Local(String),
    Global(Id<DefinitionId>),
}

pub enum ResolvedNameOrThis {
    Resolved(ResolvedName),
    This,
}

#[derive(Clone, Debug)]
pub enum AccessResolution {
    Field(String),
    Method(Id<DefinitionId>),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
    pub id: ExprId,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    This,
    Unit,
    Sequence(Vec<Expr>),
    Assignment {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    IntConst(isize),
    StringConst(String),
    FloatConst(f64),
    BoolConst(bool),
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        args: Vec<Expr>,
        implicit_args: Vec<Expr>,
        resolution: CallResolution,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        implicit_args: Vec<Expr>,
        resolution: CallResolution,
    },
    Name {
        symbol: String,
        resolved: ResolvedName,
    },
    Access {
        val: Box<Expr>,
        field: String,
        access_resolution: AccessResolution,
    },
    Tuple(Vec<Expr>),
    ArrayLiteral {
        size: Box<Expr>,
        init: Vec<Expr>,
    },
    IndexAccess {
        lhs: Box<Expr>,
        index: Box<Expr>,
    },
    Let {
        name: String,
        init: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_body: Option<Box<Expr>>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    Lambda {
        args: Vec<(String, Type)>,
        body: Box<Expr>,
    },
    StructLiteral {
        def: Id<DefinitionId>,
        fields: Vec<(String, Expr)>,
    },
    AdtUnitConstructor {
        def: Id<DefinitionId>,
        case: String,
    },
    AdtTupleConstructor {
        def: Id<DefinitionId>,
        case: String,
        fields: Vec<Expr>,
    },
    AdtStructConstructor {
        def: Id<DefinitionId>,
        case: String,
        fields: Vec<(String, Expr)>,
    },
    MakeInstance {
        def: Id<DefinitionId>,
        args: Vec<Expr>,
    },
    Return {
        expr: Box<Expr>,
    }, // todo! and so on
}

pub fn build_rast(
    database: &Database,
    expr_types: &HashMap<ExprId, Type>,
    call_resolutions: &HashMap<usize, CallResolution>,
    name_resolutions: &HashMap<ExprId, ast::Name>,
) -> RastTree {
    let all_functions = database
        .function_definitions
        .iter()
        .map(|(id, def)| (id.clone(), def.body.clone()))
        .chain(
            database
                .method_definitions
                .iter()
                .map(|(id, def)| (id.clone(), def.body.clone())),
        )
        .filter(|(def, _)| {
            database
                .definitions
                .get(def)
                .is_some_and(|d| !d.annotations.contains("runtime"))
        })
        .collect::<Vec<_>>();

    let mut bodies = HashMap::new();

    let call_resolutions = call_resolutions
        .iter()
        .map(|(id, res)| (ExprId(*id), res.clone()))
        .collect::<HashMap<_, _>>();
    for (id, body) in all_functions.into_iter() {
        let is_method = matches!(
            database.get_definition_map(id.as_global(CURRENT_PACKAGE_ID), |def| def.kind.clone()),
            Some(DefinitionKind::Method(_))
        );
        let as_global = id.as_global(CURRENT_PACKAGE_ID);
        let mut constraints = FlatScope::new();
        let mut prefex_scope = PrefexScope::without_scope();
        let this_type = gather(database, as_global, &mut constraints, &mut prefex_scope);
        // resolve type of this
        let mut builder = RastBuilder {
            database,
            expr_types,
            call_resolutions: &call_resolutions,
            name_resolutions,
            is_method,
            this_type,
        };
        println!("Building RAST for {:?}, body is {:#?}", id, body);
        let body = builder.transform_expr(body);
        bodies.insert(id.clone(), body);
    }
    RastTree { bodies }
}

pub struct RastBuilder<'database, 'expr_types, 'call_resolutions, 'name_resolutions> {
    database: &'database Database,
    expr_types: &'expr_types HashMap<ExprId, Type>,
    call_resolutions: &'call_resolutions HashMap<ExprId, CallResolution>,
    name_resolutions: &'name_resolutions HashMap<ExprId, ast::Name>,
    is_method: bool,
    this_type: Option<Id<TypeId>>,
}

impl RastBuilder<'_, '_, '_, '_> {
    fn receiver_type(&self) -> Option<Type> {
        self.this_type
            .map(|t| self.database.resolve_alias_to_type(t).unwrap())
    }

    fn resolve_name(&self, name: &ast::Name, id: usize) -> ResolvedNameOrThis {
        match name {
            ast::Name::Local(name) if name == "this" && self.this_type.is_some() => {
                ResolvedNameOrThis::This
            }
            ast::Name::Local(name) => {
                ResolvedNameOrThis::Resolved(ResolvedName::Local(name.clone()))
            }
            ast::Name::Global {
                package,
                definition,
            } => {
                let package = PackageId(*package);
                let id = Id {
                    package,
                    id: DefinitionId(*definition),
                };
                ResolvedNameOrThis::Resolved(ResolvedName::Global(id))
            }
            ast::Name::Unresolved { path, identifier } => {
                if path.is_empty() && identifier == "this" && self.this_type.is_some() {
                    return ResolvedNameOrThis::This;
                }
                match self.name_resolutions.get(&ExprId(id)) {
                    Some(ast::Name::Local(name)) => {
                        ResolvedNameOrThis::Resolved(ResolvedName::Local(name.clone()))
                    }
                    Some(ast::Name::Global {
                        package,
                        definition,
                    }) => ResolvedNameOrThis::Resolved(ResolvedName::Global(Id {
                        package: PackageId(*package),
                        id: DefinitionId(*definition),
                    })),
                    _ => {
                        panic!("After all could not resolve name {path:?}::{identifier}")
                    }
                }
            }
        }
    }
}

impl AstExprTransformer for RastBuilder<'_, '_, '_, '_> {
    type Expr = Expr;
    type ExprKind = ExprKind;
    type Pattern = ();
    type PatternKind = ();

    fn transform_expr(&mut self, ast::Expr { id, kind }: ast::Expr) -> Self::Expr {
        let typ = self.expr_types.get(&ExprId(id)).unwrap().clone();
        let kind = match kind {
            ast::ExprKind::MethodCall {
                receiver,
                method,
                args,
                kwargs,
            } => {
                let ExprKind::MethodCall {
                    receiver,
                    method,
                    args,
                    ..
                } = self.transform_method_call(*receiver, method, args, kwargs)
                else {
                    panic!("Method call resolution failed")
                };
                let resolution = self.call_resolutions.get(&ExprId(id)).unwrap().clone();
                assert!(matches!(
                    resolution,
                    CallResolution::StaticMethod(_) | CallResolution::FromInstanceInScope { .. }
                ));
                ExprKind::MethodCall {
                    receiver,
                    method,
                    args,
                    // todo: get from call resolution using MakeInstance nodes
                    implicit_args: Vec::new(),
                    resolution,
                }
            }
            ast::ExprKind::FunctionCall { func, args, kwargs } => {
                let resolution = self.call_resolutions.get(&ExprId(id)).clone();
                match resolution {
                    // If its a method call we need to call transform_method passing this as
                    // the receiver
                    // Otherwise its just a function call. It does not need to be static btw.
                    // Could be just a name. In this case idk, i guess call resolution is empty.
                    Some(CallResolution::StaticMethod(resolution)) => {
                        let receiver_t = self.receiver_type().unwrap();
                        let receiver = Expr {
                            kind: ExprKind::This,
                            ty: receiver_t,
                            // dummy id, hope its not used anywhere
                            id: ExprId(usize::MAX),
                        };
                        let method = self
                            .database
                            .get_definition_map(*resolution, |def| def.name.clone())
                            .unwrap();
                        ExprKind::MethodCall {
                            receiver: Box::new(receiver),
                            method,
                            args: args
                                .into_iter()
                                .map(|expr| self.transform_expr(expr))
                                .collect(),
                            implicit_args: Vec::new(),
                            resolution: CallResolution::StaticMethod(*resolution),
                        }
                    }
                    Some(CallResolution::FromInstanceInScope {
                        instance_index,
                        method_def,
                    }) => {
                        let receiver_t = self.receiver_type().unwrap();
                        let receiver = Expr {
                            kind: ExprKind::This,
                            ty: receiver_t,
                            // dummy id, hope its not used anywhere
                            id: ExprId(usize::MAX),
                        };
                        let method = self
                            .database
                            .get_definition_map(*method_def, |def| def.name.clone())
                            .unwrap();
                        ExprKind::MethodCall {
                            receiver: Box::new(receiver),
                            method,
                            args: args
                                .into_iter()
                                .map(|expr| self.transform_expr(expr))
                                .collect(),
                            implicit_args: Vec::new(),
                            resolution: CallResolution::FromInstanceInScope {
                                instance_index: *instance_index,
                                method_def: *method_def,
                            },
                        }
                    }
                    Some(CallResolution::StaticFunction(id)) => {
                        let ExprKind::Call {
                            callee,
                            args,
                            implicit_args,
                            ..
                        } = self.transform_function_call(*func, args, kwargs)
                        else {
                            unreachable!("not possible");
                        };
                        ExprKind::Call {
                            callee,
                            args,
                            implicit_args,
                            resolution: CallResolution::StaticFunction(*id),
                        }
                    }
                    Some(CallResolution::Unresolved) => {
                        panic!(
                            "Call is unresolved. This should not happen. If anything the \
                             resolution should be None for function calls"
                        );
                    }
                    None => match self.name_resolutions.get(&ExprId(func.id)) {
                        // Possibly just a name that has been resolved
                        Some(name) => {
                            if let ResolvedNameOrThis::Resolved(ResolvedName::Global(id)) =
                                self.resolve_name(name, func.id)
                            {
                                let ExprKind::Call {
                                    callee,
                                    args,
                                    implicit_args,
                                    ..
                                } = self.transform_function_call(*func, args, kwargs)
                                else {
                                    unreachable!("not possible");
                                };
                                ExprKind::Call {
                                    callee,
                                    args,
                                    implicit_args,
                                    resolution: CallResolution::StaticFunction(id),
                                }
                            } else {
                                self.transform_function_call(*func, args, kwargs)
                            }
                        }
                        None => {
                            // in this case the lhs is an expression that does not have resolution
                            self.transform_function_call(*func, args, kwargs)
                        }
                    },
                }
            }
            ast::ExprKind::StructLiteral { strct, values } => {
                let resolved = match strct {
                    ast::Name::Global {
                        package,
                        definition,
                    } => Id {
                        package: PackageId(package),
                        id: DefinitionId(definition),
                    },
                    ast::Name::Local(_) => panic!("Struct name should never be local"),
                    ast::Name::Unresolved { .. } => match self.name_resolutions.get(&ExprId(id)) {
                        Some(ast::Name::Global {
                            package,
                            definition,
                        }) => Id {
                            package: PackageId(*package),
                            id: DefinitionId(*definition),
                        },
                        other => {
                            panic!("Unexpected invalid struct name {other:?}")
                        }
                    },
                };
                ExprKind::StructLiteral {
                    def: resolved,
                    fields: values
                        .into_iter()
                        .map(|(key, value)| (key, self.transform_expr(value)))
                        .collect(),
                }
            }
            ast::ExprKind::Name(name) => {
                let resolved = match self.resolve_name(&name, id) {
                    ResolvedNameOrThis::Resolved(resolved) => resolved,
                    ResolvedNameOrThis::This => {
                        return Expr {
                            id: ExprId(id),
                            kind: ExprKind::This,
                            ty: self.receiver_type().unwrap(),
                        };
                    }
                };
                let symbol = match &resolved {
                    ResolvedName::Local(name) => name.clone(),
                    ResolvedName::Global(id) => self
                        .database
                        .get_definition_map(*id, |def| def.name.clone())
                        .unwrap(),
                };
                ExprKind::Name { symbol, resolved }
            }
            k => self.transform_expr_kind(k),
        };
        Expr {
            id: ExprId(id),
            kind,
            ty: typ,
        }
    }

    fn transform_unit(&mut self) -> Self::ExprKind {
        ExprKind::Unit
    }
    fn transform_int_const(&mut self, val: isize) -> Self::ExprKind {
        ExprKind::IntConst(val)
    }

    fn transform_string_const(&mut self, val: String) -> Self::ExprKind {
        ExprKind::StringConst(val)
    }

    fn transform_float_const(&mut self, val: f64) -> Self::ExprKind {
        ExprKind::FloatConst(val)
    }

    fn transform_name(&mut self, _name: ast::Name) -> Self::ExprKind {
        // let (resolved, symbol) = match name {
        //     ast::Name::Local(name) if name == "this" && self.is_method => {
        //         return ExprKind::This;
        //     }
        //     ast::Name::Local(name) => (ResolvedName::Local(name.clone()), name),
        //     ast::Name::Global {
        //         package,
        //         definition,
        //     } => {
        //         let package = PackageId(package);
        //         let id = Id {
        //             package,
        //             id: DefinitionId(definition),
        //         };
        //         let name = self.database.get_definition_map(id, |def|
        // def.name.clone());         (ResolvedName::Global(id), name.unwrap())
        //     }
        //     ast::Name::Unresolved { path, identifier } => {
        //         // We should have id here, and get trivial resolutions
        //         panic!("Unresolved name: {path:?}::{identifier:?}")
        //     }
        // };
        // ExprKind::Name { symbol, resolved }
        unreachable!("Resolved at transform_expression")
    }

    fn transform_sequence(&mut self, seq: Vec<ast::Expr>) -> Self::ExprKind {
        ExprKind::Sequence(
            seq.into_iter()
                .map(|expr| self.transform_expr(expr))
                .collect(),
        )
    }

    fn transform_assignment(&mut self, lval: ast::Expr, rval: ast::Expr) -> Self::ExprKind {
        ExprKind::Assignment {
            lhs: Box::new(self.transform_expr(lval)),
            rhs: Box::new(self.transform_expr(rval)),
        }
    }

    fn transform_access(&mut self, val: ast::Expr, field: String) -> Self::ExprKind {
        todo!("not yet there")
    }

    fn transform_tuple(&mut self, fields: Vec<ast::Expr>) -> Self::ExprKind {
        ExprKind::Tuple(fields.into_iter().map(|e| self.transform_expr(e)).collect())
    }

    fn transform_lambda(
        &mut self,
        _params: Vec<ast::TypedIdentifier>,
        _body: ast::Expr,
    ) -> Self::ExprKind {
        todo!("no lambda support yet")
    }

    fn transform_while(&mut self, cond: ast::Expr, body: ast::Expr) -> Self::ExprKind {
        ExprKind::While {
            cond: Box::new(self.transform_expr(cond)),
            body: Box::new(self.transform_expr(body)),
        }
    }

    fn transform_return(&mut self, expr: ast::Expr) -> Self::ExprKind {
        ExprKind::Return {
            expr: Box::new(self.transform_expr(expr)),
        }
    }

    fn transform_if(
        &mut self,
        cond: ast::Expr,
        body: ast::Expr,
        else_body: Option<ast::Expr>,
    ) -> Self::ExprKind {
        ExprKind::If {
            cond: Box::new(self.transform_expr(cond)),
            then: Box::new(self.transform_expr(body)),
            else_body: else_body.map(|expr| Box::new(self.transform_expr(expr))),
        }
    }

    fn transform_let(&mut self, name: String, _typ: ast::Typ, init: ast::Expr) -> Self::ExprKind {
        ExprKind::Let {
            name,
            init: Box::new(self.transform_expr(init)),
        }
    }

    fn transform_typed_expr(&mut self, expr: ast::Expr, typ: ast::Typ) -> Self::ExprKind {
        todo!("not supporter yet")
    }

    fn transform_struct_literal(
        &mut self,
        _strct: ast::Name,
        _values: HashMap<String, ast::Expr>,
    ) -> Self::ExprKind {
        // let resolved = match strct {
        //     ast::Name::Global {
        //         package,
        //         definition,
        //     } => Id {
        //         package: PackageId(package),
        //         id: DefinitionId(definition),
        //     },
        //     ast::Name::Local(_) => panic!("Struct name should never be local"),
        //     ast::Name::Unresolved { path, identifier } => {
        //         panic!("Unresolved name: {path:?}::{identifier:?}")
        //     }
        // };
        // ExprKind::StructLiteral {
        //     def: resolved,
        //     fields: values
        //         .into_iter()
        //         .map(|(key, value)| (key, self.transform_expr(value)))
        //         .collect(),
        // }
        unreachable!("Resolved ad transform_expr")
    }

    fn transform_bool_const(&mut self, val: bool) -> Self::ExprKind {
        ExprKind::BoolConst(val)
    }

    fn transform_array_literal(
        &mut self,
        _typ: ast::Typ,
        size: ast::Expr,
        init: Vec<ast::Expr>,
    ) -> Self::ExprKind {
        ExprKind::ArrayLiteral {
            size: Box::new(self.transform_expr(size)),
            init: init
                .into_iter()
                .map(|expr| self.transform_expr(expr))
                .collect(),
        }
    }

    fn transform_index_access(&mut self, lhs: ast::Expr, index: ast::Expr) -> Self::ExprKind {
        ExprKind::IndexAccess {
            lhs: Box::new(self.transform_expr(lhs)),
            index: Box::new(self.transform_expr(index)),
        }
    }

    fn transform_method_call(
        &mut self,
        receiver: ast::Expr,
        method: String,
        args: Vec<ast::Expr>,
        _kwargs: HashMap<String, ast::Expr>,
    ) -> Self::ExprKind {
        ExprKind::MethodCall {
            receiver: Box::new(self.transform_expr(receiver)),
            method,
            args: args
                .into_iter()
                .map(|expr| self.transform_expr(expr))
                .collect(),
            implicit_args: Vec::new(),
            resolution: CallResolution::Unresolved,
        }
    }

    fn transform_function_call(
        &mut self,
        func: ast::Expr,
        args: Vec<ast::Expr>,
        _kwargs: HashMap<String, ast::Expr>,
    ) -> Self::ExprKind {
        ExprKind::Call {
            callee: Box::new(self.transform_expr(func)),
            args: args
                .into_iter()
                .map(|expr| self.transform_expr(expr))
                .collect(),
            implicit_args: Vec::new(),
            resolution: CallResolution::Unresolved,
        }
    }

    fn transform_adt_struct_constructor(
        &mut self,
        name: ast::Name,
        case: String,
        fields: Vec<(String, ast::Expr)>,
    ) -> Self::ExprKind {
        todo!()
    }

    fn transform_adt_tuple_constructor(
        &mut self,
        name: ast::Name,
        case: String,
        fields: Vec<ast::Expr>,
    ) -> Self::ExprKind {
        todo!()
    }

    fn transform_adt_unit_constructor(&mut self, name: ast::Name, case: String) -> Self::ExprKind {
        todo!()
    }

    fn transform_match(
        &mut self,
        expr: ast::Expr,
        cases: Vec<(ast::Pattern, ast::Expr, Option<Box<ast::Expr>>)>,
    ) -> Self::ExprKind {
        todo!()
    }

    fn transform_pattern(&mut self, pat: ast::Pattern) -> Self::Pattern {
        todo!()
    }

    fn transform_var_pattern(&mut self, name: String) -> Self::PatternKind {
        todo!()
    }

    fn transform_tuple_pattern(&mut self, fields: Vec<ast::Pattern>) -> Self::PatternKind {
        todo!()
    }

    fn transform_array_pattern(&mut self, fields: Vec<ast::Pattern>) -> Self::PatternKind {
        todo!()
    }

    fn transform_struct_pattern(
        &mut self,
        strct: ast::Name,
        fields: HashMap<String, ast::Pattern>,
    ) -> Self::PatternKind {
        todo!()
    }

    fn transform_tuple_struct_pattern(
        &mut self,
        strct: ast::Name,
        fields: Vec<ast::Pattern>,
    ) -> Self::PatternKind {
        todo!()
    }

    fn transform_rest_pattern(&mut self, name: String) -> Self::PatternKind {
        todo!()
    }

    fn transform_type_specified_pattern(
        &mut self,
        name: ast::Name,
        pat: ast::Pattern,
    ) -> Self::PatternKind {
        todo!()
    }

    fn transform_anon_struct_pattern(
        &mut self,
        fields: HashMap<String, ast::Pattern>,
    ) -> Self::PatternKind {
        todo!()
    }

    fn transform_wildcard_pattern(&mut self) -> Self::PatternKind {
        todo!()
    }

    fn transform_unit_pattern(&mut self) -> Self::PatternKind {
        todo!()
    }

    fn transform_string_pattern(&mut self, val: String) -> Self::PatternKind {
        todo!()
    }

    fn transform_int_pattern(&mut self, val: isize) -> Self::PatternKind {
        todo!()
    }

    fn transform_bool_pattern(&mut self, val: bool) -> Self::PatternKind {
        todo!()
    }
}

impl ToSExpr for LocalId<DefinitionId> {
    fn to_sexpr(self) -> SExpr {
        let LocalId(def) = self;
        let def = def.0;
        sexpr!((local id #def))
    }
}

impl ToSExpr for RastTree {
    fn to_sexpr(self) -> SExpr {
        let bodies: Vec<_> = self
            .bodies
            .into_iter()
            .map(|(id, expr)| {
                sexpr!(
                    (definition[$id] &n $expr)
                )
            })
            .collect();
        sexpr!((rast &en $bodies))
    }
}

impl ToSExpr for Expr {
    fn to_sexpr(self) -> SExpr {
        let Expr { kind, ty, .. } = self;
        sexpr!((expr type of $ty &n body $kind))
    }
}

impl ToSExpr for Id<TypeId> {
    fn to_sexpr(self) -> SExpr {
        let Id { package, id } = self;
        let package = package.0;
        let id = id.0;
        sexpr!((id (package #package) (id #id)))
    }
}

impl ToSExpr for Id<DefinitionId> {
    fn to_sexpr(self) -> SExpr {
        let Id { package, id } = self;
        let package = package.0;
        let id = id.0;
        sexpr!((id (package #package) (id #id)))
    }
}

impl ToSExpr for Type {
    fn to_sexpr(self) -> SExpr {
        match self {
            Type::Int => sexpr!(int),
            Type::UInt => sexpr!(uint),
            Type::Bool => sexpr!(bool),
            Type::String => sexpr!(string),
            Type::Float => sexpr!(float),
            Type::Unit => sexpr!(unit),
            Type::Byte => sexpr!(byte),
            Type::Struct { def, fields } => sexpr!(
                (struct[$def] &en $fields)
            ),
            Type::ADT { def, constructors } => sexpr!(
                (adt[$def] &n $constructors)
            ),
            Type::Function { args, ret } => sexpr!(
                (fn $args $ret)
            ),
            Type::Tuple(items) => sexpr!((tuple @ items)),
            Type::Array(inner) => sexpr!((array $inner)),
            Type::Alias(id) => {
                sexpr!((alias $id))
            }
            Type::Divergent => sexpr!(divergent),
            Type::ToInfere => sexpr!(to_infere),
            Type::Scheme { prefex, typ } => sexpr!(
                (forall $prefex $typ)
            ),
            Type::App { typ, args } => sexpr!((app $typ @args)),
            Type::Generic(debruijn, index) => {
                let debruijn = debruijn.0;
                sexpr!((generic #debruijn #index))
            }
            Type::Fresh(id) => sexpr!((fresh #id)),
        }
    }
}

impl ToSExpr for ExprId {
    fn to_sexpr(self) -> SExpr {
        let ExprId(inner) = self;
        sexpr!((exprid #inner))
    }
}

impl ToSExpr for CallResolution {
    fn to_sexpr(self) -> SExpr {
        match self {
            CallResolution::StaticFunction(id) => sexpr!((static $id)),
            CallResolution::StaticMethod(id) => sexpr!((static method $id)),
            CallResolution::FromInstanceInScope {
                instance_index,
                method_def,
            } => sexpr!((
                from instance #instance_index $method_def
            )),
            CallResolution::Unresolved => sexpr!(unresolved),
        }
    }
}

impl ToSExpr for ResolvedName {
    fn to_sexpr(self) -> SExpr {
        match self {
            ResolvedName::Local(name) => sexpr!((local #name)),
            ResolvedName::Global(id) => sexpr!((global $id)),
        }
    }
}

impl ToSExpr for AccessResolution {
    fn to_sexpr(self) -> SExpr {
        match self {
            AccessResolution::Field(val) => sexpr!((field #val)),
            AccessResolution::Method(id) => sexpr!((method $id)),
        }
    }
}

impl ToSExpr for ExprKind {
    fn to_sexpr(self) -> classy_sexpr::SExpr {
        match self {
            ExprKind::This => sexpr!(this),
            ExprKind::Unit => sexpr!(unit),
            ExprKind::Sequence(exprs) => {
                let exprs = exprs.into_iter().map(ToSExpr::to_sexpr).collect::<Vec<_>>();
                sexpr!((&en $exprs))
            }
            ExprKind::Assignment { lhs, rhs } => sexpr!(
                ($lhs assign &n $rhs)
            ),
            ExprKind::IntConst(val) => sexpr!((const int #val)),
            ExprKind::StringConst(val) => sexpr!((const string #val)),
            ExprKind::FloatConst(val) => sexpr!((const string #val)),
            ExprKind::BoolConst(val) => sexpr!((const bool #val)),
            ExprKind::MethodCall {
                receiver,
                method,
                args,
                implicit_args,
                resolution,
            } => sexpr!((
                call &n (with $receiver) &n #method resolved to $resolution &n2 args &en3 $args
            )),
            ExprKind::Call {
                callee,
                args,
                implicit_args,
                resolution,
            } => sexpr!((call $callee[$resolution] &n2 args &en3 $args)),
            ExprKind::Name { symbol, resolved } => sexpr!((name #symbol[$resolved])),
            ExprKind::Access {
                val,
                field,
                access_resolution,
            } => sexpr!((get $val $field [$access_resolution])),
            ExprKind::Tuple(exprs) => sexpr!((tuple @ exprs)),
            ExprKind::ArrayLiteral { size, init } => sexpr!((array[$size] @init)),
            ExprKind::IndexAccess { lhs, index } => sexpr!((get index $lhs $index)),
            ExprKind::Let { name, init } => sexpr!((let #name &n $init)),
            ExprKind::If {
                cond,
                then,
                else_body,
            } => sexpr!((if $cond $then @else_body)),
            ExprKind::While { cond, body } => sexpr!((while $cond $body)),
            ExprKind::Lambda { args, body } => sexpr!((lambda @args $body)),
            ExprKind::StructLiteral { def, fields } => sexpr!((struct literal [$def] &en $fields)),
            ExprKind::AdtUnitConstructor { def, case } => sexpr!((adt[$def] unit #case)),
            ExprKind::AdtTupleConstructor { def, case, fields } => {
                sexpr!((adt[$def] tuple #case @fields))
            }
            ExprKind::AdtStructConstructor { def, case, fields } => sexpr!((
                adt[$def] struct #case @fields
            )),
            ExprKind::MakeInstance { def, args } => sexpr!((
                make instance[$def] @args
            )),
            ExprKind::Return { expr } => sexpr!((return $expr)),
        }
    }
}

fn gather(
    database: &Database,
    id: Id<DefinitionId>,
    constraints: &mut FlatScope<GenericConstraint>,
    prefex_scope: &mut PrefexScope,
) -> Option<Id<TypeId>> {
    let definition = database.get_definition(id).unwrap();
    let parent = definition.parent;
    let mut def_receiver = None;
    if let Some(parent) = parent {
        def_receiver = gather(
            database,
            parent.as_global(id.package),
            constraints,
            prefex_scope,
        );
    }
    println!("Looking at definition {id:?} => {}", definition.name);
    match definition.kind {
        DefinitionKind::Function(_) | DefinitionKind::Method(_) => {
            let ty = database.get_definitions_type(id).unwrap();
            if let Type::Scheme { prefex, .. } = ty {
                constraints.new_scope();
                prefex_scope.new_scope();
                prefex_scope.add_type_vars(&prefex);
            }
        }
        DefinitionKind::MethodBlock(MethodBlockInfo {
            free_vars,
            receiver,
            ..
        }) => {
            def_receiver = Some(receiver);
            if !free_vars.is_empty() {
                constraints.new_scope();
                prefex_scope.new_scope();
                prefex_scope.add_type_vars(&free_vars);
            }
        }
        DefinitionKind::Instance(InstanceInfo { free_vars, .. }) => {
            if !free_vars.is_empty() {
                constraints.new_scope();
                prefex_scope.new_scope();
                prefex_scope.add_type_vars(&free_vars);
            }
        }
        kind => panic!("Unexpected definition kind: {kind:?}"),
    }
    if !definition.constraints.is_empty() {
        assert!(!constraints.is_empty());
        constraints
            .last_scope_mut()
            .unwrap()
            .extend_from_slice(&definition.constraints);
    }
    def_receiver
}
