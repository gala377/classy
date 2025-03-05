//! Resolved AST. Simplified ast that has all the calls
//! names and expr types resolved.

use std::collections::HashMap;

use classy_syntax::ast::{self, transformer::AstExprTransformer, FunctionDefinition};

use super::{
    constraint_generation::ExprId,
    constraint_solver::CallResolution,
    knowledge::{
        Database, DefinitionId, DefinitionKind, Id, LocalId, PackageId, CURRENT_PACKAGE_ID,
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
        let mut builder = RastBuilder {
            database,
            expr_types,
            call_resolutions: &call_resolutions,
            name_resolutions,
            is_method,
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
                // TODO: Could be a method call, depends on the resolution.
                let resolution = self.call_resolutions.get(&ExprId(id)).clone();
                let kind: ExprKind = match resolution {
                    // If its a method call we need to call transform_method passing this as
                    // the receiver
                    // Otherwise its just a function call. It does not need to be static btw.
                    // Could be just a name. In this case idk, i guess call resolution is empty.
                    Some(CallResolution::StaticMethod(_)) => todo!(),
                    Some(CallResolution::FromInstanceInScope {
                        instance_index,
                        method_def,
                    }) => todo!(),
                    Some(CallResolution::StaticFunction(_)) => todo!(),
                    Some(CallResolution::Unresolved) => panic!("Call is unresolved"),
                    None => {
                        assert!(
                            matches!(func.kind, ast::ExprKind::Name(ast::Name::Local(_))),
                            "Kind: {:?}, Resolution: {:?}",
                            func.kind,
                            &self.call_resolutions
                        );
                        todo!("call resolution does not exist, so this should a call to a local")
                    }
                };
                todo!()
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
                let (resolved, symbol) = match name {
                    ast::Name::Local(name) if name == "this" && self.is_method => {
                        return Expr {
                            id: ExprId(id),
                            kind: ExprKind::This,
                            ty: typ,
                        };
                    }
                    ast::Name::Local(name) => (ResolvedName::Local(name.clone()), name),
                    ast::Name::Global {
                        package,
                        definition,
                    } => {
                        let package = PackageId(package);
                        let id = Id {
                            package,
                            id: DefinitionId(definition),
                        };
                        let name = self.database.get_definition_map(id, |def| def.name.clone());
                        (ResolvedName::Global(id), name.unwrap())
                    }
                    ast::Name::Unresolved { path, identifier } => {
                        match self.name_resolutions.get(&ExprId(id)) {
                            Some(ast::Name::Local(name)) => {
                                (ResolvedName::Local(name.clone()), name.clone())
                            }
                            Some(ast::Name::Global {
                                package,
                                definition,
                            }) => (
                                ResolvedName::Global(Id {
                                    package: PackageId(*package),
                                    id: DefinitionId(*definition),
                                }),
                                identifier.clone(),
                            ),
                            _ => {
                                panic!("After all could not resolve name {path:?}::{identifier}")
                            }
                        }
                    }
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

    fn transform_name(&mut self, name: ast::Name) -> Self::ExprKind {
        let (resolved, symbol) = match name {
            ast::Name::Local(name) if name == "this" && self.is_method => {
                return ExprKind::This;
            }
            ast::Name::Local(name) => (ResolvedName::Local(name.clone()), name),
            ast::Name::Global {
                package,
                definition,
            } => {
                let package = PackageId(package);
                let id = Id {
                    package,
                    id: DefinitionId(definition),
                };
                let name = self.database.get_definition_map(id, |def| def.name.clone());
                (ResolvedName::Global(id), name.unwrap())
            }
            ast::Name::Unresolved { path, identifier } => {
                panic!("Unresolved name: {path:?}::{identifier:?}")
            }
        };
        ExprKind::Name { symbol, resolved }
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

    fn transform_function_call(
        &mut self,
        _func: ast::Expr,
        _args: Vec<ast::Expr>,
        _kwargs: HashMap<String, ast::Expr>,
    ) -> Self::ExprKind {
        unreachable!("Should be resolved at transform_expr")
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
        strct: ast::Name,
        values: HashMap<String, ast::Expr>,
    ) -> Self::ExprKind {
        let resolved = match strct {
            ast::Name::Global {
                package,
                definition,
            } => Id {
                package: PackageId(package),
                id: DefinitionId(definition),
            },
            ast::Name::Local(_) => panic!("Struct name should never be local"),
            ast::Name::Unresolved { path, identifier } => {
                panic!("Unresolved name: {path:?}::{identifier:?}")
            }
        };
        ExprKind::StructLiteral {
            def: resolved,
            fields: values
                .into_iter()
                .map(|(key, value)| (key, self.transform_expr(value)))
                .collect(),
        }
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
