use std::collections::{HashMap, HashSet};

use classy_syntax::ast::{self, Pattern};

use crate::{
    scope::{FlatScope, FlatScopeExt},
    session::Session,
    typecheck::ast_to_type::PrefexScope,
    v2::{
        constraint_solver::ConstraintSolver,
        instance::instance,
        knowledge::{
            Database, DefinitionId, DefinitionKind, FunctionInfo, GenericConstraint, Id,
            InstanceInfo, MethodBlockInfo, MethodInfo, PackageId, TypeId,
        },
        name_scope::NameScope,
        ty::Type,
    },
};

#[derive(Debug)]
pub enum Constraint {
    Eq(Type, Type),
    HasProperty {
        ty: Type,
        property: String,
        of_type: Type,
    },
    HasCase {
        ty: Type,
        case: String,
        of_type: Type,
    },
    HasMethod {
        receiver: Type,
        method: String,
        of_type: Type,
        resolution_id: usize,
    },
    MethodOrGlobal {
        receiver: Type,
        name: String,
        of_ty: Type,
        resolution_id: usize,
    },
}

#[repr(transparent)]
#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct ExprId(pub usize);

pub struct Inferer<'sess, 'db> {
    constraints: Vec<Constraint>,
    pub expr_types: HashMap<ExprId, Type>,
    scope: NameScope,
    session: &'sess Session,
    database: &'db mut Database,
    current_namespace: Vec<String>,
    receiver: Option<Type>,
    prefex_scope: PrefexScope,
    function_return_type: Type,

    // Can be a method def id if we are inferring a method
    function_def_id: Id<DefinitionId>,
    constraints_in_scope: FlatScope<GenericConstraint>,
    pub trivial_name_resolutions: HashMap<ExprId, ast::Name>,
}

impl<'sess, 'db> Inferer<'sess, 'db> {
    pub fn new(
        session: &'sess Session,
        database: &'db mut Database,
        current_namespace: &[String],
        prefex_scope: PrefexScope,
        receiver: Option<Type>,
        function_def_id: Id<DefinitionId>,
        function_args: &[(String, Type)],
        function_return_type: Type,
        constraints_in_scope: FlatScope<GenericConstraint>,
    ) -> Self {
        let mut scope = NameScope::new();
        for (name, ty) in function_args {
            scope.add_variable(name.clone(), ty.clone());
        }
        Self {
            constraints: Vec::new(),
            expr_types: HashMap::new(),
            scope,
            session,
            database,
            current_namespace: current_namespace.to_vec(),
            prefex_scope,
            function_return_type,
            receiver,
            constraints_in_scope,
            function_def_id,
            trivial_name_resolutions: HashMap::new(),
        }
    }

    pub fn for_function(
        id: Id<DefinitionId>,
        database: &'db mut Database,
        session: &'sess Session,
    ) -> Self {
        assert!(id.as_local().is_some());
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

        let def = database.get_definition(id).unwrap();
        let namespace = database.get_namespace(id.as_local().unwrap()).to_vec();
        let mut constraints = FlatScope::new();
        let mut prefex_scope = PrefexScope::without_scope();
        let receiver = gather(database, id, &mut constraints, &mut prefex_scope)
            .map(|r| database.resolve_alias_to_type(r).unwrap());
        let definition_type = database.get_definitions_type(id).cloned().unwrap();
        let definition_type = Self::resolve_alias_deep(database, definition_type);
        let (args_ty, ret_ty) = match definition_type {
            Type::Function { args, ret } => (args, *ret),
            Type::Scheme {
                typ: box Type::Function { args, ret },
                ..
            } => (args, *ret),
            t => panic!("Expected function, got {:?}", t),
        };
        let arg_names = match def.kind {
            DefinitionKind::Method(MethodInfo { arg_names, .. }) => arg_names.clone(),
            DefinitionKind::Function(FunctionInfo { arg_names, .. }) => arg_names.clone(),
            _ => panic!("Expected function or method, got {:?}", def.kind),
        };
        assert_eq!(
            args_ty.len(),
            arg_names.len(),
            "Maybe you passed declaration instead of a definition? Declarations don't have arg \
             names."
        );
        let args = arg_names
            .iter()
            .zip(args_ty.iter())
            .map(|(name, ty)| (name.clone(), ty.clone()))
            .collect::<Vec<_>>();
        Self::new(
            session,
            database,
            &namespace,
            prefex_scope,
            receiver,
            id,
            &args,
            ret_ty,
            constraints,
        )
    }

    fn resolve_alias_deep(database: &Database, ty: Type) -> Type {
        match ty {
            Type::Alias(for_t) => {
                let id = database.resolve_alias_to_type(for_t).unwrap();
                Self::resolve_alias_deep(database, id)
            }
            Type::Scheme {
                prefex,
                typ: box Type::Alias(for_t),
            } => {
                let id = database.resolve_alias_to_type(for_t).unwrap();
                Type::Scheme {
                    prefex,
                    typ: Box::new(Self::resolve_alias_deep(database, id)),
                }
            }
            t => t,
        }
    }

    pub fn into_constraint_solver(self) -> ConstraintSolver<'db, 'sess> {
        let file = self
            .database
            .definitions
            .get(&self.function_def_id.as_local().unwrap())
            .unwrap()
            .file;
        let visible_instances = self.database.get_visible_instances(file);
        let visible_method_blocks = self.database.get_visible_method_blocks(file);
        let types = self.database.all_types().collect();
        let classes = self.database.all_classes().collect();
        ConstraintSolver::new(
            self.session,
            self.database,
            self.current_namespace,
            self.prefex_scope,
            self.constraints,
            self.constraints_in_scope,
            visible_instances,
            visible_method_blocks,
            types,
            classes,
        )
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    pub fn add_expr_type(&mut self, expr: ExprId, ty: Type) {
        self.expr_types.insert(expr, ty);
    }

    fn new_fresh_type(&self) -> Type {
        Type::Fresh(self.session.id_provider().next())
    }

    pub fn infer_function_body(&mut self) {
        println!("All function defnitions");
        for (id, def) in self.database.function_definitions.iter() {
            println!("{id:?}: {def:#?}\n\n\n");
        }
        println!("Function id {:?}", self.function_def_id);
        let id = self.function_def_id.as_local().unwrap();
        let body = self
            .database
            .function_definitions
            .get(&id)
            .map(|def| def.body.clone())
            .unwrap_or_else(|| {
                self.database
                    .method_definitions
                    .get(&id)
                    .unwrap()
                    .body
                    .clone()
            });
        let ret_type = self.infer_expr(&body);
        match &self.function_return_type {
            Type::Unit => {}
            t => {
                self.add_constraint(Constraint::Eq(ret_type, t.clone()));
            }
        }
    }

    pub fn infer_expr(&mut self, ast::Expr { id, kind }: &ast::Expr) -> Type {
        let ty: Type = match kind {
            ast::ExprKind::Unit => Type::Unit,
            ast::ExprKind::Sequence(exprs) => exprs
                .iter()
                .map(|expr| self.infer_expr(expr))
                .last()
                .expect(&format!("Expected sequence to not be empty {:?}", exprs)),
            ast::ExprKind::Assignment { lval, rval } => {
                let lval_ty = self.infer_expr(lval);
                let rval_ty = self.infer_expr(rval);
                self.add_constraint(Constraint::Eq(lval_ty, rval_ty));
                Type::Unit
            }
            ast::ExprKind::IntConst(_) => Type::Int,
            ast::ExprKind::StringConst(_) => Type::String,
            ast::ExprKind::FloatConst(_) => Type::Float,
            ast::ExprKind::BoolConst(_) => Type::Bool,
            ast::ExprKind::Name(name) => match name {
                ast::Name::Unresolved { path, identifier }
                    if path.is_empty() && identifier == "this" && self.receiver.is_some() =>
                {
                    self.receiver.clone().unwrap()
                }
                ast::Name::Unresolved { path, identifier } if path.is_empty() => {
                    let possibly_local = self.scope.get_variable(identifier).cloned();
                    match possibly_local {
                        Some(ty) => {
                            self.trivial_name_resolutions
                                .insert(ExprId(*id), ast::Name::Local(identifier.clone()));
                            ty
                        }
                        None => {
                            let (def, ty) = self
                                .database
                                .resolve_unresolved_name(&self.current_namespace, path, identifier)
                                .map(|(def, ty)| (def, ty.clone()))
                                .unwrap_or_else(|| {
                                    panic!("Name {path:?}::{identifier} not found in database")
                                });
                            self.trivial_name_resolutions.insert(
                                ExprId(*id),
                                ast::Name::Global {
                                    package: def.package.0,
                                    definition: def.id.0,
                                },
                            );
                            ty
                        } /* ! TODO
                           * other possibility is that it is a name that is scoped by the
                           * constraint so we should lookup
                           * names in classes in scope to see if we can find this name.
                           * Also if receiver.is_some() == true then this can also be a method or
                           * a field so we should have a constraint
                           * like MethodOrField { receiver, name, of_ty }
                           * That we should resolve in the constraint solver. */
                    }
                }
                ast::Name::Unresolved { path, identifier } => {
                    let (def, ty) = self
                        .database
                        .resolve_unresolved_name(&self.current_namespace, path, identifier)
                        .map(|(def, ty)| (def, ty.clone()))
                        .unwrap_or_else(|| {
                            panic!("Name {path:?}::{identifier} not found in database")
                        });
                    self.trivial_name_resolutions.insert(
                        ExprId(*id),
                        ast::Name::Global {
                            package: def.package.0,
                            definition: def.id.0,
                        },
                    );
                    ty
                }
                name => {
                    panic!("Name {name:?} not expected as resolution did not take place yet");
                }
            },
            ast::ExprKind::FunctionCall {
                func:
                    box ast::Expr {
                        id: _,
                        kind: ast::ExprKind::Name(ast::Name::Unresolved { path, identifier }),
                    },
                args,
                kwargs: _,
            } if path.is_empty() && self.receiver.is_some() => {
                println!("AAAAA Possible method is triggered?");
                // possibly a method call
                let arg_tys = args.iter().map(|arg| self.infer_expr(arg)).collect();
                let ret_ty = self.new_fresh_type();
                let inferred_func_ty = Type::Function {
                    args: arg_tys,
                    ret: Box::new(ret_ty.clone()),
                };
                self.add_constraint(Constraint::MethodOrGlobal {
                    receiver: self.receiver.clone().unwrap(),
                    name: identifier.clone(),
                    of_ty: inferred_func_ty.clone(),
                    resolution_id: *id,
                });
                ret_ty
            }
            ast::ExprKind::FunctionCall {
                func,
                args,
                kwargs: _,
            } => {
                println!("AAAAA This one has been triggered");
                let func_ty = self.infer_expr(func);
                let arg_tys = args.iter().map(|arg| self.infer_expr(arg)).collect();
                let ret_ty = self.new_fresh_type();
                let inferred_func_ty = Type::Function {
                    args: arg_tys,
                    ret: Box::new(ret_ty.clone()),
                };
                self.add_constraint(Constraint::Eq(func_ty, inferred_func_ty.clone()));
                ret_ty
            }
            ast::ExprKind::Access { val, field } => {
                let val_ty = self.infer_expr(val);
                let field_ty = self.new_fresh_type();
                self.add_constraint(Constraint::HasProperty {
                    ty: val_ty,
                    property: field.clone(),
                    of_type: field_ty.clone(),
                });
                field_ty
            }
            ast::ExprKind::Tuple(inner) => {
                let inner_tys = inner.iter().map(|expr| self.infer_expr(expr)).collect();
                Type::Tuple(inner_tys)
            }
            ast::ExprKind::Lambda { parameters, body } => {
                self.scope.new_scope();
                let param_tys = parameters
                    .iter()
                    .map(|ast::TypedIdentifier { name, typ }| {
                        let ty = if *typ == ast::Typ::ToInfere {
                            self.new_fresh_type()
                        } else {
                            self.database.ast_type_to_type_shallow(
                                self.session,
                                &mut self.prefex_scope,
                                &self.current_namespace,
                                typ,
                            )
                        };
                        self.scope.add_variable(name.clone(), ty.clone());
                        ty
                    })
                    .collect();
                let old_ret_t = self.function_return_type.clone();
                self.function_return_type = self.new_fresh_type();
                let body_ty = self.infer_expr(body);
                self.add_constraint(Constraint::Eq(body_ty, self.function_return_type.clone()));
                let ret = Type::Function {
                    args: param_tys,
                    ret: Box::new(self.function_return_type.clone()),
                };
                self.function_return_type = old_ret_t;
                self.scope.pop_scope();
                ret
            }
            ast::ExprKind::TypedExpr { expr, typ } => {
                let ty = self.database.ast_type_to_type_shallow(
                    self.session,
                    &mut self.prefex_scope,
                    &self.current_namespace,
                    typ,
                );
                let expr_ty = self.infer_expr(expr);
                self.add_constraint(Constraint::Eq(expr_ty, ty.clone()));
                ty
            }
            ast::ExprKind::StructLiteral { strct, values } => {
                let ret = self.new_fresh_type();
                let mut strct_t = self.resolve_type_name(strct);
                if let Type::Alias(for_t) = strct_t {
                    let tid = self.database.resolve_alias(for_t).unwrap();
                    strct_t = self.database.resolve_tid(tid).unwrap();
                }
                if let Type::Scheme { prefex, .. } = &strct_t {
                    let arguments = (0..prefex.len())
                        .map(|_| self.new_fresh_type())
                        .collect::<Vec<_>>();
                    strct_t = Type::App {
                        typ: Box::new(strct_t),
                        args: arguments,
                    };
                }
                self.add_constraint(Constraint::Eq(ret.clone(), strct_t.clone()));
                let (def, fields) = self.unpack_struct(&strct_t).unwrap();
                let seen_fields = values.keys().collect::<HashSet<_>>();
                let actual_fields = fields.iter().map(|(name, _)| name).collect::<HashSet<_>>();
                if seen_fields != actual_fields {
                    panic!(
                        "Expected fields {actual_fields:?}, got {seen_fields:?} in struct literal"
                    );
                }
                for (name, expr) in values {
                    let expr_ty = self.infer_expr(expr);
                    let field_ty = fields
                        .iter()
                        .find(|(field_name, _)| field_name == name)
                        .map(|(_, ty)| ty.clone())
                        .unwrap_or_else(|| panic!("Field {name} not found in struct {def:?}"));
                    self.add_constraint(Constraint::Eq(expr_ty, field_ty));
                }
                ret
            }
            ast::ExprKind::AdtTupleConstructor {
                typ,
                constructor,
                args,
            } => {
                let ret = self.new_fresh_type();
                let strct_t = self.resolve_type_name(typ);
                let inferred_args = args.iter().map(|arg| self.infer_expr(arg)).collect();
                self.add_constraint(Constraint::HasCase {
                    ty: ret.clone(),
                    case: constructor.clone(),
                    of_type: Type::Tuple(inferred_args),
                });
                self.add_constraint(Constraint::Eq(ret.clone(), strct_t.clone()));
                ret
            }
            ast::ExprKind::AdtStructConstructor {
                typ,
                constructor,
                fields,
            } => {
                let ret = self.new_fresh_type();
                let inferred_fields = fields
                    .iter()
                    .map(|(name, t)| {
                        let t = self.infer_expr(t);
                        (name.clone(), t)
                    })
                    .collect();
                self.add_constraint(Constraint::HasCase {
                    ty: ret.clone(),
                    case: constructor.clone(),
                    of_type: Type::Struct {
                        def: Id::dummy(),
                        fields: inferred_fields,
                    },
                });
                let strct_t = self.resolve_type_name(typ);
                self.add_constraint(Constraint::Eq(ret.clone(), strct_t));
                ret
            }
            ast::ExprKind::AdtUnitConstructor { typ, constructor } => {
                let ret = self.new_fresh_type();
                self.add_constraint(Constraint::HasCase {
                    ty: ret.clone(),
                    case: constructor.clone(),
                    of_type: Type::Unit,
                });
                let strct_t = self.resolve_type_name(typ);
                self.add_constraint(Constraint::Eq(ret.clone(), strct_t));
                ret
            }
            ast::ExprKind::While { cond, body } => {
                let cond_ty = self.infer_expr(cond);
                self.add_constraint(Constraint::Eq(cond_ty, Type::Bool));
                self.scope.new_scope();
                let body_ty = self.infer_expr(body);
                self.scope.pop_scope();
                self.add_constraint(Constraint::Eq(body_ty, Type::Unit));
                Type::Unit
            }
            ast::ExprKind::Return(expr) => {
                let expr_ty = self.infer_expr(expr);
                self.add_constraint(Constraint::Eq(expr_ty, self.function_return_type.clone()));
                Type::Divergent
            }
            ast::ExprKind::If {
                cond,
                body,
                else_body,
            } => {
                let cond_ty = self.infer_expr(cond);
                self.add_constraint(Constraint::Eq(cond_ty, Type::Bool));
                self.scope.new_scope();
                let body_ty = self.infer_expr(body);
                self.scope.pop_scope();
                if let Some(else_body) = else_body {
                    self.scope.new_scope();
                    let else_ty = self.infer_expr(else_body);
                    self.scope.pop_scope();
                    self.add_constraint(Constraint::Eq(body_ty.clone(), else_ty));
                }
                if else_body.is_some() {
                    body_ty
                } else {
                    Type::Unit
                }
            }
            ast::ExprKind::Let { name, typ, init } => {
                let init_ty = self.infer_expr(init);
                self.scope.add_variable(name.clone(), init_ty.clone());
                if *typ != ast::Typ::ToInfere {
                    let ty = self.database.ast_type_to_type_shallow(
                        self.session,
                        &mut self.prefex_scope,
                        &self.current_namespace,
                        typ,
                    );
                    self.add_constraint(Constraint::Eq(init_ty.clone(), ty));
                }
                Type::Unit
            }
            ast::ExprKind::LetRec { .. } => todo!(),
            ast::ExprKind::AnonType { .. } => todo!(),
            ast::ExprKind::ArrayLiteral { typ, size, init } => {
                let init_ty = init
                    .iter()
                    .map(|expr| self.infer_expr(expr))
                    .collect::<Vec<_>>();
                let size_ty = self.infer_expr(size);
                let inner_ty = if *typ == ast::Typ::ToInfere {
                    self.new_fresh_type()
                } else {
                    self.database.ast_type_to_type_shallow(
                        self.session,
                        &mut self.prefex_scope,
                        &self.current_namespace,
                        typ,
                    )
                };
                self.add_constraint(Constraint::Eq(size_ty, Type::Int));
                for ty in init_ty.into_iter() {
                    self.add_constraint(Constraint::Eq(ty.clone(), inner_ty.clone()));
                }
                Type::Array(Box::new(inner_ty))
            }
            ast::ExprKind::IndexAccess { lhs, index } => {
                let lhs_ty = self.infer_expr(lhs);
                let index_ty = self.infer_expr(index);
                let inner_ty = self.new_fresh_type();
                self.add_constraint(Constraint::Eq(
                    lhs_ty,
                    Type::Array(Box::new(inner_ty.clone())),
                ));
                self.add_constraint(Constraint::Eq(index_ty, Type::Int));
                inner_ty
            }
            ast::ExprKind::Match { expr, cases } => {
                let ret = self.new_fresh_type();
                let expr_ty = self.infer_expr(expr);
                for (pattern, body, guard) in cases {
                    self.scope.new_scope();
                    let mut subinferer = PatternInferer {
                        constraints: Vec::new(),
                        scope: &mut self.scope,
                        session: self.session,
                        database: self.database,
                        expr_types: HashMap::new(),
                        current_namespace: &self.current_namespace,
                    };
                    let pattern_ty = subinferer.infer_pattern(pattern);

                    // pattern constraints have to be reversed and added after pattern == expr type
                    // constraint
                    let pattern_constraints =
                        subinferer.constraints.into_iter().rev().collect::<Vec<_>>();
                    let pattern_types = subinferer.expr_types;
                    self.add_constraint(Constraint::Eq(pattern_ty, expr_ty.clone()));
                    self.constraints.extend(pattern_constraints);
                    self.expr_types.extend(pattern_types);
                    if let Some(guard) = guard {
                        let guard_ty = self.infer_expr(guard);
                        self.add_constraint(Constraint::Eq(guard_ty, Type::Bool));
                    }
                    let body_ty = self.infer_expr(body);
                    self.add_constraint(Constraint::Eq(body_ty, ret.clone()));
                    self.scope.pop_scope();
                }
                ret
            }
            ast::ExprKind::MethodCall {
                receiver,
                method,
                args,
                kwargs: _,
            } => {
                let receiver_ty = self.infer_expr(receiver);
                let arg_tys = args.iter().map(|arg| self.infer_expr(arg)).collect();
                let ret_ty = self.new_fresh_type();
                let inferred_func_ty = Type::Function {
                    args: arg_tys,
                    ret: Box::new(ret_ty.clone()),
                };
                self.add_constraint(Constraint::HasMethod {
                    receiver: receiver_ty,
                    method: method.clone(),
                    of_type: inferred_func_ty.clone(),
                    resolution_id: *id,
                });
                ret_ty
            }
        };
        self.add_expr_type(ExprId(*id), ty.clone());
        ty
    }

    fn resolve_type_name(&self, name: &ast::Name) -> Type {
        match name {
            ast::Name::Unresolved { path, identifier } => self
                .database
                .get_type_by_unresolved_name(&self.current_namespace, path, identifier)
                .cloned()
                .unwrap_or_else(|| panic!("Name {path:?}::{identifier} not found in database")),
            ast::Name::Global {
                package,
                definition,
            } => {
                let package = PackageId(*package);
                let definition = DefinitionId(*definition);
                let id = Id {
                    package,
                    id: definition,
                };
                self.database.get_definitions_type(id).cloned().unwrap()
            }
            ast::Name::Local(_) => panic!("What are you trying to do exaclty?"),
        }
    }

    fn unpack_struct(&self, strct: &Type) -> Option<(Id<DefinitionId>, Vec<(String, Type)>)> {
        match strct {
            Type::Struct { def, fields } => Some((*def, fields.clone())),
            Type::App {
                typ:
                    scheme @ box Type::Scheme {
                        prefex: _,
                        typ: box Type::Struct { def, .. },
                    },
                args,
            } => {
                let Type::Struct { fields, .. } =
                    instance(&self.database, *scheme.clone(), args.clone()).unwrap()
                else {
                    unreachable!("Not a struct, should be a struct");
                };
                Some((*def, fields))
            }
            _ => None,
        }
    }
}

struct PatternInferer<'sess, 'db, 'scope, 'namespace> {
    constraints: Vec<Constraint>,
    scope: &'scope mut NameScope,
    session: &'sess Session,
    database: &'db mut Database,
    expr_types: HashMap<ExprId, Type>,
    current_namespace: &'namespace [String],
}

impl PatternInferer<'_, '_, '_, '_> {
    pub fn new_fresh_type(&self) -> Type {
        Type::Fresh(self.session.id_provider().next())
    }

    pub fn infer_pattern(&mut self, pattern: &Pattern) -> Type {
        let typ = match &pattern.kind {
            ast::PatternKind::Var(name) => {
                let ty = self.new_fresh_type();
                if name.starts_with(char::is_uppercase) {
                    self.constraints.push(Constraint::HasCase {
                        ty: ty.clone(),
                        case: name.clone(),
                        of_type: Type::Unit,
                    });
                } else {
                    self.scope.add_variable(name.clone(), ty.clone());
                }
                ty
            }
            ast::PatternKind::Tuple(inner) => {
                let typ = self.new_fresh_type();
                let inner_types = inner.iter().map(|p| self.infer_pattern(p)).collect();
                self.constraints
                    .push(Constraint::Eq(typ.clone(), Type::Tuple(inner_types)));
                typ
            }
            ast::PatternKind::Struct {
                strct: ast::Name::Unresolved { path, identifier },
                fields,
            } if path.is_empty() => {
                let typ = self.new_fresh_type();
                let strct = identifier;
                let inner_types = fields
                    .iter()
                    .map(|(n, p)| {
                        let typ = self.infer_pattern(p);
                        (n.clone(), typ)
                    })
                    .collect::<Vec<_>>();
                self.constraints.push(Constraint::HasCase {
                    ty: typ.clone(),
                    case: strct.clone(),
                    of_type: Type::Struct {
                        def: Id::dummy(),
                        fields: inner_types.into_iter().collect(),
                    },
                });
                typ
            }
            ast::PatternKind::Struct { strct, fields } => {
                let typ = self.new_fresh_type();
                let strct_t = self
                    .database
                    .get_type_by_name(strct, self.current_namespace)
                    .cloned()
                    .unwrap();
                let inner_types = fields
                    .iter()
                    .map(|(n, p)| {
                        let typ = self.infer_pattern(p);
                        (n.clone(), typ)
                    })
                    .collect::<Vec<_>>();
                for (name, ty) in inner_types {
                    self.constraints.push(Constraint::HasProperty {
                        ty: strct_t.clone(),
                        property: name,
                        of_type: ty,
                    });
                }
                typ
            }
            ast::PatternKind::TupleStruct {
                strct: ast::Name::Unresolved { path, identifier },
                fields,
            } if path.is_empty() => {
                let typ = self.new_fresh_type();

                let strct = identifier;
                let inner_types = fields
                    .iter()
                    .map(|p| self.infer_pattern(p))
                    .collect::<Vec<_>>();
                self.constraints.push(Constraint::HasCase {
                    ty: typ.clone(),
                    case: strct.clone(),
                    of_type: Type::Tuple(inner_types),
                });
                typ
            }
            ast::PatternKind::AnonStruct { fields } => {
                let typ = self.new_fresh_type();

                let inner_types = fields
                    .iter()
                    .map(|(n, p)| {
                        let typ = self.infer_pattern(p);
                        (n.clone(), typ)
                    })
                    .collect::<Vec<_>>();
                for (name, t) in &inner_types {
                    self.constraints.push(Constraint::HasProperty {
                        ty: typ.clone(),
                        property: name.clone(),
                        of_type: t.clone(),
                    })
                }
                typ
            }
            ast::PatternKind::Array(patterns) => {
                let typ = self.new_fresh_type();
                let inner = patterns
                    .iter()
                    .map(|p| self.infer_pattern(p))
                    .collect::<Vec<_>>();
                for t in &inner {
                    self.constraints
                        .push(Constraint::Eq(t.clone(), inner[0].clone()));
                }
                self.constraints.push(Constraint::Eq(
                    typ.clone(),
                    Type::Array(Box::new(inner[0].clone())),
                ));
                typ
            }
            ast::PatternKind::Wildcard => self.new_fresh_type(),
            ast::PatternKind::Unit => Type::Unit,
            ast::PatternKind::String(_) => Type::String,
            ast::PatternKind::Int(_) => Type::Int,
            ast::PatternKind::Bool(_) => Type::Bool,
            ast::PatternKind::TypeSpecifier(name, case) => {
                let t = self
                    .database
                    .get_type_by_name(name, self.current_namespace)
                    .cloned()
                    .expect("Type not found");
                let case_t = self.infer_pattern(case);
                self.constraints
                    .push(Constraint::Eq(t.clone(), case_t.clone()));
                t
            }
            p => panic!("Pattern {p:?} not implemented"),
        };
        self.expr_types.insert(ExprId(pattern.id), typ.clone());
        typ
    }
}

#[cfg(test)]
mod tests {
    use crate::v2::knowledge::{Definition, DefinitionKind, LocalId, PackageInfo, TypeId};

    use super::*;
    use crate::util::composition::Pipe;
    use crate::v2::compile::Compiler;
    use crate::v2::knowledge::Database;
    use classy_syntax::ast;
    use std::collections::HashMap;

    const SOURCE_1: &str = r#"
        // import std::Int 

        type Foo {
            bar: std::Int
        }

        foo: (std::Int, std::Int) -> std::Int
        foo(a, b) = ()
    "#;

    fn prepare_std_package() -> PackageInfo {
        let types = vec![
            ("String", crate::v2::ty::Type::String),
            ("Int", crate::v2::ty::Type::Int),
            ("Bool", crate::v2::ty::Type::Bool),
            ("Float", crate::v2::ty::Type::Float),
            ("Byte", crate::v2::ty::Type::Byte),
            ("UInt", crate::v2::ty::Type::UInt),
        ];
        PackageInfo {
            name: "std".to_string(),
            globals: {
                let mut globals = HashMap::new();
                for (i, (name, _)) in types.iter().enumerate() {
                    globals.insert(name.to_string(), LocalId(DefinitionId(i)));
                }
                globals
            },
            definition: {
                let mut map = HashMap::new();
                for (i, (name, _)) in types.iter().enumerate() {
                    map.insert(
                        LocalId(DefinitionId(i)),
                        Definition {
                            name: name.to_string(),
                            kind: DefinitionKind::Type,
                            constraints: vec![],
                            ty: LocalId(TypeId(i)),
                            file: LocalId(DefinitionId(0)),
                            implicit_imports: vec![],
                            parent: None,
                            annotations: Default::default(),
                        },
                    );
                }
                map
            },
            typeid_to_type: {
                let mut map = HashMap::new();
                for (i, (_, ty)) in types.iter().enumerate() {
                    map.insert(LocalId(TypeId(i)), ty.clone());
                }
                map
            },
            method_blocks: Default::default(),
            classes: Default::default(),
            instances: Default::default(),
            methods: Default::default(),
        }
    }

    fn setup_database(source: &str) -> (Database, Session) {
        let std_package = prepare_std_package();
        let compiler = Compiler::new(
            "test",
            vec![("test".into(), source.into())],
            vec![std_package],
        );
        let (db, sess) = compiler.make_database();
        for (name, id) in db.globals.iter() {
            println!("{name}: {id:?}");
            let def = db.get_global(name).unwrap();
            let ty = db.get_definitions_type(def.as_global(PackageId(0)));
            println!("type: {ty:?}");
        }
        (db, sess)
    }

    fn simple_inferer<'sess, 'db>(
        database: &'db mut Database,
        session: &'sess Session,
    ) -> Inferer<'sess, 'db> {
        Inferer::new(
            session,
            database,
            &[],
            PrefexScope::with_empty_scope(),
            None,
            Id::dummy(),
            &[],
            Type::Unit,
            vec![],
        )
    }

    #[test]
    fn infer_intlit() {
        let (mut database, session) = setup_database(SOURCE_1);
        let mut inferer = simple_inferer(&mut database, &session);
        let res = inferer.infer_expr(&ast::Expr {
            id: 0,
            kind: ast::ExprKind::IntConst(0),
        });
        assert_eq!(res, Type::Int);
    }

    fn assert_types_eq(database: &Database, res: &Type, expected: &Type) {
        if !database.types_strictly_eq(res, expected).unwrap() {
            let resolved = if let Type::Alias(for_t) = res {
                let id = database.resolve_alias(*for_t).unwrap();
                database.resolve_tid(id).unwrap()
            } else {
                res.clone()
            };
            panic!("expected {expected:?} got {resolved:?}");
        }
    }

    #[test]
    fn infer_function_t() {
        let (mut database, session) = setup_database(SOURCE_1);
        let mut inferer = simple_inferer(&mut database, &session);
        let res = inferer.infer_expr(&ast::Expr {
            id: 0,
            kind: ast::ExprKind::Name(ast::Name::Unresolved {
                path: vec![],
                identifier: "foo".to_string(),
            }),
        });
        let expected = Type::Function {
            args: vec![Type::Int, Type::Int],
            ret: Box::new(Type::Int),
        };
        assert_types_eq(&database, &res, &expected);
    }

    const SOURCE_2: &str = r#"
        namespace some::nested::name
        type Foo {}

        foo: (Foo) -> std::Int
        foo a = ()
    "#;

    #[test]
    fn infer_function_t_within_namespace() {
        let (mut database, session) = setup_database(SOURCE_2);
        let mut inferer = Inferer::new(
            &session,
            &mut database,
            &["some", "nested", "name"]
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            PrefexScope::with_empty_scope(),
            None,
            Id::dummy(),
            &[],
            Type::Unit,
            vec![],
        );
        let res = inferer.infer_expr(&ast::Expr {
            id: 0,
            kind: ast::ExprKind::Name(ast::Name::Unresolved {
                path: vec![],
                identifier: "foo".to_string(),
            }),
        });
        let foo_t = database
            .get_global("some::nested::name::Foo")
            .unwrap()
            .pipe(|id| {
                database
                    .get_definitions_type(id.as_global(PackageId(0)))
                    .cloned()
                    .unwrap()
            });
        let expected = Type::Function {
            args: vec![foo_t],
            ret: Box::new(Type::Int),
        };
        assert_types_eq(&database, &res, &expected);
    }
}
