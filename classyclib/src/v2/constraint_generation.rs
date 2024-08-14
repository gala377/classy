use std::collections::{HashMap, HashSet};

use classy_syntax::ast;

use crate::{
    session::Session,
    typecheck::ast_to_type::PrefexScope,
    v2::{
        constraint_solver::ConstraintSolver,
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
    },
    MethodOrGlobal {
        receiver: Type,
        name: String,
        of_ty: Type,
    },
}

#[repr(transparent)]
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct ExprId(usize);

pub struct Inferer<'sess, 'db> {
    constraints: Vec<Constraint>,
    expr_types: HashMap<ExprId, Type>,
    scope: NameScope,
    session: &'sess Session,
    database: &'db mut Database,
    current_namespace: Vec<String>,
    receiver: Option<Type>,
    prefex_scope: PrefexScope,
    function_return_type: Type,

    // Can be a method def id if we are inferring a method
    // TODO Both of those are useless now but when we create a solver from this inferer
    // TODO both of them will be passes to the solver
    function_def_id: Id<DefinitionId>,
    constraints_in_scope: Vec<GenericConstraint>,
    /* TODO:
    - Add constraints in scope, that takes into account function and
        outer block constraints (method blocks, instances)
    - Add receiver type, this is important for method blocks.
    */
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
        constraints_in_scope: Vec<GenericConstraint>,
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
        }
    }

    pub fn for_function(
        id: Id<DefinitionId>,
        database: &'db mut Database,
        session: &'sess Session,
    ) -> Self {
        fn gather(
            database: &Database,
            id: Id<DefinitionId>,
            constraints: &mut Vec<GenericConstraint>,
            prefex_scope: &mut PrefexScope,
        ) -> Option<Id<TypeId>> {
            let definition = database.get_definition(id).unwrap();
            let parent = definition.parent;
            if let Some(parent) = parent {
                gather(
                    database,
                    parent.as_global(id.package),
                    constraints,
                    prefex_scope,
                );
            }
            let mut def_receiver = None;
            match definition.kind {
                DefinitionKind::Function(_) | DefinitionKind::Method(_) => {
                    let ty = database.get_definitions_type(id).unwrap();
                    if let Type::Scheme { prefex, .. } = ty {
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
                        prefex_scope.new_scope();
                        prefex_scope.add_type_vars(&free_vars);
                    }
                }
                DefinitionKind::Instance(InstanceInfo { free_vars, .. }) => {
                    if !free_vars.is_empty() {
                        prefex_scope.new_scope();
                        prefex_scope.add_type_vars(&free_vars);
                    }
                }
                kind => panic!("Unexpected definition kind: {kind:?}"),
            }
            constraints.extend_from_slice(&definition.constraints);
            def_receiver
        }

        let def = database.get_definition(id).unwrap();
        let namespace = database.get_namespace(id.as_local().unwrap()).to_vec();
        let mut constraints = Vec::new();
        let mut prefex_scope = PrefexScope::without_scope();
        let receiver = gather(database, id, &mut constraints, &mut prefex_scope)
            .map(|r| database.resolve_alias_to_type(r).unwrap());
        let (args_ty, ret_ty) = match database.get_definitions_type(id).cloned() {
            Some(Type::Function { args, ret }) => (args, *ret),
            Some(Type::Scheme {
                typ: box Type::Function { args, ret },
                ..
            }) => (args, *ret),
            t => panic!("Expected function, got {:?}", t),
        };
        let arg_names = match def.kind {
            DefinitionKind::Method(MethodInfo { arg_names, .. }) => arg_names.clone(),
            DefinitionKind::Function(FunctionInfo { arg_names }) => arg_names.clone(),
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
        let body = &self
            .database
            .function_definitions
            .get(&self.function_def_id.as_local().unwrap())
            .unwrap()
            .body
            .clone();
        self.infer_expr(&body);
    }

    pub fn infer_expr(&mut self, ast::Expr { id, kind }: &ast::Expr) -> Type {
        let ty: Type = match kind {
            ast::ExprKind::Unit => Type::Unit,
            ast::ExprKind::Sequence(exprs) => exprs
                .iter()
                .map(|expr| self.infer_expr(expr))
                .last()
                .unwrap(),
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
                        Some(ty) => ty,
                        None => self
                            .database
                            .get_type_by_unresolved_name(&self.current_namespace, path, identifier)
                            .cloned()
                            .unwrap_or_else(|| {
                                panic!("Name {path:?}::{identifier} not found in database")
                            }),
                    }
                }
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
                ast::Name::Local(name) => self
                    .scope
                    .get_variable(name)
                    .cloned()
                    .unwrap_or_else(|| panic!("Variable {name} not found in scope")),
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
                });
                ret_ty
            }
            ast::ExprKind::FunctionCall {
                func,
                args,
                kwargs: _,
            } => {
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
                self.add_constraint(Constraint::Eq(ret.clone(), strct_t.clone()));
                if let Type::Alias(for_t) = strct_t {
                    let tid = self.database.resolve_alias(for_t).unwrap();
                    strct_t = self.database.resolve_tid(tid).unwrap();
                }
                let Type::Struct { def, fields } = strct_t else {
                    panic!("Expected struct type, got {strct_t:?}");
                };
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
                Type::Unit
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
                init_ty
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
            ast::ExprKind::Match { expr: _, cases: _ } => todo!(),
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
