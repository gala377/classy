use std::collections::HashMap;

use classy_syntax::ast;

use crate::{
    scope::Scope,
    session::{self, Session},
    typecheck::ast_to_type::PrefexScope,
    v2::knowledge::Database,
};

use super::{
    knowledge::{self, DefinitionId, Id, PackageId, CURRENT_PACKAGE_ID},
    name_scope::NameScope,
    ty::Type,
};

enum Constraint {
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
}

#[repr(transparent)]
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
struct ExprId(usize);

struct Inferer<'sess, 'db> {
    constraints: Vec<Constraint>,
    expr_types: HashMap<ExprId, Type>,
    scope: NameScope,
    session: &'sess Session,
    database: &'db mut Database,
    current_namespace: Vec<String>,
    prefex_scope: PrefexScope,
    function_return_type: Type,
}

impl<'sess, 'db> Inferer<'sess, 'db> {
    fn new(
        session: &'sess Session,
        database: &'db mut Database,
        current_namespace: &[String],
        prefex_scope: PrefexScope,
        function_return_type: Type,
    ) -> Self {
        Self {
            constraints: Vec::new(),
            expr_types: HashMap::new(),
            scope: NameScope::new(),
            session,
            database,
            current_namespace: current_namespace.to_vec(),
            prefex_scope,
            function_return_type,
        }
    }

    fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    fn add_expr_type(&mut self, expr: ExprId, ty: Type) {
        self.expr_types.insert(expr, ty);
    }

    fn new_fresh_type(&self) -> Type {
        Type::Fresh(self.session.id_provider().next())
    }

    fn infer_expr(&mut self, ast::Expr { id, kind }: &ast::Expr) -> Type {
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
                ast::Name::Unresolved { path, identifier } if path.is_empty() => {
                    let possibly_local = self.scope.get_variable(&identifier).cloned();
                    match possibly_local {
                        Some(ty) => ty,
                        None => self
                            .database
                            .get_type_by_unresolved_name(&self.current_namespace, path, &identifier)
                            .cloned()
                            .expect(&format!(
                                "Name {path:?}::{identifier} not found in database"
                            )),
                    }
                }
                ast::Name::Unresolved { path, identifier } => self
                    .database
                    .get_type_by_unresolved_name(&self.current_namespace, path, &identifier)
                    .cloned()
                    .expect(&format!(
                        "Name {path:?}::{identifier} not found in database"
                    )),
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
                    self.database.get_type(id).cloned().unwrap()
                }
                ast::Name::Local(name) => self
                    .scope
                    .get_variable(name)
                    .cloned()
                    .expect(&format!("Variable {name} not found in scope")),
            },
            ast::ExprKind::FunctionCall { func, args, kwargs } => {
                let func_ty = self.infer_expr(func);
                let arg_tys = args.iter().map(|arg| self.infer_expr(arg)).collect();
                let ret_ty = self.new_fresh_type();
                let inferred_func_ty = Type::Function {
                    args: arg_tys,
                    ret: Box::new(ret_ty.clone()),
                };
                self.add_constraint(Constraint::Eq(func_ty, inferred_func_ty.clone()));
                inferred_func_ty
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
            ast::ExprKind::StructLiteral { strct, values } => todo!(),
            ast::ExprKind::AdtTupleConstructor {
                typ,
                constructor,
                args,
            } => todo!(),
            ast::ExprKind::AdtStructConstructor {
                typ,
                constructor,
                fields,
            } => todo!(),
            ast::ExprKind::AdtUnitConstructor { typ, constructor } => todo!(),
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
            } => todo!(),
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
            ast::ExprKind::LetRec { definitions } => todo!(),
            ast::ExprKind::AnonType { fields } => todo!(),
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
            ast::ExprKind::Match { expr, cases } => todo!(),
            ast::ExprKind::MethodCall {
                receiver,
                method,
                args,
                kwargs,
            } => todo!(),
        };
        self.add_expr_type(ExprId(*id), ty.clone());
        ty
    }
}

#[cfg(test)]
mod tests {
    use self::knowledge::{Definition, DefinitionKind, LocalId, PackageInfo, TypeId};

    use super::*;
    use crate::v2::compile::Compiler;
    use crate::v2::knowledge::Database;
    use classy_syntax::ast;
    use std::collections::HashMap;

    const SOURCE: &str = r#"
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

    fn setup_database() -> (Database, Session) {
        let std_package = prepare_std_package();
        let compiler = Compiler::new(
            "test",
            vec![("test".into(), SOURCE.into())],
            vec![std_package],
        );
        let (db, sess) = compiler.make_database();
        for (name, id) in db.globals.iter() {
            println!("{name}: {id:?}");
            let def = db.get_global(name).unwrap();
            let ty = db.get_type(def.as_global(PackageId(0)));
            println!("type: {ty:?}");
        }
        (db, sess)
    }

    fn simple_inferer<'sess, 'db>(
        database: &'db mut Database,
        session: &'sess Session,
    ) -> Inferer<'sess, 'db> {
        Inferer::new(session, database, &[], PrefexScope::new(), Type::Unit)
    }

    #[test]
    fn infer_intlit() {
        let (mut database, session) = setup_database();
        let mut inferer = simple_inferer(&mut database, &session);
        let res = inferer.infer_expr(&ast::Expr {
            id: 0,
            kind: ast::ExprKind::IntConst(0),
        });
        assert_eq!(res, Type::Int);
    }

    #[test]
    fn infer_function_t() {
        let (mut database, session) = setup_database();
        let mut inferer = simple_inferer(&mut database, &session);
        let res = inferer.infer_expr(&ast::Expr {
            id: 0,
            kind: ast::ExprKind::Name(ast::Name::Unresolved {
                path: vec![],
                identifier: "foo".to_string(),
            }),
        });
        assert_eq!(
            res,
            Type::Function {
                args: vec![Type::Int, Type::Int],
                ret: Box::new(Type::Int)
            }
        );
    }
}
