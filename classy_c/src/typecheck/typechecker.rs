use std::collections::HashMap;

use crate::syntax::ast::{self, Visitor};

use super::{
    r#type::Type,
    type_context::{TypCtx},
    types_eq,
};

struct Scope<'s> {
    type_ctx: TypCtx,
    resolved_types: HashMap<String, Type>,
    resolved_vars: HashMap<String, Type>,
    parent: Option<&'s Scope<'s>>,
}

impl<'s> Scope<'s> {
    pub fn from_type_ctx(type_ctx: TypCtx) -> Self {
        let resolved_vars = {
            type_ctx
                .variables
                .iter()
                .map(|(name, typ_id)| {
                    let typ = type_ctx.definitions.get(typ_id).unwrap().clone();
                    (name.clone(), typ)
                })
                .collect()
        };
        let resolved_types = {
            type_ctx
                .types
                .iter()
                .map(|(name, typ_id)| {
                    let typ = type_ctx.definitions.get(typ_id).unwrap().clone();
                    (name.clone(), typ)
                })
                .collect()
        };
        Self {
            type_ctx,
            resolved_types,
            resolved_vars,
            parent: None,
        }
    }

    pub fn empty_scope_with_parent<'outer: 's>(parent: &'outer Scope) -> Self {
        Self {
            type_ctx: TypCtx::new(),
            resolved_types: HashMap::new(),
            resolved_vars: HashMap::new(),
            parent: Some(&parent),
        }
    }

    pub fn type_of(&self, name: &str) -> Option<Type> {
        self.resolved_types
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.type_of(name)))
    }

    pub fn lookup_type(&self, name: &str) -> Option<Type> {
        let Some(id) = self.type_ctx.types.get(name).cloned() else {
            return self.parent.as_ref().and_then(|parent| parent.lookup_type(name));
        };
        self.type_ctx.definitions.get(&id).cloned()
    }

    pub fn add_variable(&mut self, name: &str, typ: Type) {
        self.resolved_vars.insert(name.to_owned(), typ);
    }
}

pub struct TypeChecker<'s> {
    scope: Scope<'s>,
    functions_ret_type: Option<Type>,
}

impl TypeChecker<'static> {
    pub fn new(type_ctx: TypCtx) -> Self {
        Self {
            scope: Scope::from_type_ctx(type_ctx),
            functions_ret_type: None,
        }
    }
}

impl<'s> TypeChecker<'s> {
    pub fn new_scope<'this: 's>(&'this self) -> TypeChecker<'this> {
        Self {
            scope: Scope::empty_scope_with_parent(&self.scope),
            functions_ret_type: None,
        }
    }

    pub fn add_variable(&mut self, name: &str, typ: Type) {
        self.scope.add_variable(name, typ)
    }

    pub fn typecheck_expr(&mut self, expr: &ast::Expr) -> Type {
        match expr {
            ast::Expr::Unit => Type::Unit,
            ast::Expr::Sequence(exprs) => exprs
                .iter()
                .map(|expr| self.typecheck_expr(expr))
                .last()
                .unwrap_or(Type::Unit),
            ast::Expr::Assignment { lval, rval } => {
                let lval_t = self.typecheck_expr(lval);
                let rval_t = self.typecheck_expr(rval);
                if !types_eq(&self.scope.type_ctx, &lval_t, &rval_t) {
                    panic!("Expected type {:?} but got {:?}", lval_t, rval_t)
                }
                Type::Unit
            }
            ast::Expr::IntConst(_) => Type::Int,
            ast::Expr::StringConst(_) => Type::String,
            ast::Expr::FloatConst(_) => Type::Float,
            ast::Expr::Name(name) => self
                .scope
                .type_of(name)
                .expect("Expected type of variable to exist"),
            ast::Expr::FunctionCall { func, args } => {
                let func_t = self.typecheck_expr(func);
                let args_t = args
                    .iter()
                    .map(|arg| self.typecheck_expr(arg))
                    .collect::<Vec<_>>();
                match func_t {
                    Type::Function {
                        args: expected_args,
                        ret,
                    } => {
                        if expected_args.len() != args_t.len() {
                            panic!(
                                "Expected {} arguments but got {}",
                                expected_args.len(),
                                args_t.len()
                            )
                        }
                        for (expected_arg, arg) in expected_args.iter().zip(args_t) {
                            if !types_eq(&self.scope.type_ctx, &expected_arg, &arg) {
                                panic!("Expected type {:?} but got {:?}", expected_arg, arg)
                            }
                        }
                        *ret
                    }
                    _ => panic!("Expected function type but got {:?}", func_t),
                }
            }
            ast::Expr::Access { val, field } => {
                let val_t = self.typecheck_expr(val);
                match val_t {
                    Type::Struct { fields, .. } => {
                        let field_t = fields
                            .iter()
                            .find(|(name, _)| name == field)
                            .map(|(_, typ)| typ)
                            .expect("Expected field to exist");
                        field_t.clone()
                    }
                    _ => panic!("Expected struct type but got {:?}", val_t),
                }
            }
            ast::Expr::Tuple(exprs) => {
                let types = exprs
                    .iter()
                    .map(|expr| self.typecheck_expr(expr))
                    .collect::<Vec<_>>();
                Type::Tuple(types)
            }
            ast::Expr::Lambda { .. } => todo!(),
            ast::Expr::TypedExpr { expr, typ } => {
                let expr_t = self.typecheck_expr(expr);
                let typ_t = self.ast_type_to_type(typ);
                if !types_eq(&self.scope.type_ctx, &expr_t, &typ_t) {
                    panic!("Expected type {:?} but got {:?}", typ_t, expr_t)
                }
                expr_t
            }
            ast::Expr::StructLiteral { .. } => todo!(),
            ast::Expr::While { cond, body } => {
                let cond_t = self.typecheck_expr(cond);
                if cond_t != Type::Bool {
                    panic!("Expected type {:?} but got {:?}", Type::Bool, cond_t)
                }
                let body_t = self.typecheck_expr(body);
                if body_t != Type::Unit {
                    panic!("Expected type {:?} but got {:?}", Type::Unit, body_t)
                }
                Type::Unit
            }
            ast::Expr::Return(expr) => {
                let expr_t = self.typecheck_expr(expr);
                let ret_t = self
                    .functions_ret_type
                    .clone()
                    .expect("Expected return type to be set");
                if !types_eq(&self.scope.type_ctx, &expr_t, &ret_t) {
                    panic!("Expected type {:?} but got {:?}", ret_t, expr_t)
                }
                ret_t
            }
            ast::Expr::If {
                cond,
                body,
                else_body,
            } => {
                let cond_t = self.typecheck_expr(cond);
                if cond_t != Type::Bool {
                    panic!("Expected type {:?} but got {:?}", Type::Bool, cond_t)
                }
                let body_t = self.typecheck_expr(body);
                let else_body_t = else_body
                    .as_ref()
                    .map(|e| self.typecheck_expr(e))
                    .unwrap_or(Type::Unit);
                if !types_eq(&self.scope.type_ctx, &body_t, &else_body_t) {
                    panic!("Expected type {:?} but got {:?}", body_t, else_body_t)
                }
                body_t
            }
            ast::Expr::Let { name, typ, init } => {
                let init_t = self.typecheck_expr(init);
                let typ_t = match typ {
                    ast::Typ::ToInfere => init_t,
                    _ => {
                        let typ_t = self.ast_type_to_type(typ);
                        if !types_eq(&self.scope.type_ctx, &init_t, &typ_t) {
                            panic!("Expected type {:?} but got {:?}", typ_t, init_t)
                        }
                        typ_t
                    }
                };
                self.scope.add_variable(name, typ_t.clone());
                typ_t
            }
        }
    }

    fn ast_type_to_type(&self, typ: &ast::Typ) -> Type {
        // convert ast::Typ to Type
        match typ {
            ast::Typ::Unit => Type::Unit,
            ast::Typ::Name(name) => self.scope.lookup_type(name).expect("unknown type"),
            ast::Typ::Application { .. } => {
                todo!()
            }
            ast::Typ::Array(t) => {
                let t = Box::new(self.ast_type_to_type(t));
                Type::Array(t)
            }
            ast::Typ::Function { args, ret } => {
                let args = args
                    .iter()
                    .map(|typ| self.ast_type_to_type(typ))
                    .collect::<Vec<_>>();
                let ret = Box::new(self.ast_type_to_type(ret));
                Type::Function { args, ret }
            }
            ast::Typ::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|typ| self.ast_type_to_type(typ))
                    .collect::<Vec<_>>();
                Type::Tuple(types)
            }
            ast::Typ::ToInfere => todo!(),
        }
    }
}

impl<'ast, 'parent> Visitor<'ast> for TypeChecker<'parent> {
    fn visit_type_def(&mut self, _node: &'ast ast::TypeDefinition) {}

    fn visit_fn_def(&mut self, fn_def: &'ast ast::FunctionDefinition) {
        let ast::FunctionDefinition {
            name,
            body,
            parameters,
            ..
        } = fn_def;
        let Type::Function { args, ret } = self.scope.type_of(name).expect("Expected type of function to exist") else {
            panic!("Expected function to have a function type")
        };
        let mut fn_scope = self.new_scope();
        fn_scope.functions_ret_type = Some(*ret.clone());
        for (param, typ) in parameters.iter().zip(args) {
            fn_scope.add_variable(&param, typ);
        }
        let actual_type = fn_scope.typecheck_expr(body);
        if !types_eq(&self.scope.type_ctx, &actual_type, &ret) {
            panic!(
                "Expected function {} to return type {:?} but got {:?}",
                name, ret, actual_type
            )
        }
    }
}
