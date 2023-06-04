use crate::syntax::ast::{self, Folder, Visitor};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    r#type::{Type, TypeFolder},
    type_context::TypCtx,
    typechecker::Scope, constrait_solver::ConstraintSolver,
};

#[derive(Debug)]
pub (super) enum Constraint {
    /// Defines that type t1 should be equal to t2
    Eq(Type, Type),
    /// Defines that a t.field should exist and be of type 'of_type'
    HasField {
        t: Type,
        field: String,
        of_type: Type,
    },
}

pub fn run(tctx: &mut TypCtx, ast: &ast::Program) {
    let cons = Inference::generate_constraints(tctx, ast);
    
    println!("{}", tctx.debug_string());
    let mut solver = ConstraintSolver::new();
    solver.solve(cons.constraints);
    println!("{}", tctx.debug_string());
}

pub(super) struct Inference {
    // the scope, mathes names with types
    scope: Rc<RefCell<Scope>>,
    // allows matching expression ids with types
    env: HashMap<usize, Type>,
    constraints: Vec<Constraint>,
    next_var: usize,
    ret_t: Option<Type>,
}

impl Inference {
    pub fn new(scope: Rc<RefCell<Scope>>) -> Self {
        Self {
            scope,
            env: HashMap::new(),
            constraints: Vec::new(),
            next_var: 0,
            ret_t: None,
        }
    }

    pub fn set_ret_t(&mut self, t: Option<Type>) {
        self.ret_t = t;
    }

    pub fn in_scope(
        &mut self,
        scope: Rc<RefCell<Scope>>,
        f: impl FnOnce(&mut Inference) -> Type,
    ) -> Type {
        let mut sub = Self {
            scope,
            env: HashMap::new(),
            constraints: Vec::new(),
            next_var: self.next_var,
            ret_t: None,
        };
        let ret = f(&mut sub);
        self.merge(sub);
        ret
    }

    pub fn generate_constraints(tctx: &mut TypCtx, ast: &ast::Program) -> Self {
        let replace_to_infere = &mut ReplaceInferTypes::default();
        tctx.fold_types(replace_to_infere);
        let next_id = replace_to_infere.next_id;
        let global_scope = Rc::new(RefCell::new(Scope::from_type_ctx(tctx)));
        let mut inferer = Inference::new(global_scope.clone());
        inferer.next_var = next_id;
        for item in &ast.items {
            if let ast::TopLevelItem::FunctionDefinition(fn_def) = item {
                let ast::FunctionDefinition {
                    name,
                    body,
                    parameters,
                    ..
                } = fn_def;
                let Type::Function { args, ret } = global_scope.borrow().type_of(name).expect("Expected type of function to exist") else {
                    panic!("Expected function to have a function type")
                };
                let fn_actual_type = {
                    let fn_scope = Scope::empty_scope_with_parent(global_scope.clone());
                    inferer.in_scope(fn_scope.clone(), |scope| {
                        scope.set_ret_t(Some(*(ret.clone())));
                        for (param, typ) in parameters.iter().zip(&args) {
                            fn_scope.borrow_mut().add_variable(&param, typ.clone());
                        }
                        scope.infer_in_expr(body)
                    })
                };
                inferer.constraints.push(Constraint::Eq(fn_actual_type, *ret));
            }
        }
        println!("CONSTRAINTS: \n");
        for constraint in &inferer.constraints {
            println!("{constraint:?}");
        }
        inferer

    }

    pub fn merge(&mut self, other: Self) {
        self.env.extend(other.env);
        self.constraints.extend(other.constraints);
        self.next_var = other.next_var;
    }

    pub fn infer_in_expr(&mut self, expr: &ast::Expr) -> Type {
        let id = expr.id;
        match &expr.kind {
            ast::ExprKind::Unit => {
                self.env.insert(id, Type::Unit);
                Type::Unit
            }
            ast::ExprKind::Sequence(exprs) => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                let exprs = exprs.iter().map(|expr| self.infer_in_expr(expr)).collect::<Vec<_>>();
                let last_t = exprs.last().unwrap().clone();
                self.constraints.push(Constraint::Eq(ret_t.clone(), last_t));
                ret_t 
            },
            ast::ExprKind::Assignment { lval, rval } => {
                let l_t = self.infer_in_expr(lval);
                let r_t = self.infer_in_expr(rval);
                self.constraints.push(Constraint::Eq(l_t, r_t));
                self.env.insert(id, Type::Unit);
                Type::Unit
            }
            ast::ExprKind::IntConst(_) => {
                self.env.insert(id, Type::Int);
                Type::Int
            }
            ast::ExprKind::StringConst(_) => {
                self.env.insert(id, Type::String);
                Type::String
            }
            ast::ExprKind::FloatConst(_) => {
                self.env.insert(id, Type::Float);
                Type::Float
            }
            ast::ExprKind::BoolConst(_) => {
                self.env.insert(id, Type::Bool);
                Type::Bool
            }
            ast::ExprKind::Name(name) => {
                let typ = {
                    let scope = self.scope.borrow();
                    scope.type_of(name).unwrap()
                };
                self.env.insert(id, typ.clone());
                typ
            }
            ast::ExprKind::FunctionCall { func, args, kwargs } => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                let f_t = self.infer_in_expr(func);
                let args_t = args
                    .iter()
                    .map(|arg| self.infer_in_expr(arg))
                    .collect::<Vec<_>>();
                if kwargs.len() > 0 {
                    todo!("kwargs not implemented yet")
                }
                self.constraints.push(Constraint::Eq(
                    f_t,
                    Type::Function {
                        args: args_t,
                        ret: Box::new(ret_t.clone()),
                    },
                ));
                ret_t
            }
            ast::ExprKind::Access { val, field } => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                let val_t = self.infer_in_expr(val);
                self.constraints.push(Constraint::HasField {
                    t: val_t,
                    field: field.clone(),
                    of_type: ret_t.clone(),
                });
                ret_t
            }
            ast::ExprKind::Tuple(args) => {
                let t_t = self.fresh_type();
                self.env.insert(id, t_t.clone());
                let args_t = args
                    .iter()
                    .map(|arg| self.infer_in_expr(arg))
                    .collect::<Vec<_>>();
                self.constraints
                    .push(Constraint::Eq(t_t.clone(), Type::Tuple(args_t.clone())));
                t_t
            }
            ast::ExprKind::Lambda { parameters, body } => {
                let lambda_t = self.fresh_type();
                self.env.insert(id, lambda_t.clone());
                let args_t = parameters
                    .iter()
                    .map(|ast::TypedName { typ, .. }| match &typ {
                        ast::Typ::ToInfere => self.fresh_type(),
                        typ => self.ast_type_to_type(&self.scope.borrow(), typ),
                    })
                    .collect::<Vec<_>>();
                let lambda_scope = Scope::empty_scope_with_parent(self.scope.clone());
                for (par, typ) in parameters.iter().zip(args_t.iter()) {
                    lambda_scope
                        .borrow_mut()
                        .add_variable(&par.name, typ.clone());
                }
                let body_t = self.in_scope(lambda_scope, |scope| scope.infer_in_expr(body));
                self.constraints.push(Constraint::Eq(
                    lambda_t.clone(),
                    Type::Function {
                        args: args_t,
                        ret: Box::new(body_t),
                    },
                ));
                lambda_t
            }
            ast::ExprKind::TypedExpr { expr, typ } => {
                let expr_t = self.infer_in_expr(expr);
                let typ_t = self.ast_type_to_type(&self.scope.borrow(), typ);
                self.constraints.push(Constraint::Eq(expr_t, typ_t.clone()));
                self.env.insert(id, typ_t.clone());
                typ_t
            }
            ast::ExprKind::StructLiteral { strct, values } => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                assert!(
                    strct.0.len() == 1,
                    "structs with multiple path segments not implemented yet"
                );
                let name = strct.0[0].clone();
                let strct_t = {
                    let scope = self.scope.borrow();
                    scope.lookup_type(&name).unwrap()
                };
                self.constraints
                    .push(Constraint::Eq(ret_t.clone(), strct_t.clone()));
                let args_t = values
                    .iter()
                    .map(|(name, expr)| {
                        let expr_t = self.infer_in_expr(expr);
                        (name, expr_t)
                    })
                    .collect::<HashMap<_, _>>();
                let fields_t = match strct_t {
                    Type::Struct { fields, .. } => fields,
                    _ => panic!("expected struct type"),
                };
                for (f_name, typ) in &fields_t {
                    let expr_t = args_t.get(f_name).unwrap();
                    self.constraints
                        .push(Constraint::Eq(expr_t.clone(), typ.clone()));
                }
                ret_t
            }
            ast::ExprKind::While { cond, body } => {
                self.env.insert(id, Type::Unit);
                let cond_t = self.infer_in_expr(cond);
                self.constraints.push(Constraint::Eq(cond_t, Type::Bool));
                let _ = self.infer_in_expr(body);
                Type::Unit
            }
            ast::ExprKind::Return(expr) => {
                self.env.insert(id, Type::Divergent);
                let expr_t = self.infer_in_expr(expr);
                self.constraints
                    .push(Constraint::Eq(expr_t.clone(), self.ret_t.clone().unwrap()));
                Type::Divergent
            }
            ast::ExprKind::If {
                cond,
                body,
                else_body,
            } => {
                let if_t = self.fresh_type();
                self.env.insert(id, if_t.clone());
                let cond_t = self.infer_in_expr(cond);
                self.constraints.push(Constraint::Eq(cond_t, Type::Bool));
                let body_t = self.infer_in_expr(body);
                match else_body {
                    None => {
                        self.constraints
                            .push(Constraint::Eq(if_t.clone(), Type::Unit));
                    }
                    Some(else_body) => {
                        let else_body_t = self.infer_in_expr(else_body);
                        self.constraints
                            .push(Constraint::Eq(body_t.clone(), else_body_t));
                        self.constraints.push(Constraint::Eq(if_t.clone(), body_t));
                    }
                }
                if_t
            }
            ast::ExprKind::Let { name, typ, init } => {
                self.env.insert(id, Type::Unit);
                let init_t = self.infer_in_expr(init);
                let let_t = match typ {
                    ast::Typ::ToInfere => init_t.clone(),
                    t => self.ast_type_to_type(&self.scope.borrow(), t),
                };
                self.constraints.push(Constraint::Eq(init_t, let_t.clone()));
                self.scope.borrow_mut().add_variable(name, let_t.clone());
                Type::Unit
            }
            ast::ExprKind::AnonType { .. } => {
                panic!("There should be no anon types when type checking")
            }
        }
    }

    fn fresh_type(&mut self) -> Type {
        let var = self.next_var;
        self.next_var += 1;
        Type::Fresh(var)
    }

    fn ast_type_to_type(&self, scope: &Scope, typ: &ast::Typ) -> Type {
        // convert ast::Typ to Type
        match typ {
            ast::Typ::Unit => Type::Unit,
            ast::Typ::Name(name) => scope.lookup_type(name).expect("unknown type"),
            ast::Typ::Application { .. } => {
                todo!()
            }
            ast::Typ::Array(t) => {
                let t = Box::new(self.ast_type_to_type(scope, t));
                Type::Array(t)
            }
            ast::Typ::Function {
                args,
                ret,
                generics: _generics,
            } => {
                let args = args
                    .iter()
                    .map(|typ| self.ast_type_to_type(scope, typ))
                    .collect::<Vec<_>>();
                let ret = Box::new(self.ast_type_to_type(scope, ret));
                Type::Function { args, ret }
            }
            ast::Typ::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|typ| self.ast_type_to_type(scope, typ))
                    .collect::<Vec<_>>();
                Type::Tuple(types)
            }
            ast::Typ::ToInfere => panic!("ToInfer types should not be present when typechecking"),
        }
    }
}

#[derive(Default)]
struct ReplaceInferTypes {
    next_id: usize,
}

impl TypeFolder for ReplaceInferTypes {
    fn fold_to_infere(&mut self) -> Type {
        let id = self.next_id;
        self.next_id += 1;
        Type::Fresh(id)
    }
}
