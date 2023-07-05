use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast_passes,
    syntax::ast::{self, Visitor},
    typecheck::{
        constrait_solver::ConstraintSolver,
        fix_fresh,
        r#type::{Type, TypeFolder},
        scope::Scope,
        type_context::TypCtx,
    },
};

#[derive(Debug)]
pub(super) enum Constraint {
    /// Defines that type t1 should be equal to t2
    Eq(Type, Type),
    /// Defines that a t.field should exist and be of type 'of_type'
    HasField {
        t: Type,
        field: String,
        of_type: Type,
    },
}

/// Maps unique node id in the ast with its type.
pub type TypeEnv = HashMap<usize, Type>;

pub fn run(tctx: &mut TypCtx, ast: &ast::Program) -> TypeEnv {
    tctx.remove_to_infere_type();
    let cons = Inference::generate_constraints(tctx, ast);

    println!("{}", tctx.debug_string());
    let mut solver = ConstraintSolver::new(tctx);
    solver.solve(cons.constraints);
    let mut substs = solver.substitutions.iter().cloned().collect();
    let mut env = cons.env;
    fix_fresh::fix_types_after_inference(&mut substs, tctx, &mut env);
    println!("SUBSTITUTIONS");
    for (id, typ) in substs.iter() {
        println!("{} -> {:?}", id, typ);
    }
    println!("{}", tctx.debug_string());
    println!("ENV IS:");
    for (id, typ) in env.iter() {
        println!("{} -> {:?}", id, typ);
    }
    env
}

pub(super) struct Inference {
    // the scope, maps names to types
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
            ret_t: self.ret_t.clone(),
        };
        let ret = f(&mut sub);
        self.merge(sub);
        ret
    }

    pub fn generate_constraints(tctx: &mut TypCtx, ast: &ast::Program) -> Self {
        // create fresh variables for anonymous types
        let replace_to_infere = &mut ReplaceInferTypes::default();
        tctx.fold_types(replace_to_infere);
        let next_id = replace_to_infere.next_id;
        let global_scope = Rc::new(RefCell::new(Scope::from_type_ctx(tctx)));
        let mut inferer = Inference::new(global_scope.clone());
        inferer.next_var = next_id;

        for item in &ast.items {
            match item {
                ast::TopLevelItem::FunctionDefinition(fn_def) => {
                    let ast::FunctionDefinition {
                        name,
                        body,
                        parameters,
                        attributes,
                        ..
                    } = fn_def;
                    let Type::Function { args, ret } = global_scope.borrow().type_of(name).expect("Expected type of function to exist") else {
                    panic!("Expected function to have a function type")
                };
                    if attributes.contains(&"runtime".to_owned()) {
                        continue;
                    }
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
                    inferer
                        .constraints
                        .push(Constraint::Eq(fn_actual_type, *ret));
                }
                _ => {}
            }
        }

        // TODO: UNCOMMENT IF NEEDED? (I think it's not needed)
        // It does not seem like the function ordering is actually
        // needed for anything
        //
        //
        // let mut order = ast_passes::order_functions::FunctionsOrderer::new();
        // order.visit(ast);
        // let function_check_order = order.order();
        // let items_mapping = ast
        //     .items
        //     .iter()
        //     .enumerate()
        //     .filter_map(|(i, item)| match item {
        //         ast::TopLevelItem::FunctionDefinition(fn_def) => {
        //             let ast::FunctionDefinition { name, .. } = fn_def;
        //             Some((name.clone(), i))
        //         }
        //         _ => None,
        //     })
        //     .collect::<HashMap<_, _>>();
        // for name in &function_check_order {
        //     let index = items_mapping.get(name).expect("Expected function to exist");
        //     let ast::TopLevelItem::FunctionDefinition(fn_def) = &ast.items[*index] else {
        //         panic!("unexpected");
        //     };
        //     let ast::FunctionDefinition {
        //         name,
        //         body,
        //         parameters,
        //         attributes,
        //         ..
        //     } = fn_def;
        //     let Type::Function { args, ret } = global_scope.borrow().type_of(name).expect("Expected type of function to exist") else {
        //             panic!("Expected function to have a function type")
        //         };
        //     if attributes.contains(&"runtime".to_owned()) {
        //         continue;
        //     }
        //     let fn_actual_type = {
        //         let fn_scope = Scope::empty_scope_with_parent(global_scope.clone());
        //         inferer.in_scope(fn_scope.clone(), |scope| {
        //             scope.set_ret_t(Some(*(ret.clone())));
        //             for (param, typ) in parameters.iter().zip(&args) {
        //                 fn_scope.borrow_mut().add_variable(&param, typ.clone());
        //             }
        //             scope.infer_in_expr(body)
        //         })
        //     };
        //     inferer
        //         .constraints
        //         .push(Constraint::Eq(fn_actual_type, *ret));
        // }
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
                let exprs = exprs
                    .iter()
                    .map(|expr| self.infer_in_expr(expr))
                    .collect::<Vec<_>>();
                let last_t = exprs.last().unwrap().clone();
                self.constraints.push(Constraint::Eq(ret_t.clone(), last_t));
                ret_t
            }
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
                let _ = self.in_scope(Scope::empty_scope_with_parent(self.scope.clone()), |this| {
                    this.infer_in_expr(body)
                });
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
                let body_t = self
                    .in_scope(Scope::empty_scope_with_parent(self.scope.clone()), |this| {
                        this.infer_in_expr(body)
                    });
                match else_body {
                    None => {
                        self.constraints
                            .push(Constraint::Eq(if_t.clone(), Type::Unit));
                    }
                    Some(else_body) => {
                        let else_body_t = self
                            .in_scope(Scope::empty_scope_with_parent(self.scope.clone()), |this| {
                                this.infer_in_expr(else_body)
                            });
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
            ast::ExprKind::ArrayLiteral { typ, size, init } => {
                let array_t = self.fresh_type();
                self.env.insert(id, array_t.clone());
                let size_t = self.infer_in_expr(size);
                self.constraints.push(Constraint::Eq(size_t, Type::Int));
                let array_inner_t = match typ {
                    ast::Typ::ToInfere => self.fresh_type(),
                    t => self.ast_type_to_type(&self.scope.borrow(), t),
                };
                self.constraints.push(Constraint::Eq(
                    array_t.clone(),
                    Type::Array(Box::new(array_inner_t.clone())),
                ));
                let init_types: Vec<_> = init.iter().map(|expr| self.infer_in_expr(expr)).collect();
                for t in init_types {
                    self.constraints
                        .push(Constraint::Eq(t, array_inner_t.clone()));
                }
                array_t
            }
            ast::ExprKind::AnonType { .. } => {
                panic!("There should be no anon types when type checking")
            }
            ast::ExprKind::IndexAccess { lhs, index } => {
                let inner_t = self.fresh_type();
                self.env.insert(id, inner_t.clone());
                let lhs_t = self.infer_in_expr(lhs);
                let index_t = self.infer_in_expr(index);
                self.constraints.push(Constraint::Eq(index_t, Type::Int));
                self.constraints.push(Constraint::Eq(
                    lhs_t,
                    Type::Array(Box::new(inner_t.clone())),
                ));
                inner_t
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

#[cfg(test)]
mod tests {
    use crate::{
        ast_passes::{self},
        syntax::{lexer::Lexer, parser::Parser},
        typecheck,
    };

    fn run_typechecker(source: &str) {
        let lex = Lexer::new(source);
        let mut parser = Parser::new(lex);
        let res = parser.parse().unwrap();
        let res = ast_passes::run_befor_type_context_passes(res);
        let mut tctx = typecheck::prepare_for_typechecking(&res);
        let res = ast_passes::run_before_typechecking_passes(&tctx, res);
        typecheck::inference::run(&mut tctx, &res);
        println!("Final ast {res:#?}");
    }

    #[test]
    fn function_returning_literal_typechecks() {
        let source = r#"
            main: () -> Int
            main = 1
        "#;
        run_typechecker(source);
    }

    #[test]
    #[should_panic]
    fn function_returning_wrong_type_panics() {
        let source = r#"
            main: () -> Int
            main = "Hello world"
        "#;
        run_typechecker(source);
    }

    #[test]
    fn variable_defined_in_if_does_not_propagate_type() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let a = 1
                if (true) {
                    let a = "Hello world"
                    is_string a
                }
                is_int a
            }
        "#;
        run_typechecker(source);
    }

    #[test]
    fn return_in_if_diverges_and_typechecks() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            main: () -> ()
            main {
                let a = if (true) {
                    return ()
                } else {
                    "Hello"
                }
                is_string a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    fn return_type_of_if_is_based_on_both_branches() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            main: () -> ()
            main {
                let a = if (true) {
                    "Hello"
                } else {
                    "World"
                }
                is_string a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    #[should_panic]
    fn return_type_of_if_is_based_on_both_branches_error() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            main: () -> ()
            main {
                let a = if (true) {
                    "Hello"
                } else {
                    1
                }
                is_string a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    fn assigning_functions_typecheck() {
        let source = r#"
            is_function: (() -> ()) -> ()
            is_function f = ()

            main: () -> ()
            main {
                let a = main
                is_function main
                is_function a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    fn struct_literals_typecheck() {
        let source = r#"
            type Foo {
                a: Int
                b: String
            }

            is_foo: (Foo) -> ()
            is_foo f = ()

            main: () -> ()
            main {
                let a = Foo(a = 1, b = "Hello")
                is_foo a
            }
        "#;
        run_typechecker(source);
    }

    #[test]
    fn anon_types_typecheck() {
        let source = r#"
            type Foo {
                a: Int
                b: String
            }

            is_string: (String) -> ()
            is_string s = ()

            is_foo: (Foo) -> ()
            is_foo f = ()

            main: () -> ()
            main {
                let a = Foo(a = 1, b = "Hello")
                let anon = type { foo = a; bar = "World" }
                is_foo anon.foo
                is_string anon.bar
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn nested_anon_types_typecheck() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()

            main: () -> ()
            main {
                let anon = type { a = type { b = type { c = "Hello" } } }
                is_string anon.a.b.c
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn check_typechecking_arrays_with_externally_provided_type() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            main: () -> ()
            main {
                let arr = array[0]Int
                is_arr arr
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn check_typechecking_arrays_with_type_inferred_by_call() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            main: () -> ()
            main {
                let arr = array[0]
                is_arr arr
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    #[should_panic]
    fn check_typechecking_fails_arrays_with_assigned_typed() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            is_arr_2: ([String]) -> ()
            is_arr_2 a = ()

            main: () -> ()
            main {
                let arr = array[0]
                is_arr arr
                is_arr_2 arr 
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn check_array_with_type_by_elements() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            main: () -> ()
            main {
                let arr = array{1, 2}
                is_arr arr
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn infer_type_based_on_access_call() {
        let source = r#"
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let arr = array[1]
                is_int arr[0]
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    #[should_panic]
    fn infer_type_based_on_access_assign_rhs() {
        let source = r#"
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let arr = array[1]
                let a: String = arr[0]
                is_int arr[0]
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn infer_type_based_on_access_assign_lhs() {
        let source = r#"
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let arr = array[1]
                arr[0] = 1
                is_int arr[0]
            }
        "#;
        run_typechecker(source)
    }

    // #[test]
    // fn generic_functions() {
    //     let source = r#"
    //         is_string: (String) -> ()
    //         is_string s = ()

    //         id: forall a => (a) -> a
    //         id a = a

    //         main: () -> ()
    //         main {
    //             is_string(id "hello")
    //         }
    //     "#;
    //     run_typechecker(source)
    // }
}
