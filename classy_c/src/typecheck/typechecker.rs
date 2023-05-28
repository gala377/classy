use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::syntax::ast::{self, Visitor};

use super::{
    r#type::Type,
    type_context::{TypCtx, TypeId},
    types_eq,
};

struct Scope {
    resolved_types: HashMap<String, Type>,
    resolved_vars: HashMap<String, Type>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn from_type_ctx(type_ctx: &TypCtx) -> Self {
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
            resolved_types,
            resolved_vars,
            parent: None,
        }
    }

    pub fn empty_scope_with_parent(parent: Rc<RefCell<Scope>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            resolved_types: HashMap::new(),
            resolved_vars: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn type_of(&self, name: &str) -> Option<Type> {
        self.resolved_vars.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().type_of(name))
        })
    }

    pub fn lookup_type(&self, name: &str) -> Option<Type> {
        self.resolved_types.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().lookup_type(name))
        })
    }

    pub fn add_variable(&mut self, name: &str, typ: Type) {
        self.resolved_vars.insert(name.to_owned(), typ);
    }

    pub fn types_eq(&self, tctx: &TypCtx, a: &Type, b: &Type) -> bool {
        types_eq(tctx, a, b)
            || self
                .parent
                .as_ref()
                .map_or(false, |parent| parent.borrow().types_eq(tctx, a, b))
    }

    pub fn resolve_type_id(&self, tctx: &TypCtx, typ_id: TypeId) -> Option<Type> {
        tctx.definitions.get(&typ_id).cloned().or_else(|| {
            self.parent
                .as_ref()
                .map(|parent| parent.borrow().resolve_type_id(tctx, typ_id))
                .flatten()
        })
    }

    pub fn resolve_alias(&self, tctx: &TypCtx, for_type: TypeId) -> Type {
        let mut t = Type::Alias(for_type);
        while let Type::Alias(for_type) = t {
            t = self
                .resolve_type_id(tctx, for_type)
                .expect("Expected type to exist");
        }
        t
    }

    pub fn update_type(&mut self, name: &str, typ: Type) {
        if self.resolved_types.contains_key(name) {
            self.resolved_types.insert(name.to_owned(), typ);
        } else {
            self.parent
                .as_ref()
                .map(|parent| parent.borrow_mut().update_type(name, typ));
        }
    }
}

pub struct TypeChecker<'ctx> {
    tctx: &'ctx mut TypCtx,
    functions_ret_type: Option<Type>,
    typed_ast: ast::typed::Program,
}

impl<'ctx> TypeChecker<'ctx> {
    pub fn new(type_ctx: &'ctx mut TypCtx) -> Self {
        Self {
            tctx: type_ctx,
            functions_ret_type: None,
            typed_ast: ast::typed::Program::default(),
        }
    }

    pub fn into_typed_ast(self) -> ast::typed::Program {
        self.typed_ast
    }
}

impl<'ctx> TypeChecker<'ctx> {
    fn add_variable(&mut self, scope: &mut Scope, name: &str, typ: Type) {
        scope.add_variable(name, typ)
    }

    fn update_struct_field_type(&mut self, scope: &mut Scope, strct: &str, field: &str, typ: Type) {
        let t_id = self.tctx.types.get(strct).expect("Expected type to exist");
        let strc_type = self
            .tctx
            .definitions
            .get_mut(t_id)
            .expect("Expected type to exist");
        match strc_type {
            Type::Struct { fields, .. } => {
                let field_t = fields
                    .iter_mut()
                    .find(|(name, _)| name == field)
                    .map(|(_, typ)| typ)
                    .expect("Expected field to exist");
                *field_t = typ;
            }
            _ => panic!("Expected struct type but got {:?}", strc_type),
        }
        scope.update_type(strct, strc_type.clone());
    }

    fn typecheck_expr(
        &mut self,
        scope: Rc<RefCell<Scope>>,
        expr: &ast::Expr,
    ) -> (Type, ast::typed::ExprKind) {
        match &expr.kind {
            ast::ExprKind::Unit => (Type::Unit, ast::typed::ExprKind::Unit),
            ast::ExprKind::BoolConst(v) => (Type::Bool, ast::typed::ExprKind::BoolConst(*v)),
            ast::ExprKind::IntConst(v) => (Type::Int, ast::typed::ExprKind::IntConst(*v)),
            ast::ExprKind::StringConst(v) => {
                (Type::String, ast::typed::ExprKind::StringConst(v.clone()))
            }
            ast::ExprKind::FloatConst(v) => (Type::Float, ast::typed::ExprKind::FloatConst(*v)),
            ast::ExprKind::Sequence(exprs) => {
                let seqs: Vec<_> = exprs
                    .iter()
                    .map(|expr| self.typecheck_expr(scope.clone(), expr))
                    .map(|(typ, inner)| ast::typed::Expr { typ, inner })
                    .collect();
                let typ = seqs.last().map(|e| e.typ.clone()).unwrap_or(Type::Unit);
                (typ, ast::typed::ExprKind::Sequence(seqs))
            }
            ast::ExprKind::Assignment { lval, rval } => {
                let (lval_t, lexpr) = self.typecheck_expr(scope.clone(), lval);
                let (rval_t, rexpr) = self.typecheck_expr(scope.clone(), rval);
                if !scope.borrow().types_eq(self.tctx, &lval_t, &rval_t) {
                    panic!("Expected type {:?} but got {:?}", lval_t, rval_t)
                }
                (
                    Type::Unit,
                    ast::typed::ExprKind::Assignment {
                        lval: Box::new(ast::typed::Expr {
                            typ: lval_t,
                            inner: lexpr,
                        }),
                        rval: Box::new(ast::typed::Expr {
                            typ: rval_t,
                            inner: rexpr,
                        }),
                    },
                )
            }
            ast::ExprKind::Name(name) => {
                let typ = scope
                    .borrow()
                    .type_of(name)
                    .expect(&format!("Expected type of variable {} to exist", name));
                (typ, ast::typed::ExprKind::Name(name.clone()))
            }
            ast::ExprKind::FunctionCall {
                func,
                args,
                kwargs: _kwargs,
            } => {
                let (mut func_t, func_expr) = self.typecheck_expr(scope.clone(), func);
                if let Type::Alias(f_id) = func_t {
                    func_t = scope.borrow().resolve_alias(self.tctx, f_id);
                }
                let args_t = args
                    .iter()
                    .map(|arg| self.typecheck_expr(scope.clone(), arg))
                    .map(|(typ, inner)| ast::typed::Expr { typ, inner })
                    .collect::<Vec<_>>();
                match &func_t {
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
                        for (expected_arg, arg) in expected_args.iter().zip(&args_t) {
                            if !scope.borrow().types_eq(self.tctx, &expected_arg, &arg.typ) {
                                println!("{}", self.tctx.debug_string());
                                panic!("Expected type {:?} but got {:?}", expected_arg, arg.typ)
                            }
                        }
                        (
                            ret.as_ref().clone(),
                            ast::typed::ExprKind::FunctionCall {
                                func: Box::new(ast::typed::Expr {
                                    typ: func_t,
                                    inner: func_expr,
                                }),
                                args: args_t,
                                kwargs: HashMap::new(),
                            },
                        )
                    }
                    _ => panic!("Expected function type but got {:?}", func_t),
                }
            }
            ast::ExprKind::Access { val, field } => {
                let (val_t, val_typed) = self.typecheck_expr(scope, val);
                match &val_t {
                    Type::Struct { fields, .. } => {
                        let field_t = fields
                            .iter()
                            .find(|(name, _)| name == field)
                            .map(|(_, typ)| typ)
                            .expect("Expected field to exist");
                        (
                            field_t.clone(),
                            ast::typed::ExprKind::Access {
                                val: Box::new(ast::typed::Expr {
                                    typ: val_t,
                                    inner: val_typed,
                                }),
                                field: field.clone(),
                            },
                        )
                    }
                    _ => panic!("Expected struct type but got {:?}", val_t),
                }
            }
            ast::ExprKind::Tuple(exprs) => {
                let typed_exprs = exprs
                    .iter()
                    .map(|expr| self.typecheck_expr(scope.clone(), expr))
                    .map(|(typ, inner)| ast::typed::Expr { typ, inner })
                    .collect::<Vec<_>>();
                let types = typed_exprs
                    .iter()
                    .map(|e| e.typ.clone())
                    .collect::<Vec<_>>();
                (Type::Tuple(types), ast::typed::ExprKind::Tuple(typed_exprs))
            }
            ast::ExprKind::Lambda { .. } => todo!(),
            ast::ExprKind::TypedExpr { expr, typ } => {
                let (expr_t, exptr_body) = self.typecheck_expr(scope.clone(), expr);
                let typ_t = self.ast_type_to_type(&scope.borrow(), typ);
                if !scope.borrow().types_eq(self.tctx, &expr_t, &typ_t) {
                    panic!("Expected type {:?} but got {:?}", typ_t, expr_t)
                }
                (expr_t, exptr_body)
            }
            ast::ExprKind::StructLiteral { strct, values } => {
                let name = &strct.0[0];
                let mut strct_t = scope
                    .borrow()
                    .lookup_type(name)
                    .expect("Expected type to exist");
                if let Type::Alias(id) = strct_t {
                    strct_t = scope.borrow().resolve_alias(self.tctx, id);
                }
                let fields = match &strct_t {
                    Type::Struct { fields, .. } => fields,
                    _ => panic!("Expected struct type but got {:?}", strct_t),
                };
                let mut str_typed_fields = HashMap::new();
                for (field, field_t) in fields {
                    let expr = values.get(field).expect("Expected field to exist");
                    let (expr_t, expr_typed) = self.typecheck_expr(scope.clone(), expr);
                    if let Type::ToInfere = field_t {
                        let expr_t = expr_t.clone();
                        let inferred_type = match expr_t {
                            // TODO :
                            // Def id gives us a struct name, from which we can get the type id
                            Type::Struct { def, .. } => {
                                let type_name = match self.tctx.nodes.get(&def).unwrap() {
                                    ast::TopLevelItem::TypeDefinition(ast::TypeDefinition {
                                        name,
                                        ..
                                    }) => name,
                                    _ => panic!("Expected type definition"),
                                };
                                let type_id = self.tctx.types.get(type_name).unwrap();
                                Type::Alias(*type_id)
                            }
                            _ => expr_t,
                        };
                        self.update_struct_field_type(
                            &mut scope.borrow_mut(),
                            name,
                            field,
                            inferred_type,
                        );
                    } else if !scope.borrow().types_eq(self.tctx, &field_t, &expr_t) {
                        panic!("Expected type {:?} but got {:?}", field_t, expr_t)
                    }
                    str_typed_fields.insert(
                        field.clone(),
                        ast::typed::Expr {
                            typ: expr_t,
                            inner: expr_typed,
                        },
                    );
                }
                (
                    // lookup again as we might have updated types
                    scope
                        .borrow()
                        .lookup_type(name)
                        .expect("Expected type to exist"),
                    ast::typed::ExprKind::StructLiteral {
                        strct: name.clone(),
                        values: str_typed_fields,
                    },
                )
            }
            ast::ExprKind::While { cond, body } => {
                let (cond_t, cond_typed) = self.typecheck_expr(scope.clone(), cond);
                if cond_t != Type::Bool {
                    panic!("Expected type {:?} but got {:?}", Type::Bool, cond_t)
                }
                let (body_t, body_typed) =
                    self.typecheck_expr(Scope::empty_scope_with_parent(scope.clone()), body);
                if body_t != Type::Unit {
                    panic!("Expected type {:?} but got {:?}", Type::Unit, body_t)
                }
                (
                    Type::Unit,
                    ast::typed::ExprKind::While {
                        cond: Box::new(ast::typed::Expr {
                            typ: cond_t,
                            inner: cond_typed,
                        }),
                        body: Box::new(ast::typed::Expr {
                            typ: body_t,
                            inner: body_typed,
                        }),
                    },
                )
            }
            ast::ExprKind::Return(expr) => {
                let (expr_t, expr_typed) = self.typecheck_expr(scope.clone(), expr);
                let ret_t = self
                    .functions_ret_type
                    .clone()
                    .expect("Expected return type to be set");
                if !scope.borrow().types_eq(self.tctx, &expr_t, &ret_t) {
                    panic!("Expected type {:?} but got {:?}", ret_t, expr_t)
                }
                (
                    Type::Divergent,
                    ast::typed::ExprKind::Return(Box::new(ast::typed::Expr {
                        typ: expr_t,
                        inner: expr_typed,
                    })),
                )
            }
            ast::ExprKind::If {
                cond,
                body,
                else_body,
            } => {
                let (cond_t, cond_typed) = self.typecheck_expr(scope.clone(), cond);
                if cond_t != Type::Bool {
                    panic!("Expected type {:?} but got {:?}", Type::Bool, cond_t)
                }
                let (body_t, body_typed) =
                    self.typecheck_expr(Scope::empty_scope_with_parent(scope.clone()), body);
                let else_body_opt = else_body
                    .as_ref()
                    .map(|e| self.typecheck_expr(Scope::empty_scope_with_parent(scope.clone()), e));
                let (else_body_t, else_typed) = match else_body_opt {
                    Some((t, e)) => (t, Some(e)),
                    None => (Type::Unit, None),
                };
                if !scope.borrow().types_eq(self.tctx, &body_t, &else_body_t) {
                    panic!("Expected type {:?} but got {:?}", body_t, else_body_t)
                }
                (
                    body_t.clone(),
                    ast::typed::ExprKind::If {
                        cond: Box::new(ast::typed::Expr {
                            typ: Type::Bool,
                            inner: cond_typed,
                        }),
                        body: Box::new(ast::typed::Expr {
                            typ: body_t,
                            inner: body_typed,
                        }),
                        else_body: else_typed.map(|e| {
                            Box::new(ast::typed::Expr {
                                typ: else_body_t,
                                inner: e,
                            })
                        }),
                    },
                )
            }
            ast::ExprKind::Let { name, typ, init } => {
                let (init_t, init_typed_expr) = self.typecheck_expr(scope.clone(), init);
                let typ_t = match typ {
                    ast::Typ::ToInfere => init_t,
                    _ => {
                        let typ_t = self.ast_type_to_type(&*scope.borrow(), typ);
                        if !scope.borrow().types_eq(self.tctx, &init_t, &typ_t) {
                            panic!("Expected type {:?} but got {:?}", typ_t, init_t)
                        }
                        typ_t
                    }
                };
                self.add_variable(&mut scope.borrow_mut(), name, typ_t.clone());
                (
                    Type::Unit,
                    ast::typed::ExprKind::Let {
                        name: name.clone(),
                        typ: typ_t.clone(),
                        init: Box::new(ast::typed::Expr {
                            inner: init_typed_expr,
                            typ: typ_t,
                        }),
                    },
                )
            }
            ast::ExprKind::AnonType { .. } => panic!("There should be no anon types when typechecking"),
        }
    }

    fn ast_type_to_type<'s>(&self, scope: &Scope, typ: &ast::Typ) -> Type {
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
            ast::Typ::Function { args, ret, generics: _generics } => {
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

impl<'ast, 'parent> Visitor<'ast> for TypeChecker<'parent> {
    fn visit(&mut self, node: &'ast ast::Program) {
        let global_scope = Rc::new(RefCell::new(Scope::from_type_ctx(self.tctx)));
        for item in &node.items {
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
                let (actual_type, fn_body) = {
                    let fn_scope = Scope::empty_scope_with_parent(global_scope.clone());
                    self.functions_ret_type = Some(*ret.clone());
                    for (param, typ) in parameters.iter().zip(&args) {
                        fn_scope.borrow_mut().add_variable(&param, typ.clone());
                    }
                    self.typecheck_expr(fn_scope, body)
                };
                if !global_scope
                    .borrow()
                    .types_eq(self.tctx, &actual_type, &ret)
                {
                    panic!(
                        "Expected function {} to return type {:?} but got {:?}",
                        name, ret, actual_type
                    )
                }
                self.typed_ast
                    .items
                    .push(ast::typed::TopLevelItem::FunctionDefinition(
                        ast::typed::FunctionDefinition {
                            name: name.clone(),
                            parameters: parameters.clone(),
                            body: ast::typed::Expr {
                                typ: actual_type,
                                inner: fn_body,
                            },
                            typ: Type::Function {
                                args: args.clone(),
                                ret: ret.clone(),
                            },
                        },
                    ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast_passes::{self},
        syntax::{ast::visitor::Visitor, lexer::Lexer, parser::Parser},
        typecheck,
    };

    use super::TypeChecker;

    fn run_typechecker(source: &str) {
        let lex = Lexer::new(source);
        let mut parser = Parser::new(lex);
        let res = parser.parse().unwrap();
        let res = ast_passes::run_befor_type_context_passes(res);
        let mut tctx = typecheck::prepare_for_typechecking(&res);
        let res = ast_passes::run_before_typechecking_passes(&tctx, res);
        println!("Final ast {res:#?}");
        let mut type_check = TypeChecker::new(&mut tctx);
        type_check.visit(&res);
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
