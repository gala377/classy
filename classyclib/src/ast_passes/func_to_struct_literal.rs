use classy_syntax::ast::{self, ExprKind};

use crate::ast_passes;
use crate::session::Session;
use crate::typecheck::type_context::TypCtx;
use crate::typecheck::types::Type;

/// TODO: We should have a scope there
/// in case someone uses variables with the same name as the types
pub struct PromoteCallToStructLiteral<'ctx> {
    tctx: &'ctx TypCtx,
}

impl<'ctx> PromoteCallToStructLiteral<'ctx> {
    pub fn new(tctx: &'ctx TypCtx) -> Self {
        Self { tctx }
    }

    pub fn try_to_resolve_struct(
        &mut self,
        expr_id: usize,
        name: &str,
        args: Vec<ast::Expr>,
        kwargs: std::collections::HashMap<String, ast::Expr>,
    ) -> ast::ExprKind {
        let Some(typ) = self.tctx.get_type(name) else {
            println!("Function call {name} is not a known name, so no struct");
            return ast::fold::fold_function_call(
                self,
                ast::Expr {
                    id: expr_id,
                    kind: ast::ExprKind::Name(ast::Name::Unresolved {
                        path: vec![],
                        identifier: name.to_owned(),
                    }),
                },
                args,
                kwargs,
            );
        };
        let Some(fields) = resolve_fields(self.tctx, &typ) else {
            println!("Function call {name} is not a struct, so no struct");
            return ast::fold::fold_function_call(
                self,
                ast::Expr {
                    id: expr_id,
                    kind: ast::ExprKind::Name(ast::Name::Unresolved {
                        path: vec![],
                        identifier: name.into(),
                    }),
                },
                args,
                kwargs,
            );
        };

        assert!(
            kwargs.is_empty(),
            "use struct {name} creation syntax, kwargs are deprecated"
        );
        let [ast::Expr {
            kind:
                ast::ExprKind::Lambda {
                    parameters,
                    body:
                        box ast::Expr {
                            kind: ast::ExprKind::Sequence(body),
                            ..
                        },
                },
            ..
        }] = &args[..]
        else {
            panic!("struct {name} creation syntax expected a record literal")
        };
        assert!(
            parameters.is_empty(),
            "struct {name} creation syntax expected a record literal"
        );
        let body = body
            .iter()
            .map(|e| match e {
                ast::Expr {
                    kind:
                        ast::ExprKind::Assignment {
                            lval:
                                box ast::Expr {
                                    kind: ast::ExprKind::Name(name),
                                    ..
                                },
                            rval: value,
                        },
                    ..
                } => (name.to_owned(), *value.clone()),
                _ => panic!("struct {name} creation syntax expected a record literal"),
            })
            .collect::<Vec<_>>();
        assert_eq!(
            body.len(),
            fields.len(),
            "struct {name} syntax missing fields"
        );
        for (name, _) in &body {
            assert!(
                fields.iter().any(|(n, _)| n
                    == match name {
                        ast::Name::Unresolved { identifier, .. } => identifier,
                        _ => panic!(),
                    }),
                "struct {name:?} syntax has unknown field"
            );
        }
        println!("Function call is a struct {name}, so promoting");
        ast::fold::fold_struct_literal(
            self,
            ast::Name::Unresolved {
                path: vec![],
                identifier: name.to_owned(),
            },
            body.into_iter()
                .map(|(n, v)| {
                    (
                        match n {
                            ast::Name::Unresolved { identifier, .. } => identifier,
                            _ => panic!(),
                        },
                        v,
                    )
                })
                .collect(),
        )
    }

    fn try_to_resolve_adt(
        &mut self,
        receiver: ast::Expr,
        field: String,
        args: Vec<ast::Expr>,
        kwargs: std::collections::HashMap<String, ast::Expr>,
    ) -> ExprKind {
        let ast::ExprKind::Name(ast::Name::Unresolved { path, identifier }) = receiver.kind.clone()
        else {
            return ast::fold::fold_method_call(self, receiver, field, args, kwargs);
        };
        let Some(t) = self.tctx.get_type(&identifier) else {
            return ast::fold::fold_method_call(self, receiver, field, args, kwargs);
        };
        let Some(t) = resolve_case(self.tctx, &field, &t) else {
            return ast::fold::fold_method_call(self, receiver, field, args, kwargs);
        };
        match t {
            Type::Struct { fields, .. } => {
                assert!(
                    kwargs.is_empty(),
                    "use struct {identifier:?} creation syntax, kwargs are deprecated",
                );
                let [ast::Expr {
                    kind:
                        ast::ExprKind::Lambda {
                            parameters,
                            body:
                                box ast::Expr {
                                    kind: ast::ExprKind::Sequence(body),
                                    ..
                                },
                        },
                    ..
                }] = &args[..]
                else {
                    panic!("struct {identifier:?} creation syntax expected a record literal")
                };
                assert!(
                    parameters.is_empty(),
                    "struct {identifier:?} creation syntax expected a record literal"
                );
                let body = body
                    .iter()
                    .map(|e| match e {
                        ast::Expr {
                            kind:
                                ast::ExprKind::Assignment {
                                    lval:
                                        box ast::Expr {
                                            kind: ast::ExprKind::Name(name),
                                            ..
                                        },
                                    rval: value,
                                },
                            ..
                        } => (name.to_owned(), *value.clone()),
                        _ => panic!(
                            "struct {identifier:?} creation syntax expected a record literal"
                        ),
                    })
                    .collect::<Vec<_>>();
                assert_eq!(
                    body.len(),
                    fields.len(),
                    "struct {identifier:?} syntax missing fields"
                );
                for (name, _) in &body {
                    let ast::Name::Unresolved { identifier, .. } = name else {
                        panic!();
                    };
                    assert!(
                        fields.iter().any(|(n, _)| n == identifier),
                        "struct {name:?} syntax has unknown field"
                    );
                }
                ast::fold::fold_adt_struct_constructor(
                    self,
                    ast::Name::Unresolved { path, identifier },
                    field,
                    body.into_iter()
                        .map(|(n, v)| {
                            (
                                match n {
                                    ast::Name::Unresolved { path, identifier }
                                        if path.is_empty() =>
                                    {
                                        identifier
                                    }
                                    _ => panic!(),
                                },
                                v,
                            )
                        })
                        .collect(),
                )
            }
            Type::Tuple(field_t) => {
                assert!(kwargs.is_empty(), "tuple constructors do not that kwargs");
                assert_eq!(
                    args.len(),
                    field_t.len(),
                    "tuple constructors take exactly the number of arguments as there are fields"
                );
                ast::fold::fold_adt_tuple_constructor(
                    self,
                    ast::Name::Unresolved { path, identifier },
                    field,
                    args,
                )
            }
            _ => ast::fold::fold_method_call(self, receiver, field, args, kwargs),
        }
    }
}

impl<'ctx> ast_passes::AstPass for PromoteCallToStructLiteral<'ctx> {
    fn run(&mut self, ast: ast::SourceFile, _: &Session) -> ast::SourceFile {
        ast::fold::fold_program(self, ast)
    }
}

impl<'ctx> ast::fold::Folder for PromoteCallToStructLiteral<'ctx> {
    fn fold_function_call(
        &mut self,
        func: ast::Expr,
        args: Vec<ast::Expr>,
        kwargs: std::collections::HashMap<String, ast::Expr>,
    ) -> ast::ExprKind {
        match func.kind {
            ast::ExprKind::Name(ast::Name::Unresolved {
                path,
                identifier: name,
            }) if path.is_empty() => self.try_to_resolve_struct(func.id, &name, args, kwargs),
            _ => ast::fold::fold_function_call(self, func, args, kwargs),
        }
    }

    fn fold_methods_call(
        &mut self,
        receiver: ast::Expr,
        method: String,
        args: Vec<ast::Expr>,
        kwargs: std::collections::HashMap<String, ast::Expr>,
    ) -> ExprKind {
        self.try_to_resolve_adt(receiver, method, args, kwargs)
    }

    fn fold_access(&mut self, val: ast::Expr, field: String) -> ast::ExprKind {
        let ast::ExprKind::Name(name) = val.kind else {
            return ast::fold::fold_access(self, val, field);
        };
        let ast::Name::Unresolved { identifier, .. } = name else {
            panic!();
        };
        let Some(t) = self.tctx.get_type(&identifier) else {
            return ast::fold::fold_access(
                self,
                ast::Expr {
                    id: val.id,
                    kind: ast::ExprKind::Name(ast::Name::Unresolved {
                        path: vec![],
                        identifier,
                    }),
                },
                field,
            );
        };
        let Some(t) = resolve_case(self.tctx, &field, &t) else {
            return ast::fold::fold_access(
                self,
                ast::Expr {
                    id: val.id,
                    kind: ast::ExprKind::Name(ast::Name::Unresolved {
                        path: vec![],
                        identifier,
                    }),
                },
                field,
            );
        };
        match t {
            Type::Unit => self.fold_adt_unit_constructor(
                ast::Name::Unresolved {
                    path: vec![],
                    identifier,
                },
                field,
            ),
            _ => ast::fold::fold_access(
                self,
                ast::Expr {
                    id: val.id,
                    kind: ast::ExprKind::Name(ast::Name::Unresolved {
                        path: vec![],
                        identifier,
                    }),
                },
                field,
            ),
        }
    }
}

fn resolve_case(tctx: &TypCtx, case: &str, typ: &Type) -> Option<Type> {
    match typ {
        Type::ADT { constructors, .. } => constructors
            .iter()
            .find(|(n, _)| n == case)
            .map(|(_, t)| t.clone()),
        Type::Scheme { typ, .. } => resolve_case(tctx, case, typ),
        Type::Alias(for_t) => {
            let typ = tctx.resolve_alias(*for_t);
            resolve_case(tctx, case, &typ)
        }
        Type::App { typ, .. } => resolve_case(tctx, case, typ),
        _ => None,
    }
}

fn resolve_fields(tctx: &TypCtx, t: &Type) -> Option<Vec<(String, Type)>> {
    match t {
        Type::Struct { fields, .. } => Some(fields.clone()),
        Type::Scheme { typ, .. } => resolve_fields(tctx, typ),
        Type::App { typ, .. } => resolve_fields(tctx, typ),
        Type::Alias(for_t) => {
            let typ = tctx.resolve_alias(*for_t);
            resolve_fields(tctx, &typ)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {

    use classy_sexpr::ToSExpr;
    use classy_sexpr_proc_macro::sexpr;
    use classy_syntax::{lexer::Lexer, parser::Parser};

    use crate::{
        ast_passes::{func_to_struct_literal::PromoteCallToStructLiteral, AstPass},
        typecheck,
    };

    fn run_test(input: &str, expected: classy_sexpr::SExpr) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let tctx = typecheck::prepare_for_typechecking(&ast);
        let mut pass = PromoteCallToStructLiteral::new(&tctx);
        let sess = crate::session::Session::new("test");
        let actual = pass.run(ast, &sess);
        similar_asserts::assert_eq!(expected: expected, actual: actual.to_sexpr());
    }

    #[test]
    fn does_not_replace_call_to_normal_function() {
        run_test(
            r#"main: () -> (); main = foo();"#,
            sexpr!((
                (fn {}
                    (type (poly [] (fn () (poly [] unit))))
                    main () {
                        call foo () {}
                    }
                )
            )),
        )
    }

    #[test]
    fn replaces_simple_struct_literal() {
        let input = r#"
            type Foo {bar: Int; baz: Int}
            main: () -> (); main = Foo { bar=1; baz=2 } 
        "#;
        run_test(
            input,
            sexpr!((
            (type Foo [] {
                record
                    [bar (poly [] Int)]
                    [baz (poly [] Int)]
            })

            (fn {}
                (type (poly [] (fn () (poly [] unit))))
                main () {
                    struct Foo {
                        ["bar" 1] 
                        ["baz" 2]
                    }
                }
            )
        )),
        )
    }

    #[test]
    fn recognizes_struct_with_no_fields() {
        let input = r#"
         type Foo {}

         main = Foo {}
        "#;
        run_test(
            input,
            sexpr!((
            (type Foo [] {
                record
            })

            (fn {}
                (type (fn () infere))
                main () {
                    struct Foo {}
                }
            )
        )),
        )
    }
}
