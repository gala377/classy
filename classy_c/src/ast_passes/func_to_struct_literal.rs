use crate::ast_passes;
use crate::syntax::ast::{self, ExprKind};
use crate::typecheck::r#type::Type;
use crate::typecheck::type_context::TypCtx;

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
                    kind: ast::ExprKind::Name(name.to_owned()),
                },
                args,
                kwargs,
            );
        };
        let Some(fields) = resolve_fields(&self.tctx, &typ) else {
            println!("Function call {name} is not a struct, so no struct");
            return ast::fold::fold_function_call(
                self,
                ast::Expr {
                    id: expr_id,
                    kind: ast::ExprKind::Name(name.to_owned()),
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
                fields.iter().find(|(n, _)| &n == &name).is_some(),
                "struct {name} syntax has unknown field: {}",
                name
            );
        }
        println!("Function call is a struct {name}, so promoting");
        ast::fold::fold_struct_literal(
            self,
            ast::Path(vec![name.to_owned()]),
            body.into_iter().collect(),
        )
    }

    fn try_to_resolve_adt(
        &mut self,
        expr_id: usize,
        access_id: usize,
        name: &str,
        case: &str,
        args: Vec<ast::Expr>,
        kwargs: std::collections::HashMap<String, ast::Expr>,
    ) -> ExprKind {
        let Some(t) = self.tctx.get_type(name) else {
            return ast::fold::fold_function_call(
                self,
                ast::Expr {
                    id: expr_id,
                    kind: ast::ExprKind::Access {
                        val: Box::new(ast::Expr {
                            id: access_id,
                            kind: ast::ExprKind::Name(name.to_owned()),
                        }),
                        field: case.to_owned(),
                    },
                },
                args,
                kwargs,
            );
        };
        let Some(t) = resolve_case(&self.tctx, case, &t) else {
            return ast::fold::fold_function_call(
                self,
                ast::Expr {
                    id: expr_id,
                    kind: ast::ExprKind::Access {
                        val: Box::new(ast::Expr {
                            id: access_id,
                            kind: ast::ExprKind::Name(name.to_owned()),
                        }),
                        field: case.to_owned(),
                    },
                },
                args,
                kwargs,
            );
        };
        match t {
            Type::Struct { fields, .. } => {
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
                        fields.iter().find(|(n, _)| &n == &name).is_some(),
                        "struct {name} syntax has unknown field: {}",
                        name
                    );
                }
                ast::fold::fold_adt_struct_constructor(self, name.to_owned(), case.to_owned(), body)
            }
            Type::Tuple(field_t) => {
                assert!(kwargs.is_empty(), "tuple constructors do not that kwargs");
                assert_eq!(
                    args.len(),
                    field_t.len(),
                    "tuple constructors take exactly the number of arguments as there are fields"
                );
                ast::fold::fold_adt_tuple_constructor(self, name.to_owned(), case.to_owned(), args)
            }
            _ => ast::fold::fold_function_call(
                self,
                ast::Expr {
                    id: expr_id,
                    kind: ast::ExprKind::Access {
                        val: Box::new(ast::Expr {
                            id: access_id,
                            kind: ast::ExprKind::Name(name.to_owned()),
                        }),
                        field: case.to_owned(),
                    },
                },
                args,
                kwargs,
            ),
        }
    }
}

impl<'ctx> ast_passes::AstPass for PromoteCallToStructLiteral<'ctx> {
    fn run(&mut self, ast: ast::Program) -> ast::Program {
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
            ast::ExprKind::Name(name) => self.try_to_resolve_struct(func.id, &name, args, kwargs),
            ast::ExprKind::Access {
                val:
                    box ast::Expr {
                        kind: ast::ExprKind::Name(n),
                        id: access_id,
                    },
                field,
            } => self.try_to_resolve_adt(func.id, access_id, &n, &field, args, kwargs),
            _ => ast::fold::fold_function_call(self, func, args, kwargs),
        }
    }

    fn fold_access(&mut self, val: ast::Expr, field: String) -> ast::ExprKind {
        let ast::ExprKind::Name(name) = val.kind else {
            return ast::fold::fold_access(self, val, field);
        };

        let Some(t) = self.tctx.get_type(&name) else {
            return ast::fold::fold_access(
                self,
                ast::Expr {
                    id: val.id,
                    kind: ast::ExprKind::Name(name),
                },
                field,
            );
        };
        let Some(t) = resolve_case(&self.tctx, &field, &t) else {
            return ast::fold::fold_access(
                self,
                ast::Expr {
                    id: val.id,
                    kind: ast::ExprKind::Name(name),
                },
                field,
            );
        };
        match t {
            Type::Unit => self.fold_adt_unit_constructor(name, field),
            _ => ast::fold::fold_access(
                self,
                ast::Expr {
                    id: val.id,
                    kind: ast::ExprKind::Name(name),
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
    use crate::{
        ast_passes::{func_to_struct_literal::PromoteCallToStructLiteral, AstPass},
        syntax::{lexer::Lexer, parser::Parser},
        typecheck,
    };

    use crate::syntax::ast;

    fn run_test(input: &str, expected: ast::Builder) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let tctx = typecheck::prepare_for_typechecking(&ast);
        let mut pass = PromoteCallToStructLiteral::new(&tctx);
        let actual = pass.run(ast);
        let expected = expected.build();
        similar_asserts::assert_eq!(expected: expected, actual: actual);
    }

    #[test]
    fn does_not_replace_call_to_normal_function() {
        let input = r#"main: () -> (); main = foo();"#;
        let expected = ast::Builder::new().unit_fn("main", |body| {
            body.function_call(|f| f.name("foo"), |args| args, |kwargs| kwargs)
        });
        run_test(input, expected)
    }

    #[test]
    fn replaces_simple_struct_literal() {
        let input = r#"type Foo {bar: Int; baz: Int}; main: () -> (); main = Foo(bar=1, baz=2);"#;
        let expected = ast::Builder::new()
            .struct_def("Foo", |strct| strct.field("bar", "Int").field("baz", "Int"))
            .unit_fn("main", |body| {
                body.struct_literal("Foo", |values| {
                    values
                        .add("bar", |e| e.integer(1))
                        .add("baz", |e| e.integer(2))
                })
            });
        run_test(input, expected)
    }

    #[test]
    fn recognizes_struct_with_no_fields() {
        let input = r#"type Foo {}; main: () -> (); main = Foo();"#;
        let expected = ast::Builder::new()
            .struct_def("Foo", |strct| strct)
            .unit_fn("main", |body| body.struct_literal("Foo", |values| values));
        run_test(input, expected)
    }
}
