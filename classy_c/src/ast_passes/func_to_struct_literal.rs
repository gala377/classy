use crate::ast_passes;
use crate::syntax::ast;
use crate::typecheck::r#type::Type;
use crate::typecheck::type_context::TypCtx;

pub struct PromoteCallToStructLiteral<'ctx> {
    tctx: &'ctx TypCtx,
}

impl<'ctx> PromoteCallToStructLiteral<'ctx> {
    pub fn new(tctx: &'ctx TypCtx) -> Self {
        Self { tctx }
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
        let ast::ExprKind::Name(name) = func.kind else {
            println!("Functiona call is not a name, so no struct");
            return ast::ExprKind::FunctionCall { func: Box::new(func), args, kwargs };
        };
        let Some(mut typ) = self.tctx.get_type(&name) else {
            println!("Function call is not a known name, so no struct");
            return ast::ExprKind::FunctionCall { func: Box::new(ast::Expr {
                id: func.id,
                kind: ast::ExprKind::Name(name.clone()),
        }), args, kwargs };
        };
        if let Type::Alias(for_t) = typ {
            println!("Function call is an alias, so resolving");
            typ = self.tctx.resolve_alias(for_t);
        }
        let Type::Struct { fields, .. } = typ else {
            println!("Function call is not a struct, so no struct");
            return ast::ExprKind::FunctionCall {
                func: Box::new(ast::Expr {
                    id: func.id,
                    kind: ast::ExprKind::Name(name.clone()),
                }),
                args,
                kwargs,
            };
        };
        if fields.len() != kwargs.len() {
            panic!("Not all fields on a struct literal were filled in")
        };
        if !args.is_empty() {
            panic!("Struct literals do not take positional arguments")
        }
        for (field, _) in &fields {
            if !kwargs.contains_key(field) {
                panic!("Struct literal missing field: {}", field)
            }
        }
        ast::fold::fold_struct_literal(self, ast::Path(vec![name]), kwargs)
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
