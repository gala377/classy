use std::ops::Range;

use thiserror::Error;

use super::ast::{self, TypedName};
use super::lexer::Lexer;
use super::tokens::{Token, TokenType};

#[derive(Error, Debug, Clone)]
#[error("Syntax error [{span:?}] {msg}")]
pub struct SyntaxError {
    pub span: Range<usize>,
    pub msg: String,
}

enum ParseErr {
    /// The rule matched but ended with an error.
    /// The error has already been reported and recovered.
    Err(SyntaxError),
    /// Rule could not match.
    WrongRule,
}

type ParseRes<T> = Result<T, ParseErr>;

trait SyntaxContext
where
    Self: Sized,
{
    fn error(self, parser: &mut Parser, beg: usize, msg: &str) -> Self {
        self.error_with_span(parser, beg..parser.curr_pos(), msg)
    }

    fn error_with_span(self, parser: &mut Parser, range: Range<usize>, msg: &str) -> Self;
}

impl<T> SyntaxContext for ParseRes<T> {
    fn error_with_span(self, parser: &mut Parser, span: Range<usize>, msg: &str) -> Self {
        self.map_err(|_| parser.error(span, msg))
    }
}

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    errors: Vec<SyntaxError>,
}

impl<'source> Parser<'source> {
    pub fn parse(&mut self) -> Result<ast::Program, Vec<SyntaxError>> {
        let mut items = Vec::new();
        loop {
            if self.eof() {
                if !self.errors.is_empty() {
                    return Err(self.errors.clone());
                }
                return Ok(ast::Program { items });
            }
            if let Ok(str_def) = self.parse_type_definition() {
                items.push(ast::TopLevelItem::TypeDefinition(str_def));
            } else if let Ok(fun_def) = self.parse_function_definition() {
                items.push(ast::TopLevelItem::FunctionDefinition(fun_def));
            } else {
                self.error(
                    self.lexer.current().span.clone(),
                    format!(
                        "Expected function or struct definition got {:?}",
                        &self.lexer.current()
                    ),
                );
                // panic on first syntax error, no recovery yet
                return Err(self.errors.clone());
            }
        }
    }

    fn parse_type_definition(&mut self) -> ParseRes<ast::TypeDefinition> {
        let beg = self.curr_pos();
        self.match_token(TokenType::Type)?;
        let name =
            self.parse_identifier()
                .error(self, beg, "Expected a name after a struct keyword")?;
        let type_variables = self.parse_optional_type_variables().error(
            self,
            beg,
            "Expected type variables list",
        )?;
        self.expect_token(TokenType::LBrace)?;
        let possibly_adt = self.in_scope(|p| p.parse_variants_definition());
        let definition = if let Ok(adt) = possibly_adt {
            adt
        } else {
            let fields = self.parse_delimited(Self::parse_typed_identifier, TokenType::Semicolon);
            ast::DefinedType::Record(ast::Record { fields })
        };
        self.expect_token(TokenType::RBrace)?;
        let _ = self.expect_token(TokenType::Semicolon);
        Ok(ast::TypeDefinition {
            name,
            type_variables,
            definition,
            span: beg..self.curr_pos(),
        })
    }

    fn parse_variants_definition(&mut self) -> ParseRes<ast::DefinedType> {
        fn parse_discriminant(parser: &mut Parser) -> ParseRes<ast::Discriminant> {
            let constructor = parser.parse_identifier()?;
            let mut arguments = Vec::new();
            // possibly a record definition
            if parser.match_token(TokenType::Colon).is_ok() {
                return wrong_rule();
            }
            if parser.match_token(TokenType::LParen).is_ok() {
                arguments = parser.parse_delimited(Parser::parse_type, TokenType::Comma);
                let _ = parser.expect_token(TokenType::RParen)?;
            }
            Ok(ast::Discriminant {
                constructor,
                arguments,
            })
        }
        let discriminants = self.parse_delimited(parse_discriminant, TokenType::Semicolon);
        if discriminants.is_empty() {
            wrong_rule()
        } else {
            Ok(ast::DefinedType::ADT(ast::ADT { discriminants }))
        }
    }
    fn parse_optional_type_variables(&mut self) -> ParseRes<Vec<ast::TypeVariable>> {
        if self.match_token(TokenType::LParen).is_err() {
            return Ok(Vec::new());
        }
        let type_vars = self
            .parse_delimited(Self::parse_identifier, TokenType::Comma)
            .into_iter()
            .map(|name| ast::TypeVariable { name })
            .collect();
        let _ = self.expect_token(TokenType::RParen);
        Ok(type_vars)
    }

    fn parse_function_definition(&mut self) -> ParseRes<ast::FunctionDefinition> {
        let beg = self.curr_pos();
        self.match_token(TokenType::Function)?;
        let name = self
            .parse_identifier()
            .error(self, beg, "Expected a function name")?;
        let parameters = self.parse_argument_list()?;
        let body = self.parse_expr()?;
        let _ = self.expect_token(TokenType::Semicolon);
        Ok(ast::FunctionDefinition {
            name,
            parameters,
            body,
        })
    }

    fn parse_argument_list(&mut self) -> ParseRes<Vec<ast::TypedName>> {
        if self.match_token(TokenType::LParen).is_err() {
            return Ok(Vec::new());
        }
        let args = self.parse_delimited(Self::parse_typed_identifier, TokenType::Comma);
        let _ = self.expect_token(TokenType::RParen);
        Ok(args)
    }

    fn parse_typed_identifier(&mut self) -> ParseRes<TypedName> {
        let beg = self.curr_pos();
        let name = self.parse_identifier()?;
        let _colon =
            self.match_token(TokenType::Colon)
                .error(self, beg, &self.expected_err_msg("a colon"));
        let typ = self
            .parse_identifier()
            .error(self, beg, &self.expected_err_msg("a type"))?;
        Ok(ast::TypedName {
            name,
            typ: ast::Typ::Name(typ),
        })
    }

    fn parse_identifier(&mut self) -> ParseRes<String> {
        if let Token {
            typ: TokenType::Identifier(_),
            ..
        } = self.lexer.current()
        {
            let id = self.lexer.advance();
            match id.typ {
                TokenType::Identifier(id) => Ok(id),
                _ => unreachable!("we already checked that this token is an identifier"),
            }
        } else {
            wrong_rule()
        }
    }

    fn parse_type(&mut self) -> ParseRes<ast::Typ> {
        self.parse_identifier().map(|v| ast::Typ::Name(v))
    }

    fn parse_delimited<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseRes<T>,
        delimiter: TokenType,
    ) -> Vec<T> {
        let mut items = Vec::new();
        while let Ok(item) = parser(self) {
            items.push(item);
            if self.lexer.current().typ != delimiter {
                break;
            }
            self.lexer.advance();
        }
        items
    }

    fn parse_expr(&mut self) -> ParseRes<ast::Expr> {
        match self.lexer.current().clone() {
            Token {
                typ: TokenType::Integer(val),
                ..
            } => {
                self.lexer.advance();
                Ok(ast::Expr::IntConst(val))
            }
            _ => wrong_rule(),
        }
    }
}

fn wrong_rule<T>() -> ParseRes<T> {
    Err(ParseErr::WrongRule)
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>) -> Self {
        Self {
            lexer,
            errors: Vec::new(),
        }
    }

    fn in_scope<T>(&mut self, f: impl FnOnce(&mut Parser) -> ParseRes<T>) -> ParseRes<T> {
        let mut new = Parser {
            lexer: self.lexer.clone(),
            errors: Vec::new(),
        };
        let res = f(&mut new)?;
        self.errors.extend(new.errors);
        self.lexer = new.lexer;
        Ok(res)
    }

    fn eof(&self) -> bool {
        self.lexer.current().typ == TokenType::Eof
    }

    #[inline(always)]
    fn curr_pos(&self) -> usize {
        self.lexer.current().span.start
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors[..]
    }

    fn error(&mut self, span: Range<usize>, msg: impl Into<String>) -> ParseErr {
        let err = SyntaxError {
            span,
            msg: msg.into(),
        };
        self.errors.push(err.clone());
        ParseErr::Err(err)
    }

    fn expected_err_msg(&self, item: &str) -> String {
        format!(
            "Expected {} got {:?}",
            item,
            std::mem::discriminant(&self.lexer.current().typ)
        )
    }

    fn expect_token(&mut self, typ: TokenType) -> ParseRes<Token> {
        if self.lexer.current().typ == typ {
            Ok(self.lexer.advance())
        } else {
            let err = SyntaxError {
                span: self.lexer.current().span.clone(),
                msg: format!("Expected token {:?} got {:?}", typ, self.lexer.current()),
            };
            self.errors.push(err.clone());
            Err(ParseErr::Err(err))
        }
    }

    fn match_token(&mut self, typ: TokenType) -> ParseRes<Token> {
        if self.lexer.current().typ == typ {
            Ok(self.lexer.advance())
        } else {
            wrong_rule()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::ast;

    type TestRes = Result<(), Vec<SyntaxError>>;

    macro_rules! ptest {
        ($name:ident, $source:literal, $expected:expr) => {
            #[test]
            fn $name() -> TestRes {
                let source = $source;
                let lex = Lexer::new(source);
                let mut parser = Parser::new(lex);
                let actual = parser.parse()?;
                let expected = $expected.build();
                assert_eq!(expected, actual);
                Ok(())
            }
        };
    }

    ptest! {
        test_empty_struct_definition,
        r#"
            type  Foo {}
            type  Bar {
            }
        "#,
        ast::Builder::new()
            .empty_struct("Foo")
            .empty_struct("Bar")
    }

    ptest! {
        test_struct_definition_with_simple_fields,
        r#"
            type Foo { x: typ1; y: typ2 }
            type Bar {
                foo: typ_1
            }
        "#,
        ast::Builder::new()
            .struct_def("Foo", |s| s.field("x", "typ1").field("y", "typ2"))
            .struct_def("Bar", |s| s.field("foo", "typ_1"))
    }

    ptest! {
        test_struct_definition_with_explicit_not_type_vars,
        "type Foo () { x: a };",
        ast::Builder::new()
            .struct_def("Foo", |s| s.field("x", "a"))
    }

    ptest! {
        test_struct_definition_with_one_type_var,
        "type Foo (a) { x: a };",
        ast::Builder::new()
            .struct_def("Foo", |s| s.type_var("a")
                                    .field("x", "a"))
    }

    ptest! {
        test_struct_definition_with_multiple_type_vars,
        "type Foo (a, b, c) { x: a };",
        ast::Builder::new()
            .struct_def(
                "Foo", |s| s.type_var("a")
                            .type_var("b")
                            .type_var("c")
                            .field("x", "a"))
    }

    ptest! {
        test_simple_adt_definition_with_single_discriminant,
        "type A { B };",
        ast::Builder::new()
            .adt_def("A", |adt| adt.empty_discriminant("B"))
    }

    ptest! {
        test_simple_adt_with_multiple_no_argument_discriminants,
        "type A { A; B; C };",
        ast::Builder::new()
            .adt_def("A", |adt| adt.empty_discriminant("A")
                                   .empty_discriminant("B")
                                   .empty_discriminant("C"))
    }

    ptest! {
        test_adt_with_mixed_discriminants_with_arguments_and_no_arguments,
        "type A { A(T1, T2); B; C(T3); D(T4, T5, T6); E };",
        ast::Builder::new()
            .adt_def("A", |adt| adt.discriminant("A", &["T1", "T2"])
                                   .empty_discriminant("B")
                                   .discriminant("C", &["T3"])
                                   .discriminant("D", &["T4", "T5", "T6"])
                                   .empty_discriminant("E"))
    }

    ptest! {
        test_adt_with_type_vars,
        "type Res(a, b) { Ok(a); Err(b) };",
        ast::Builder::new()
            .adt_def(
                "Res",
                |adt| adt.type_var("a")
                         .type_var("b")
                         .discriminant("Ok", &["a"])
                         .discriminant("Err", &["b"]))
    }

    ptest! {
        test_function_definition_with_no_argumentsr,
        r#"func foo 10;"#,
        ast::Builder::new()
            .func_def("foo", |args| args, |body| body.integer(10))
    }

    ptest! {
        test_function_definition_with_arguments,

        r#"func foo(a:b, c:d) 10;"#,
        ast::Builder::new()
            .func_def(
                "foo",
                |a| a.name("a", "b").name("c", "d"),
                |b| b.integer(10),
            )
    }
}
