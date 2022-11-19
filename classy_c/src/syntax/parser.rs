use std::ops::Range;

use thiserror::Error;

use crate::syntax::ast::StructDefinition;

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

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    errors: Vec<SyntaxError>,
}

impl<'source> Parser<'source> {
    pub fn parse(&mut self) -> ast::Program {
        let mut items = Vec::new();
        loop {
            if let Token {
                typ: TokenType::Eof,
                ..
            } = self.lexer.current()
            {
                return ast::Program { items };
            }
            if let Ok(str_def) = self.parse_struct_definition() {
                items.push(ast::TopLevelItem::StructDefinition(str_def));
            } else if let Ok(fun_def) = self.parse_function_definition() {
                items.push(ast::TopLevelItem::FunctionDefinition(fun_def));
            } else {
                self.error(
                    self.lexer.current().span.clone(),
                    "Expected function or struct definition",
                );
                // panic on first syntax error, no recovery yet
                return ast::Program { items };
            }
        }
    }

    fn parse_struct_definition(&mut self) -> ParseRes<ast::StructDefinition> {
        let beg = self.curr_pos();
        self.match_token(TokenType::Struct)?;
        let name = self.parse_identifier().map_err(|_| {
            self.error(
                beg..self.curr_pos(),
                "Expected a name after the struct keyword",
            )
        })?;
        self.expect_token(TokenType::Where)?;
        let fields = self.parse_delimited(Self::parse_typed_identifier, TokenType::Semicolon);
        self.expect_token(TokenType::End)?;
        Ok(StructDefinition {
            name,
            fields,
            span: beg..self.curr_pos(),
        })
    }

    fn parse_function_definition(&mut self) -> ParseRes<ast::FunctionDefinition> {
        self.match_token(TokenType::Function)?;
        Err(ParseErr::WrongRule)
    }

    fn parse_typed_identifier(&mut self) -> ParseRes<TypedName> {
        let beg = self.curr_pos();
        let name = self.parse_identifier()?;
        let _colon = self
            .match_token(TokenType::Colon)
            .map_err(|_| self.error(beg..self.curr_pos(), self.expected_err_msg("a colon")));
        let typ = self
            .parse_identifier()
            .map_err(|_| self.error(beg..self.curr_pos(), self.expected_err_msg("a type")))?;
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
            Err(ParseErr::WrongRule)
        }
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
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>) -> Self {
        Self {
            lexer,
            errors: Vec::new(),
        }
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
            Err(ParseErr::WrongRule)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::ast;

    #[test]
    fn test_empty_struct_definition() {
        let source = r#"
            struct Foo where end
            struct Bar where
            end
        "#;
        let lex = Lexer::new(source);
        let mut parser = Parser::new(lex);
        let actual = parser.parse();
        let expected = ast::Builder::new()
            .empty_struct("Foo")
            .empty_struct("Bar")
            .build();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_struct_definition_with_simple_fields() {
        let source = r#"
            struct Foo where x: typ1; y: typ2 end
            struct Bar where
                foo: typ_1
            end
        "#;
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        let actual = parser.parse();
        let expected = ast::Builder::new()
            .struct_def("Foo", |s| s.field("x", "typ1").field("y", "typ2"))
            .struct_def("Bar", |s| s.field("foo", "typ_1"))
            .build();
        assert_eq!(expected, actual);
    }
}
