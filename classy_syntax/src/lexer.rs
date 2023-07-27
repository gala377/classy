use logos::Logos;

use crate::tokens::{Token, TokenType, IGNORE_NEWLINE};

#[derive(Clone)]
pub struct Lexer<'a> {
    tokens: logos::Lexer<'a, TokenType>,
    curr_token: Token,
    peek_token: Token,
    inserted_semicolon_on_eof: bool,
}

impl<'source> Lexer<'source> {
    const DUMMY: Token = Token {
        typ: TokenType::Eof,
        span: 0..0,
    };

    pub fn new(source: &'source str) -> Self {
        let mut lex = Self {
            tokens: TokenType::lexer(source),
            curr_token: Self::DUMMY,
            peek_token: Self::DUMMY,
            inserted_semicolon_on_eof: false,
        };
        lex.advance();
        lex.advance();
        lex
    }

    pub fn advance(&mut self) -> Token {
        let insert_semicolon =
            !IGNORE_NEWLINE.contains(&std::mem::discriminant(&self.peek_token.typ));
        let new_peek = self.bump(insert_semicolon);
        let peek = std::mem::replace(&mut self.peek_token, new_peek);
        std::mem::replace(&mut self.curr_token, peek)
    }

    pub fn current(&self) -> &Token {
        &self.curr_token
    }

    pub fn peek(&self) -> &Token {
        &self.peek_token
    }

    fn bump(&mut self, insert_semicolon: bool) -> Token {
        use TokenType::*;
        let tok = self.tokens.next();
        match tok {
            Some(NewLine) if insert_semicolon => Token {
                typ: Semicolon,
                span: self.tokens.span(),
            },
            Some(NewLine) => skip_new_lines(&mut self.tokens),
            Some(typ) => Token {
                typ,
                span: self.tokens.span(),
            },
            None if self.inserted_semicolon_on_eof => Token {
                typ: Eof,
                span: self.tokens.span(),
            },
            None => {
                self.inserted_semicolon_on_eof = true;
                Token {
                    typ: Semicolon,
                    span: self.tokens.span(),
                }
            }
        }
    }
}

fn skip_new_lines(tokens: &mut logos::Lexer<TokenType>) -> Token {
    use TokenType::*;
    while let Some(typ) = tokens.next() {
        match typ {
            NewLine => (),
            _ => {
                return Token {
                    typ,
                    span: tokens.span(),
                }
            }
        }
    }
    Token {
        typ: Eof,
        span: tokens.span(),
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use super::Token;
    use super::TokenType::{self, *};

    macro_rules! assert_token {
        ($p:pat, $e:expr) => {
            let t = $e;
            if let Token { typ: $p, .. } = t {
            } else {
                let repr = stringify!($p);
                panic!("Unexpected token {t:?}. Expected {repr}");
            }
        };
    }
    macro_rules! assert_lex {
        ([$($p:pat),*$(,)?], $l:expr) => {
            let mut lex = $l;
            $(assert_token!($p, lex.advance()));*;
            assert_token!(Eof, lex.advance());
        };
    }

    #[test]
    fn empty_input_gives_one_semicolon() {
        let l = Lexer::new("");
        assert_token!(Semicolon, l.current());
        assert_token!(Eof, l.peek());
    }

    #[test]
    fn advancing_tokens_past_eof_yields_oef() {
        assert_lex!([Semicolon, Eof, Eof, Eof, Eof, Eof], Lexer::new(""));
    }

    #[test]
    #[allow(illegal_floating_point_literal_pattern)]
    fn lexing_simple_floats() {
        assert_lex!(
            [
                Integer(2),
                Integer(-2),
                Float(1.0),
                Float(1.0),
                Float(0.1),
                Float(-1.1),
                Semicolon,
            ],
            Lexer::new("2 -2 1.0 1. .1 -1.1 ")
        );
    }

    #[test]
    fn lexing_strings() {
        let l = Lexer::new(r#""Hello world""#);
        match l.current() {
            Token {
                typ: TokenType::String(s),
                ..
            } if s == r#"Hello world"# => {}
            Token {
                typ: TokenType::String(s),
                ..
            } => {
                panic!("Value of s is {s}")
            }
            t => panic!("value of s is wrong {t:#?}"),
        }
    }
}
