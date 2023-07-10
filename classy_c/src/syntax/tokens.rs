use std::{
    collections::HashSet,
    mem::{discriminant, Discriminant},
    ops::Range,
};

use lazy_static::lazy_static;
use logos::Logos;

#[derive(Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub span: Range<usize>,
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum TokenType {
    // Keywords
    #[token("class")]
    Class,
    #[token("struct")]
    Struct,
    #[token("where")]
    Where,
    #[token("end")]
    End,
    #[token("do")]
    Do,
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("func")]
    Function,
    #[token("type")]
    Type,
    #[token("return")]
    Return,
    #[token("let")]
    Let,
    #[token("forall")]
    Forall,
    #[token("array")]
    Array,

    // Literals
    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.slice().to_owned())]
    Identifier(String),
    #[regex(r"(-)?(0|[1-9][0-9]*)", |lex| lex.slice().parse())]
    Integer(isize),
    #[regex(r"(-)?(0|[1-9][0-9]*)?\.[0-9]*", |lex| lex.slice().parse())]
    Float(f64),
    #[regex(r#""(?:[^"]|\\")*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_owned())]
    String(String),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"@[a-zA-Z]+", |lex| lex.slice()[1..lex.slice().len()].to_owned())]
    Attribute(String),

    // Grouping
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    // Operators
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("=")]
    Assignment,
    #[token(".", priority = 3)]
    Dot,

    // Others
    #[regex(r"[ \t\f]+", logos::skip)]
    Whitespace,
    #[regex(r"(\r)?\n")]
    NewLine,

    #[error]
    Error,

    Eof,
}

lazy_static! {
    /// List of tokens after which the lexer should not insert a semicolon
    /// in case there is a newline following them;
    pub static ref IGNORE_NEWLINE: HashSet<Discriminant<TokenType>> = {
        use TokenType::*;
        let mut set = HashSet::new();
        macro_rules! add_tokens {
            [$($name:expr),*$(,)?] => {
                $(set.insert(discriminant(&$name)));*
            };
        }
        add_tokens![
            // Special
            NewLine, Eof,
            // Operators
            Semicolon, Comma, Dot,
            // Grouping
            LBrace, LBracket, LParen,
            // Keywords
            Class, Where, End, Struct, Do, Else,

            Attribute("a".to_owned()),
        ];
        set
    };
}
