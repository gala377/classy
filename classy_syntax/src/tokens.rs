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
    #[token("match")]
    Match,
    #[token("methods")]
    Methods,
    #[token("const")]
    Const,
    #[token("rec")]
    Rec,
    #[token("namespace")]
    Namespace,
    #[token("for")]
    For,
    #[token("instance")]
    Instance,
    #[token("import")]
    Import,
    #[token("export")]
    Export,
    #[token("new")]
    New,

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
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("=")]
    EqualSign,
    #[token(".", priority = 3)]
    Dot,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[token("-")]
    Dash,
    #[token("/")]
    Slash,

    // Others
    #[regex(r"[ \t\f]+", logos::skip)]
    Whitespace,
    #[regex(r"(\r)?\n")]
    NewLine,
    #[regex(r"//.*", logos::skip)]
    Comment,

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
            Semicolon, Comma, Dot, EqualSign,
            // Grouping
            LBrace, LBracket, LParen,
            // Keywords
            Class, Where, End, Struct, Do, Else, New,

            Attribute("a".to_owned()),
        ];
        set
    };

    pub static ref IGNORE_NEWLINE_BEFORE: HashSet<Discriminant<TokenType>> = {
        use TokenType::*;
        let mut set = HashSet::new();
        macro_rules! add_tokens {
            [$($name:expr),*$(,)?] => {
                $(set.insert(discriminant(&$name)));*
            };
        }
        add_tokens![
            // No expression can start from these tokens so we can assume
            // they are a continuation of the expression.
            // When it comses to DASH, it can be a unary operator, but I assume in most cases it won't be
            Comma, Dot, FatArrow, Arrow, Plus, Star, EqualSign, Slash, Dash
        ];
        set
    };
}
