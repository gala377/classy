use std::ops::Range;

use classy_c::syntax::{
    ast,
    lexer::Lexer,
    parser::Parser,
    tokens::{Token, TokenType},
};
use logos::Logos;

fn main() {
    let lex = TokenType::lexer("class \n Hello \n\n {   \n\n } 1 1. 0 0. -0.21 -2123 .1").spanned();
    let lex = lex.map(|(typ, span)| Token { typ, span });
    for t in lex {
        let token = t.typ;
        let span = t.span;
        println!("{token:?} span: {span:?}")
    }
    println!("Hello world!");

    let source = r#"
        type Foo { x: Int; y: Float }
        type Bar {
            x : String
            y : String2
        }
        type Baz {     x : String
                         y : Int }

        func FooBar 10
        func FooBar2() 20
        func FooBar3(a: b) 20
        func FooBar3(
            a: b,
            b: c,
        ) 20
        "#;
    let lex = Lexer::new(source);
    let mut parser = Parser::new(lex);
    let res = parser.parse().unwrap();
    for def in res.items {
        println!("Parsed {def:#?}");
        if let ast::TopLevelItem::TypeDefinition(ast::TypeDefinition { span, .. }) = def {
            print_source(source, span);
        }
    }
    println!("Errors");
    for e in parser.errors() {
        println!("{e:#?}");
    }
}

fn print_source(source: &str, span: Range<usize>) {
    print!("\n{}\n\n", &source[span]);
}
