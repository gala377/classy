use classy_c::syntax::{
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
    
        type Option(a) {
            Ok(a)
            None
        }

        some: (a) -> b
        some x = Option.Ok x

        repeat: (int, () -> ()) -> ()
        repeat = todo()

        main: () -> ()
        main {
            x = repeat 10 i => {
                print i
            }
            Some x
        }

    "#;
    let lex = Lexer::new(source);
    let mut parser = Parser::new(lex);
    let res = parser.parse().unwrap();
    for def in res.items {
        println!("Parsed {def:#?}");
    }
    println!("Errors");
    for e in parser.errors() {
        println!("{e:#?}");
    }
}
