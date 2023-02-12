use classy_c::syntax::{
    ast::Visitor,
    lexer::Lexer,
    parser::Parser,
    tokens::{Token, TokenType},
};
use classy_c::typecheck::{self, TypCtx, Type};
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
    
        type A = Int
        type B = UInt
        type D = C
        type C = B
        type E = Bool

    "#;
    let lex = Lexer::new(source);
    let mut parser = Parser::new(lex);
    let res = parser.parse().unwrap();
    for def in &res.items {
        println!("Parsed {def:#?}");
    }
    println!("Errors");
    for e in parser.errors() {
        println!("{e:#?}");
    }
    let mut tctx = TypCtx::new();
    typecheck::insert_primitive_types(&mut tctx);
    println!("{}", tctx.debug_string());
    let mut add_types = typecheck::AddTypes::new(&mut tctx);
    add_types.visit(&res);
    println!("{}", tctx.debug_string());
    typecheck::resolve_type_names(&mut tctx);
    println!("{}", tctx.debug_string());
}
