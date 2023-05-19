use classy_c::typecheck::{self, type_context::TypCtx};
use classy_c::{
    syntax::{
        ast::Visitor,
        lexer::Lexer,
        parser::Parser,
        tokens::{Token, TokenType},
    },
    typecheck::add_types::AddTypes,
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
        type A = Int
        type B = UInt
        type D = C
        type C = B
        type E = Bool
        type G = F
        type F {
            a: C
            b: F
        }
        type H {
            a: F 
            b: G
        }
        type I = (I, G, C, A)
        type J = I
        type Z {
            a: I
            b: (Int, Int)
            c: (U1) -> Z
        }

        type U = (Int, Int)
        type U1 = A1
        type A1 = (Int, (Int, (Int, Int)))
        type B1 = (Int, (Int, Int))
        type D1 = (Int, Int)

        type A2 = (Int, (Int, (Int, Int)), ())
        type B2 = (Int, (Int, Int))
        type D2 = (Int, Int)

        type Z1 { a: A1 }
        type Z2 { a: A2 }

        type Z3 { z: Z6 }
        type Z6 = Z4
        type Z4 = (Int) -> A2
        type Z5 = (Int) -> ()

        print: (String) -> ()
        print s = ()

        type MyString = String

        get_string: () -> MyString
        get_string = "Hello world"

        get_int: () -> A
        get_int = 1

        main: () -> ()
        main { 
            let a = get_string
            let b = get_int()
            print(b)
        }
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
    let mut add_types = AddTypes::with_primitive_types(&mut tctx);
    add_types.visit(&res);
    println!("{}", tctx.debug_string());
    tctx = typecheck::resolve_type_names(tctx);
    println!("{}", tctx.debug_string());
    println!("RESOLVING ALIASES");
    typecheck::alias_resolver::AliasResolver::resolve(&mut tctx);
    println!("{}", tctx.debug_string());
    typecheck::dedup_trivially_eq_types(&mut tctx);
    println!("{}", tctx.debug_string());
    let mut type_check = typecheck::typechecker::TypeChecker::new(tctx);
    type_check.visit(&res);
}
