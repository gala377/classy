use classy_c::{
    ast_passes::{run_befor_type_context_passes, run_before_typechecking_passes},
    syntax::{ast, ast::Visitor, lexer::Lexer, parser::Parser},
    typecheck::{self, add_types::AddTypes, type_context::TypCtx},
};

const SOURCE: &'static str = r#"
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

    get_string: () -> MyString
    get_string = "Hello world"

    get_int: () -> A
    get_int = 1

    call: (() -> Int) -> Int
    call f = f()

    type MyFoo {
        a: MyString
    }

    main: () -> ()
    main { 
        let a = MyFoo(a = "Hello")
        print a.a
    }
"#;

fn main() {
    let package = make_package();
    let tctx = compile(SOURCE, &[package]);
    println!("{}", tctx.debug_string());
    // let package = classy_c::package::Package::new("main", &tctx);
    // let out = std::fs::File::create("out.json").unwrap();
    // serde_json::to_writer_pretty(out, &package).unwrap();
}

fn compile(source: &str, packages: &[classy_c::package::Package]) -> TypCtx {
    let res = parse_source(source);
    let res = run_befor_type_context_passes(res);
    let mut tctx = TypCtx::new();
    for package in packages {
        package.read_headers(&mut tctx);
    }
    let mut tctx = prepare_type_ctx(tctx, &res);
    println!("{}", tctx.debug_string());
    let res = run_before_typechecking_passes(&tctx, res);
    let mut type_check = typecheck::typechecker::TypeChecker::new(&mut tctx);
    type_check.visit(&res);
    tctx
}

pub fn make_package() -> classy_c::package::Package {
    let source = r#"
        type MyString = String

        print: (MyString) -> ()
        print s = ()
    "#;
    let tctx = compile(source, &[]);
    let pckg = classy_c::package::Package::new("test", &tctx);
    println!("Compiler package {:?}", pckg);
    pckg
}

pub fn parse_source(source: &str) -> ast::Program {
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
    res
}

pub fn prepare_type_ctx(mut tctx: TypCtx, ast: &ast::Program) -> TypCtx {
    let mut add_types = AddTypes::with_primitive_types(&mut tctx);
    add_types.visit(&ast);
    println!("{}", tctx.debug_string());
    tctx = typecheck::resolve_type_names(tctx);
    println!("{}", tctx.debug_string());
    println!("RESOLVING ALIASES");
    typecheck::alias_resolver::AliasResolver::resolve(&mut tctx);
    println!("{}", tctx.debug_string());
    typecheck::dedup_trivially_eq_types(&mut tctx);
    tctx
}
