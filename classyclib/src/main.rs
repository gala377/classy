use std::collections::HashSet;

use colored::*;

use classyclib::{
    ast_passes::{
        gather_runtime_functions, run_befor_type_context_passes, run_before_typechecking_passes,
    },
    code::constant_pool::ConstantPool,
    syntax::{ast, ast::Visitor, lexer::Lexer, parser::Parser},
    typecheck::{self, add_types::AddTypes, type_context::TypCtx},
};

const SOURCE: &str = r#"
    type MyFoo {
        a: String
    }

    is_int: (Int) -> ()
    is_int i = ()
    
    type Alias = () -> Int

    main: () -> ()
    main { 
        let b = if (true) {
            let a = 1
            a
        } else {
            let a = 2
            a
        }
        while (true) {
            print "Hello world"
        }
        print "Hello world"
        let c = type { a = "Hello"; b = 10 }
        let d = c.b;

        c.a = "Hello again"
    }
"#;

fn main() {
    let package = make_package();
    compile(SOURCE, &[package]);
    //   compile(SOURCE, &[]);
    //println!("{}", tctx.debug_string());
    // let package = classyclib::package::Package::new("main", &tctx);
    // let out = std::fs::File::create("out.json").unwrap();
    // serde_json::to_writer_pretty(out, &package).unwrap();
}

fn compile(source: &str, packages: &[classyclib::package::Package]) -> TypCtx {
    let res = parse_source(source);
    let res = run_befor_type_context_passes(res);
    let mut tctx = TypCtx::new();
    for package in packages {
        package.read_headers(&mut tctx);
    }
    let mut tctx = prepare_type_ctx(tctx, &res);
    println!("{}", tctx.debug_string());
    let res = run_before_typechecking_passes(&tctx, res);
    let tenv = typecheck::run(&mut tctx, &res);
    let mut constant_pool = ConstantPool::new();
    let mut gatherer = gather_runtime_functions::GatherRuntimeFunctions::new();
    gatherer.visit(&res);
    let runtime_functions: HashSet<String> = gatherer.res.into_iter().collect();
    for def in &res.items {
        if let ast::TopLevelItem::FunctionDefinition(fdef) = def {
            println!("\n\n\nFunction definition {:#?}", fdef.name);
            let emmiter = classyclib::ir::Emitter::new(&tctx, &tenv);
            let block = emmiter.emit_fn(fdef);
            for (i, instr) in block.body.iter().enumerate() {
                println!("{} {:?}", format!("{i:03}|").dimmed(), instr);
            }
            println!("\n\nCompiled:");
            let compiled = classyclib::emitter::compile_ir_function(
                &block,
                runtime_functions.clone(),
                &tctx,
                &mut constant_pool,
            );
            classyclib::code::debug::debug_print_code(&compiled.instructions, &constant_pool);
        }
    }
    tctx
}

pub fn make_package() -> classyclib::package::Package {
    let source = r#"
        type MyString = String

        print: (MyString) -> ()
        print s = ()
    "#;
    let tctx = compile(source, &[]);
    let pckg = classyclib::package::Package::new("test", &tctx);
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
    add_types.visit(ast);
    println!("{}", tctx.debug_string());
    tctx = typecheck::resolve_type_names(tctx);
    println!("{}", tctx.debug_string());
    println!("RESOLVING ALIASES");
    typecheck::alias_resolver::AliasResolver::resolve(&mut tctx);
    println!("{}", tctx.debug_string());
    typecheck::dedup_trivially_eq_types(&mut tctx);
    tctx
}
