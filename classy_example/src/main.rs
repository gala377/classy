use std::{
    collections::{HashMap, HashSet},
    mem::size_of,
    path::Path,
};

use clap::{Parser, ValueEnum};
use colored::Colorize;

use classy_c::{
    ast_passes::run_before_typechecking_passes,
    code::constant_pool::ConstantPool,
    syntax::ast::{self, Visitor},
    typecheck::{self, add_types::AddTypes, type_context::TypCtx},
};
use classy_vm::{
    mem::{page::Page, ptr::NonNullPtr},
    vm::{self, Vm},
};

const PAGE_SIZE: usize = 1 << 12;
const PAGE_ALIGN: usize = 1 << 12;

#[derive(Eq, PartialEq, PartialOrd, Ord, Copy, Debug, Clone, ValueEnum)]
enum Example {
    Allocation,
    Print,
    RunFile,
}

#[derive(Parser, Debug, Clone)]
#[command(author, version, about)]
struct Args {
    #[arg(long, default_value_t = 1)]
    pages_count: usize,

    #[arg(long, default_value_t = PAGE_SIZE)]
    page_size: usize,

    #[arg(long, default_value_t = PAGE_ALIGN)]
    page_align: usize,

    #[arg(long, default_value_t = 0)]
    allocate_integers: usize,

    #[arg(long, default_value_t = 1)]
    threads: usize,

    #[arg(long)]
    create_handle_every: Option<usize>,

    #[arg(long, value_enum, default_value_t = Example::Print)]
    example: Example,

    #[arg(long, default_value_t = false)]
    debug: bool,

    #[arg(long)]
    file: Option<String>,
}

fn main() {
    let args = Args::parse();
    let mut vm = Vm::new_default(vm::Options {
        page_size: args.page_size + size_of::<Page>(),
        page_align: args.page_align,
        young_space_size: (args.page_size + size_of::<Page>()) * args.pages_count,
        initial_tlab_size: args.page_size,
        debug: args.debug,
    });
    match args.example {
        Example::Print => {
            let source = r#"
                @runtime @empty 
                print: (String) -> ()
                @runtime @empty
                header_data: (a) -> Int
                @runtime @empty
                itos: (Int) -> String
                @runtime @empty
                print_n_times: (String, Int) -> ()
                @runtime @empty
                byte_copy: (String, [Byte], Int, Int) -> ()
                @runtime @empty
                add: (Int, Int) -> Int
                @runtime @empty
                str_from_bytes: ([Byte]) -> String
                @runtime @empty
                class_name: (a) -> String

                type Option(a) {
                    Some(a)
                    None
                }

                type Either(a, b) {
                    Left(a)
                    Right(b)
                }
                
                type Integer {
                    val: Int
                }

                printi x = print (itos x)

                print_either: (Either(Option(Integer), String)) -> ()
                print_either a = a match {
                    Either.Left(Option.Some(Integer { val: x })) => printi x
                    Either.Right(x) => print x
                }


            "#;
            let functions = compile(&mut vm, source);
            let mut thread = vm.create_evaluation_thread(functions["main"]);
            thread.interpert();
        }
        Example::RunFile => {
            let file = args.file.expect("file path has to be provided");
            let source = std::fs::read_to_string(Path::new(&file)).unwrap();
            let functions = compile(&mut vm, &source);
            let mut thread = vm.create_evaluation_thread(functions["main"]);
            thread.interpert();
        }
        _ => panic!("Not implemented"),
    }
}

fn compile(
    vm: &mut Vm,
    source: &str,
) -> HashMap<String, NonNullPtr<classy_vm::runtime::class::code::Code>> {
    let mut parser =
        classy_c::syntax::parser::Parser::new(classy_c::syntax::lexer::Lexer::new(source));
    let ast = parser.parse().unwrap();
    let ast = classy_c::ast_passes::run_befor_type_context_passes(ast);
    let tctx = classy_c::typecheck::type_context::TypCtx::new();
    let mut tctx = prepare_type_ctx(tctx, &ast);
    println!("Type ctxt: {}", tctx.debug_string());
    let res = run_before_typechecking_passes(&tctx, ast);
    //println!("AST: {:#?}", res);
    let tenv = typecheck::run(&mut tctx, &res);

    let mut constant_pool = ConstantPool::new();
    let mut functions = HashMap::new();
    let mut gatherer =
        classy_c::ast_passes::gather_runtime_functions::GatherRuntimeFunctions::new();
    gatherer.visit(&res);
    let runtime_functions: HashSet<String> = gatherer.res.into_iter().collect();
    for def in &res.items {
        if let ast::TopLevelItem::FunctionDefinition(fdef) = def {
            let emmiter = classy_c::ir::Emitter::new(&tctx, &tenv);
            let block = emmiter.emit_fn(fdef);
            println!("\n\n\nFunction definition {:#?}", fdef.name);
            for (i, instr) in block.body.iter().enumerate() {
                println!("{} {:?}", format!("{i:03}|").dimmed(), instr);
            }
            let compiled = classy_c::emitter::compile_ir_function(
                &block,
                runtime_functions.clone(),
                &tctx,
                &mut constant_pool,
            );
            println!("\n");
            classy_c::code::debug::debug_print_code(&compiled.instructions, &constant_pool);
            functions.insert(fdef.name.clone(), compiled);
        }
    }
    vm.load_types(&tctx, &constant_pool);
    let mut functions_vec: Vec<_> = functions
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    vm.load_functions(&mut functions_vec, &constant_pool)
}

pub fn prepare_type_ctx(mut tctx: TypCtx, ast: &ast::Program) -> TypCtx {
    let mut add_types = AddTypes::with_primitive_types(&mut tctx);
    add_types.visit(ast);
    tctx = typecheck::resolve_type_names(tctx);
    typecheck::alias_resolver::AliasResolver::resolve(&mut tctx);
    typecheck::dedup_trivially_eq_types(&mut tctx);
    tctx
}
