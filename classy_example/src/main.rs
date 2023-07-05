use std::{
    collections::{HashMap, HashSet},
    mem::size_of,
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
                header_data: (String) -> Int

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

                concat_strings(s1, s2) {
                    let len1 = header_data s1
                    let len2 = header_data s2
                    let len = add(len1, len2)
                    let res = array[len]Byte
                    byte_copy(s1, res, 0, len1)
                    byte_copy(s2, res, len1, len2)
                    str_from_bytes res
                }

                print_constant {
                    print_2("Hello world", 10)
                }

                print_2(s, i) {
                    print_n_times(s, i)
                }


                print_twice s {
                    print(s)
                    print(s)
                }

                type Integer { value: Int }

                print_integer: (Integer) -> ()
                print_integer i {
                    print(itos(i.value))
                }

                main { 
                    print "Hello world1" 
                    print_twice "Hello macarena"
                    let a = type { a = "Hello"; b = 10 }
                    let arr = array{"a", "b", "c", "d"}
                    print(itos(header_data(arr[0])))
                    print(itos(header_data(a.a)))
                    print_n_times("test", 10)
                    print(concat_strings("Hello", " world"))
                    print_integer(Integer(value=10))
                }
            "#;
            let functions = compile(&mut vm, source);
            let mut thread = vm.create_evaluation_thread(functions["main"].clone());
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
    add_types.visit(&ast);
    tctx = typecheck::resolve_type_names(tctx);
    typecheck::alias_resolver::AliasResolver::resolve(&mut tctx);
    typecheck::dedup_trivially_eq_types(&mut tctx);
    tctx
}
