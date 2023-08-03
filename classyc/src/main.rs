use std::{
    collections::{HashMap, HashSet},
    mem::size_of,
    path::Path,
};

use clap::Parser;
use colored::Colorize;

use classy_syntax::ast::{self, Visitor};

use classyclib::{
    ast_passes::run_before_typechecking_passes,
    code::constant_pool::ConstantPool,
    typecheck::{self, add_types::AddTypes, type_context::TypCtx},
};
use classyvm::{
    mem::{page::Page, ptr::NonNullPtr},
    vm::{self, Vm},
};

const PAGE_SIZE: usize = 1 << 12;
const PAGE_ALIGN: usize = 1 << 12;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about)]
struct Args {
    #[arg(long, default_value_t = 1)]
    pages_count: usize,

    #[arg(long, default_value_t = PAGE_SIZE)]
    page_size: usize,

    #[arg(long, default_value_t = PAGE_ALIGN)]
    page_align: usize,

    #[arg(long)]
    create_handle_every: Option<usize>,

    #[arg(long, default_value_t = false)]
    debug: bool,

    #[arg(long)]
    file: Option<String>,

    #[arg(long)]
    print_ast: bool,
}

fn main() {
    color_eyre::install().unwrap();
    let args = Args::parse();
    let mut vm = Vm::new_default(vm::Options {
        page_size: args.page_size + size_of::<Page>(),
        page_align: args.page_align,
        young_space_size: (args.page_size + size_of::<Page>()) * args.pages_count,
        initial_tlab_size: args.page_size,
        debug: args.debug,
    });
    let file = args.file.expect("file path has to be provided");
    let source = std::fs::read_to_string(Path::new(&file)).unwrap();
    let functions = compile(&mut vm, &source, args.print_ast);
    let Some(functions) = functions else {
        return;
    };
    let mut thread = vm.create_evaluation_thread(functions["main"]);
    thread.interpert();
}

fn compile(
    vm: &mut Vm,
    source: &str,
    print_ast: bool,
) -> Option<HashMap<String, NonNullPtr<classyvm::runtime::class::code::Code>>> {
    let mut parser = classy_syntax::parser::Parser::new(classy_syntax::lexer::Lexer::new(source));
    let ast = parser.parse().unwrap();
    let sess = classyclib::session::Session::new("test");
    let ast = classyclib::ast_passes::run_after_parsing_passes(ast, &sess);
    let mut tctx = typecheck::prepare_for_typechecking(&ast);
    println!("Type ctxt: {}", tctx.debug_string());
    let res = run_before_typechecking_passes(&tctx, ast, &sess);
    if print_ast {
        println!("AST: {:#?}", res);
        return None;
    }
    let tenv = typecheck::run(&mut tctx, &res, &sess);

    let mut constant_pool = ConstantPool::new();
    let mut functions = HashMap::new();
    let mut gatherer =
        classyclib::ast_passes::gather_runtime_functions::GatherRuntimeFunctions::new();
    gatherer.visit(&res);
    let runtime_functions: HashSet<String> = gatherer.res.into_iter().collect();
    for def in &res.items {
        if let ast::TopLevelItemKind::FunctionDefinition(fdef) = &def.kind {
            let emmiter = classyclib::ir::Emitter::new(&tctx, &tenv);
            let block = emmiter.emit_fn(fdef);
            println!("\n\n\nFunction definition {:#?}", fdef.name);
            for (i, instr) in block.body.iter().enumerate() {
                println!("{} {:?}", format!("{i:03}|").dimmed(), instr);
            }
            let compiled = classyclib::emitter::compile_ir_function(
                &block,
                runtime_functions.clone(),
                &tctx,
                &mut constant_pool,
            );
            println!("\n");
            classyclib::code::debug::debug_print_code(&compiled.instructions, &constant_pool);
            functions.insert(fdef.name.clone(), compiled);
        }
    }
    vm.load_types(&tctx, &constant_pool);
    let mut functions_vec: Vec<_> = functions
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    Some(vm.load_functions(&mut functions_vec, &constant_pool))
}

pub fn prepare_type_ctx(mut tctx: TypCtx, ast: &ast::SourceFile) -> TypCtx {
    let mut add_types = AddTypes::with_primitive_types(&mut tctx);
    add_types.visit(ast);
    tctx = typecheck::resolve_type_names(tctx);
    typecheck::alias_resolver::AliasResolver::resolve(&mut tctx);
    typecheck::dedup_trivially_eq_types(&mut tctx);
    tctx
}
