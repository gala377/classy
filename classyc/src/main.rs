use std::{
    collections::{HashMap, HashSet},
    mem::size_of,
    path::Path,
};

use clap::Parser;
use colored::Colorize;

use classy_syntax::ast::{self, Visitor};

use classyclib::{
    code::constant_pool::ConstantPool,
    v2::knowledge::{Definition, DefinitionId, DefinitionKind, LocalId, PackageInfo, TypeId},
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

    #[arg(long)]
    use_v2: bool,
}

fn main() {
    color_eyre::install().unwrap();
    let args = Args::parse();
    if args.use_v2 {
        compile_with_v2(args);
        return;
    }
    panic!("v1 compilation no longer supported")
    // let mut vm = Vm::new_default(vm::Options {
    //     page_size: args.page_size + size_of::<Page>(),
    //     page_align: args.page_align,
    //     young_space_size: (args.page_size + size_of::<Page>()) *
    // args.pages_count,     initial_tlab_size: args.page_size,
    //     debug: args.debug,
    // });
    // let file = args.file.expect("file path has to be provided");
    // let source = std::fs::read_to_string(Path::new(&file)).unwrap();
    // let functions = compile(&mut vm, &source, args.print_ast);
    // let Some(functions) = functions else {
    //     return;
    // };
    // let mut thread = vm.create_evaluation_thread(functions["main"]);
    // thread.interpert();
}

fn compile_with_v2(args: Args) {
    let core = prepare_std_package();
    let file = args.file.expect("file path has to be provided");
    let source = std::fs::read_to_string(Path::new(&file)).unwrap();
    let mut compiler =
        classyclib::v2::compile::Compiler::new("test", vec![(file.into(), source)], vec![core]);
    compiler.compile_package().unwrap();
}

fn prepare_std_package() -> PackageInfo {
    let types = vec![
        ("String", classyclib::v2::ty::Type::String),
        ("Int", classyclib::v2::ty::Type::Int),
        ("Bool", classyclib::v2::ty::Type::Bool),
        ("Float", classyclib::v2::ty::Type::Float),
        ("Byte", classyclib::v2::ty::Type::Byte),
        ("UInt", classyclib::v2::ty::Type::UInt),
        ("Unit", classyclib::v2::ty::Type::Unit),
    ];
    PackageInfo {
        name: "std".to_string(),
        globals: {
            let mut globals = HashMap::new();
            for (i, (name, _)) in types.iter().enumerate() {
                globals.insert(name.to_string(), LocalId(DefinitionId(i)));
            }
            globals
        },
        definition: {
            let mut map = HashMap::new();
            for (i, (name, _)) in types.iter().enumerate() {
                map.insert(
                    LocalId(DefinitionId(i)),
                    Definition {
                        name: name.to_string(),
                        kind: DefinitionKind::Type,
                        constraints: vec![],
                        ty: LocalId(TypeId(i)),
                        file: LocalId(DefinitionId(0)),
                        implicit_imports: vec![],
                        parent: None,
                        annotations: Default::default(),
                    },
                );
            }
            map
        },
        typeid_to_type: {
            let mut map = HashMap::new();
            for (i, (_, ty)) in types.iter().enumerate() {
                map.insert(LocalId(TypeId(i)), ty.clone());
            }
            map
        },
        method_blocks: Default::default(),
        classes: Default::default(),
        instances: Default::default(),
        methods: Default::default(),
    }
}
