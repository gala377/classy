use std::mem::size_of;

use clap::{Parser, ValueEnum};

use classy_c::code::constant_pool::ConstantPool;
use classy_vm::{
    mem::{
        page::Page,
        ptr::{NonNullPtr, Ptr},
    },
    runtime::thread_manager,
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
}

fn main() {
    let args = Args::parse();
    let vm = Vm::new_default(vm::Options {
        page_size: args.page_size + size_of::<Page>(),
        page_align: args.page_align,
        young_space_size: (args.page_size + size_of::<Page>()) * args.pages_count,
        initial_tlab_size: args.page_size,
    });
    match args.example {
        Example::Allocation => {
            for _ in 0..args.threads {
                start_thread(&vm, &args);
            }
            wait_for_all_threads(vm);
            println!("Done")
        }
        Example::Print => {
            let source = r#"
                main:()->();
                main { 
                    print "Hello world" 
                    print "Hello world" 
                    print "Hello world" 
                    print "Hello world" 
                    print "Hello world" 
                    print "Hello world" 
                    print "Hello world 2"
                };
            "#;
            let mut parser =
                classy_c::syntax::parser::Parser::new(classy_c::syntax::lexer::Lexer::new(&source));
            let _ast = parser.parse().unwrap();
            // let code = classy_c::emitter::ast::AstEmmiter::compile(ast);
            // let mut thread = vm.create_evaluation_thread(code);
            // thread.interpert();
        }
    }
}

fn wait_for_all_threads(vm: Vm) {
    // we need to count that vm registers current thread
    while vm.thread_manager().current_threads_count() > 1 {
        if vm.thread_manager().should_stop_thread_for_gc() {
            vm.thread_manager().stop_for_gc().unwrap()
        } else {
            std::thread::yield_now()
        }
    }
}

fn start_thread(vm: &Vm, args: &Args) {
    let thread_manager = vm.thread_manager();
    while let Err(thread_manager::StoppedForGc) =
        thread_manager.new_thread(make_thread_loop(vm.clone(), args))
    {
        thread_manager.stop_for_gc().unwrap();
    }
}

fn make_thread_loop(
    mut vm: Vm,
    Args {
        allocate_integers,
        create_handle_every,
        ..
    }: &Args,
) -> impl FnOnce() + Send + 'static {
    let allocate_integers = *allocate_integers;
    let create_handle_every = *create_handle_every;
    move || {
        let mut thread = vm.create_evaluation_thread(classy_c::code::Code::new(), &ConstantPool::new());
        let runtime = vm.runtime();
        let mut handles = Vec::new();
        for i in 0..allocate_integers {
            let Ptr(ptr) = unsafe { thread.allocate_instance::<isize>(runtime.classes.int) };
            let ptr = ptr.expect("could not allocate");
            let Some(modulo) = create_handle_every else {
                continue;
            };
            if i % modulo == 0 {
                //println!("Creating handle");
                handles.push(unsafe { thread.create_handle(NonNullPtr::new(ptr)) });
            }
        }
    }
}
