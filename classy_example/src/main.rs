use std::sync::Arc;

use clap::Parser;
use classy_vm::{
    mem::{page::Page, ptr::Ptr},
    runtime::thread_manager::{self, ThreadManager},
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

    #[arg(long, default_value_t = 0)]
    allocate_integers: usize,

    #[arg(long, default_value_t = 1)]
    threads: usize,
}

fn main() {
    println!(
        "Size and align of page: {} / {}",
        std::mem::size_of::<Page>(),
        std::mem::align_of::<Page>()
    );
    let args = Args::parse();
    let vm = Vm::new_default(vm::Options {
        page_size: args.page_size,
        page_align: args.page_align,
        young_space_size: args.page_size * args.pages_count,
        initial_tlab_size: args.page_size - std::mem::size_of::<Page>(),
    });
    for _ in 0..args.threads {
        start_thread(&vm, &args);
    }
    wait_for_all_threads(vm.thread_manager());
    println!("Done")
}

fn wait_for_all_threads(thread_manager: Arc<ThreadManager>) {
    // we need to count that vm registers current thread
    while thread_manager.current_threads_count() > 1 {
        std::thread::yield_now();
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
        allocate_integers, ..
    }: &Args,
) -> impl FnOnce() + Send + 'static {
    let allocate_integers = *allocate_integers;
    move || {
        let mut thread = vm.create_evaluation_thread();
        for _ in 0..allocate_integers {
            let Ptr(ptr) = thread.alloc::<u64>();
            ptr.expect("could not allocate");
        }
    }
}
