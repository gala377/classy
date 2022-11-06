use std::sync::{Arc, Mutex};

use clap::Parser;
use classy_vm::{
    mem::{allocator::Allocator, heap::SemiSpace, page::Page, ptr::Ptr},
    runtime::thread_manager::{self, ThreadManager},
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
    let heap = setup_semispaces(&args);
    let thread_manager = ThreadManager::new();
    for _ in 0..args.threads {
        start_thread(heap.clone(), Arc::clone(&thread_manager), &args);
    }
    wait_for_all_threads(thread_manager);
    println!("Done")
}

fn wait_for_all_threads(thread_manager: Arc<ThreadManager>) {
    while thread_manager.current_threads_count() > 0 {
        std::thread::yield_now();
    }
}

fn start_thread(semi_spaces: SemiSpaces, thread_manager: Arc<ThreadManager>, args: &Args) {
    while let Err(thread_manager::StoppedForGc) = thread_manager.new_thread(make_thread_loop(
        semi_spaces.clone(),
        Arc::clone(&thread_manager),
        args,
    )) {
        // not much we can do, let's just sleep a bit
        std::thread::sleep(std::time::Duration::from_secs(1))
    }
}

fn make_thread_loop(
    SemiSpaces {
        from_space,
        to_space,
    }: SemiSpaces,
    thread_manager: Arc<ThreadManager>,
    Args {
        page_size,
        pages_count,
        allocate_integers,
        ..
    }: &Args,
) -> impl FnOnce() + Send + 'static {
    let page_size = *page_size;
    let pages_count = *pages_count;
    let allocate_integers = *allocate_integers;
    move || {
        let mut thread = classy_vm::runtime::thread::Thread::new(
            thread_manager,
            from_space,
            to_space,
            page_size - std::mem::size_of::<Page>(),
            page_size * pages_count,
        );
        for _ in 0..allocate_integers {
            let Ptr(ptr) = thread.alloc::<u64>();
            ptr.expect("could not allocate");
        }
    }
}

#[derive(Clone)]
struct SemiSpaces {
    from_space: SemiSpace,
    to_space: SemiSpace,
}

fn setup_semispaces(args: &Args) -> SemiSpaces {
    let from_space = Allocator::new(args.page_size, args.page_align);
    let to_space = Allocator::new(args.page_size, args.page_align);
    SemiSpaces {
        from_space: Arc::new(Mutex::new(from_space)),
        to_space: Arc::new(Mutex::new(to_space)),
    }
}
