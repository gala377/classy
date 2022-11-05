use std::sync::{Arc, Mutex};

use clap::Parser;
use classy_vm::mem::{allocator::Allocator, heap::SemiSpace, page::Page, ptr::Ptr};

const PAGE_SIZE: usize = 1 << 12;
const PAGE_ALIGN: usize = 1 << 12;

#[derive(Parser, Debug)]
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
    std::thread::scope(|scope| {
        for _ in 0..args.threads {
            scope.spawn(run_thread(heap.clone(), &args));
        }
    });
    println!("Done")
}

fn run_thread<'a>(
    SemiSpaces {
        from_space,
        to_space,
    }: SemiSpaces,
    Args {
        page_size,
        pages_count,
        allocate_integers,
        ..
    }: &'a Args,
) -> impl FnOnce() + 'a {
    move || {
        let mut thread = classy_vm::runtime::thread::Thread::new(
            from_space,
            to_space,
            page_size - std::mem::size_of::<Page>(),
            page_size * pages_count,
        );
        for _ in 0..*allocate_integers {
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
