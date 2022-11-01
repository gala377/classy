use std::sync::{Arc, Mutex};

use clap::Parser;
use classy_vm::mem::{allocator::Allocator, page::Page, ptr::Ptr};

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
    let allocator = match setup_allocator(&args) {
        None => return,
        Some(a) => a,
    };
    std::thread::scope(|scope| {
        for _ in 0..args.threads {
            scope.spawn(run_thread(allocator.clone(), &args));
        }
    });
    println!("Done")
}

fn run_thread<'a>(allocator: Arc<Mutex<Allocator>>, args: &'a Args) -> impl FnOnce() + 'a {
    move || {
        let mut thread =
            classy_vm::thread::Thread::new(allocator, args.page_size - std::mem::size_of::<Page>());
        for _ in 0..args.allocate_integers {
            let Ptr(ptr) = thread.alloc::<u64>();
            ptr.expect("could not allocate");
        }
    }
}

fn setup_allocator(args: &Args) -> Option<Arc<Mutex<Allocator>>> {
    let mut alloc = Allocator::new(args.page_size, args.page_align);
    if args.pages_count == 0 {
        return None;
    }
    for _ in 0..args.pages_count {
        unsafe { alloc.allocate_page() };
    }
    println!(
        "Allocated {n} pages. Together {sum} bytes of memeory.",
        n = args.pages_count,
        sum = args.pages_count * args.page_size
    );
    Some(Arc::new(Mutex::new(alloc)))
}
