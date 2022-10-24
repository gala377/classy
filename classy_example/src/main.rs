use clap::Parser;
use classy_vm::mem::{allocator::Allocator, bump::BumpAllocator, page::Page};

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
}

fn main() {
    println!(
        "Size and align of page: {} / {}",
        std::mem::size_of::<Page>(),
        std::mem::align_of::<Page>()
    );
    let args = Args::parse();
    let mut alloc = Allocator::new();
    if args.pages_count == 0 {
        return;
    }
    let page = alloc.allocate_page(args.page_size, args.page_align);
    for _ in 1..args.pages_count {
        alloc.allocate_page(args.page_size, args.page_align);
    }
    println!(
        "Allocated {n} pages. Together {sum} bytes of memeory.",
        n = args.pages_count,
        sum = args.pages_count * args.page_size
    );

    println!("Allocating integers");
    let page = page.inner().expect("should not be none");
    let mut bump = BumpAllocator::new(page);
    for i in 0..args.allocate_integers {
        if bump.alloc::<u64>().is_null() {
            println!("Could not allocate integer {i}");
        }
    }
}
