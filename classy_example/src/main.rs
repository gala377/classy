use clap::Parser;
use classy_vm::mem::allocator::Allocator;

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
}

fn main() {
    let args = Args::parse();
    let mut alloc = Allocator::new();
    for _ in 0..args.pages_count {
        alloc.allocate_page(args.page_size, args.page_align);
    }
    println!(
        "Allocated {n} pages. Together {sum} bytes of memeory.",
        n = args.pages_count,
        sum = args.pages_count * args.page_size
    );
}
