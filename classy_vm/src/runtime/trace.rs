#![allow(unstable_name_collisions)]
use std::{
    alloc::Layout,
    collections::VecDeque,
    mem::{align_of, size_of},
    ptr::NonNull,
};

use sptr::Strict;

use crate::{
    mem::{
        allocator::Allocator,
        bump::BumpAllocator,
        ptr::{ErasedNonNull, ErasedPtr, NonNullPtr},
    },
    runtime::class::header::Header,
};

pub trait Tracer {
    fn trace_pointer(&mut self, ptr: ErasedPtr) -> *mut () {
        match ptr.inner() {
            None => std::ptr::null_mut(),
            Some(ptr) => self.trace_nonnull_pointer(ErasedNonNull::new(ptr)),
        }
    }

    fn trace_nonnull_pointer(&mut self, ptr: ErasedNonNull) -> *mut ();
}

pub struct Gc<'a> {
    to_space: &'a mut Allocator,
    to_page: BumpAllocator,
    worklist: VecDeque<ErasedPtr>,
}

impl<'a> Gc<'a> {
    pub fn new(to_space: &'a mut Allocator) -> Self {
        // this cannot use allocate_page as the pages could already be allocated
        let to_page = unsafe { NonNullPtr::from_ptr(to_space.allocate_page()) };
        Self {
            to_space,
            to_page: BumpAllocator::new(to_page.inner()),
            worklist: VecDeque::new(),
        }
    }

    /// Pointers passed as roots have to be nonull
    pub unsafe fn collect(&mut self, roots: &[*mut ErasedPtr]) {
        println!("\n\n\nTracing roots\n\n\n");
        for root in roots.iter().cloned() {
            let ptr = ErasedPtr::new(self.trace_pointer(*root));
            *root = ptr;
            self.worklist.push_back(ptr);
        }
        println!("\n\n\nTracing heap\n\n\n");
        while let Some(ptr) = self.worklist.pop_front() {
            if let Some(ptr) = ptr.inner() {
                let header = (ptr.as_ptr() as *mut Header).sub(1);
                let class = (*header).class;
                let trace = (*class.get()).trace;
                trace(ptr.as_ptr(), self);
            }
        }
        println!("Done");
    }
}

impl Tracer for Gc<'_> {
    /// Traced pointer has to be a valid managed heap pointer.
    /// Meaning it cannot point to unallocated or freed memory and
    /// this momeory has to preceeded by a header.
    /// It can be null.
    fn trace_nonnull_pointer(&mut self, ptr: ErasedNonNull) -> *mut () {
        unsafe {
            println!("Tracing pointer");
            let header = ptr.header();
            if let Some(address) = (*header.as_ptr()).forward_address() {
                println!("Forward address is {address:018x}");
                return sptr::from_exposed_addr_mut(address);
            }
            if (*header.as_ptr()).permament_allocation() {
                println!("This pointer is a permament allocation it doesn't move");
                return ptr.get();
            }
            let ptr = copy_to_space(ptr, self.to_space, &mut self.to_page);
            self.worklist.push_back(ErasedPtr::new(ptr));
            ptr
        }
    }
}

unsafe fn copy_to_space(
    ptr: ErasedNonNull,
    to_space: &mut Allocator,
    to_page: &mut BumpAllocator,
) -> *mut () {
    let header = ptr.header();
    let size = copy_size(header);
    let align = align_of::<Header>();
    let layout = Layout::from_size_align(size, align).unwrap();
    let ptr = match to_page.alloc_layout(layout).inner() {
        Some(ptr) => ptr.as_ptr(),
        None => {
            let new_page = to_space
                .get_page_unowned(size, align)
                .inner()
                .or_else(|| to_space.allocate_page().inner())
                .expect("to space cannot contain an object from a from space");
            *to_page = BumpAllocator::new(new_page);
            to_page
                .alloc_layout(layout)
                .inner()
                .expect("technically unreachable")
                .as_ptr()
        }
    };
    std::ptr::copy_nonoverlapping(header.as_ptr() as *const u8, ptr as *mut u8, size);
    let new_ptr = ptr.cast::<Header>().add(1);
    (*header.as_ptr()).set_forward_address(new_ptr.expose_addr());
    new_ptr as *mut ()
}

unsafe fn copy_size(header: NonNull<Header>) -> usize {
    let cls = Header::class(header);
    let actual_instance_size = (*cls.get()).actual_instance_size;
    let instance_size = match actual_instance_size {
        Some(size_fn) => size_fn(Header::object_ptr(header).get()),
        None => (*cls.get()).instance_size,
    };
    size_of::<Header>() + instance_size
}
