use std::alloc::Layout;

use crate::runtime::class::header::Header;

use super::{ptr::ErasedPtr, ObjectAllocator};

pub struct PermamentHeap {
    allocated_bytes: usize,
    allocated_addresses: Vec<(*mut u8, Layout)>,
}

impl PermamentHeap {
    pub fn new() -> Self {
        Self {
            allocated_bytes: 0,
            allocated_addresses: Vec::new(),
        }
    }

    pub fn free_allocated(&mut self) {
        self.allocated_bytes = 0;
        unsafe {
            // todo:
            // does this work? Because we can drop a class that needs to exists
            // so that other classes get dropped, but drop does leave instance
            // in a valid state so... maybe?
            for (addr, _) in self.allocated_addresses.iter().cloned() {
                run_drop(addr);
            }
            for (addr, layout) in self.allocated_addresses.iter().cloned() {
                std::alloc::dealloc(addr, layout);
            }
        }
    }
}

unsafe fn run_drop(addr: *mut u8) {
    let header = (addr as *mut Header).read();
    let obj = (addr as *mut Header).add(1) as _;
    (*header.class.get()).drop_instance(obj);
}

impl ObjectAllocator for PermamentHeap {
    fn try_allocate(&mut self, layout: Layout) -> ErasedPtr {
        let ptr = unsafe { std::alloc::alloc(layout) };
        if !ptr.is_null() {
            self.allocated_bytes += layout.size();
            self.allocated_addresses.push((ptr, layout));
        }
        ErasedPtr::new(ptr as _)
    }

    fn adjust_header(&mut self, header: &mut Header) {
        header.set_permament_allocation();
    }
}

impl Drop for PermamentHeap {
    fn drop(&mut self) {
        self.free_allocated();
    }
}
