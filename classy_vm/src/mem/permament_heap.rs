use std::alloc::Layout;

use crate::{
    mem::{ptr::ErasedPtr, ObjectAllocator},
    runtime::class::{self, header::Header},
};

#[derive(Default)]
pub struct PermamentHeap {
    allocated_bytes: usize,
    allocated_addresses: Vec<(*mut u8, Layout)>,
}

// We do not access allocated addresses, only on drop.
// But the heap is shared via the Arc so there is no conflict.
unsafe impl Send for PermamentHeap {}
unsafe impl Sync for PermamentHeap {}

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
    let class = header.class;
    class::drop_instance(class, obj);
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

#[cfg(test)]
mod tests {
    use std::{alloc::Layout, mem::align_of, mem::size_of};

    use crate::{
        mem::{
            ptr::{NonNullPtr, Ptr},
            ObjectAllocator,
        },
        runtime::class::{
            self,
            header::{self, Header},
            Class,
        },
    };

    use super::PermamentHeap;

    fn setup_klass(heap: &mut PermamentHeap) -> NonNullPtr<Class> {
        unsafe {
            let layout = Layout::from_size_align(
                size_of::<Header>() + size_of::<Class>(),
                align_of::<Header>(),
            )
            .unwrap();
            let klass_ptr = heap.try_allocate(layout);
            let Some(klass_ptr) = klass_ptr.inner() else {
                panic!("Could not allocate Klass inside the permament heap")
            };
            let header_ptr = klass_ptr.as_ptr() as *mut Header;
            let class_ptr = header_ptr.add(1) as *mut Class;
            let header = Header {
                class: NonNullPtr::new_unchecked(class_ptr),
                flags: header::Flags::PermamentHeap as usize,
                data: 0,
            };
            let klass = class::klass::KLASS_CLASS;
            std::ptr::write(header_ptr, header);
            std::ptr::write(class_ptr, klass);
            NonNullPtr::new_unchecked(class_ptr)
        }
    }

    #[test]
    fn test_simple_allocation_in_a_permament_heap() {
        let mut heap = PermamentHeap::new();
        let klass = setup_klass(&mut heap);
        let test_class = Class {
            name: Ptr::null(),
            drop: None,
            trace: class::instance_trace,
            instance_size: 3 * size_of::<usize>(),
            instance_align: align_of::<usize>(),
            actual_instance_size: None,
            kind: class::Kind::Instance,
        };
        let _ = heap.allocate_class(test_class, &[], klass);
    }
}
