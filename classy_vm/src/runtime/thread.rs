use std::{alloc::Layout, sync::Arc};

use crate::{
    mem::{
        heap::{self, Heap, SemiSpace},
        permament_heap,
        ptr::Ptr,
        ObjectAllocator,
    },
    runtime::{thread_manager::ThreadManager, Runtime},
};

pub struct Thread {
    heap: Heap,
    _permament_heap: Arc<permament_heap::PermamentHeap>,
    _runtime: Runtime,
    _thread_manager: Arc<ThreadManager>,
}

impl Thread {
    pub fn new(
        runtime: Runtime,
        thread_manager: Arc<ThreadManager>,
        from_space: SemiSpace,
        to_space: SemiSpace,
        permament_heap: Arc<permament_heap::PermamentHeap>,
        initial_tlab_free_size: usize,
        max_young_space_size: usize,
    ) -> Self {
        let id = std::thread::current().id();
        Thread {
            heap: Heap::new(
                id,
                from_space,
                to_space,
                thread_manager.clone(),
                heap::Options {
                    initial_tlab_free_size,
                    max_young_space_size,
                },
            ),
            _permament_heap: permament_heap,
            _runtime: runtime,
            _thread_manager: thread_manager,
        }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        unsafe { self.heap.try_allocate(Layout::new::<T>()).cast() }
    }
}
