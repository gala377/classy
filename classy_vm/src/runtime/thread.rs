use std::{alloc::Layout, sync::Arc};

use crate::mem::{
    heap::{self, Heap, SemiSpace},
    ptr::Ptr,
};

use super::thread_manager::ThreadManager;

pub struct Thread {
    heap: Heap,
}

impl Thread {
    pub fn new(
        thread_manager: Arc<ThreadManager>,
        from_space: SemiSpace,
        to_space: SemiSpace,
        initial_tlab_free_size: usize,
        max_young_space_size: usize,
    ) -> Self {
        let id = std::thread::current().id();
        Thread {
            heap: Heap::new(
                id,
                from_space,
                to_space,
                thread_manager,
                heap::Options {
                    initial_tlab_free_size,
                    max_young_space_size,
                },
            ),
        }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        unsafe { self.heap.try_allocate(Layout::new::<T>()).cast() }
    }
}
