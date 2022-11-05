#![allow(unreachable_code)]

use std::{
    alloc::Layout,
    sync::{Arc, Mutex},
    thread::ThreadId,
};

use crate::{
    mem::{
        allocator::Allocator,
        ptr::{ErasedPtr, Ptr},
    },
    runtime::tlab::Tlab,
};

pub type SemiSpace = Arc<Mutex<Allocator>>;

pub struct Heap {
    thread_id: std::thread::ThreadId,
    thread_tlab: Tlab,

    // we always allocate in teh from_space
    from_space: SemiSpace,
    max_young_space_size: usize,

    to_space: SemiSpace,
    // old generation allocator

    // new generation allocator
    // what do we need?
    //    - we need to somehow keep track of how much is allocated in pages for the young space
    //    - keep track of which pages are to space pages and which pages are from space pages
    //    - ....
}

pub struct Options {
    pub max_young_space_size: usize,
    pub initial_tlab_free_size: usize,
}

impl Heap {
    pub fn new(
        thread_id: ThreadId,
        from_space: SemiSpace,
        to_space: SemiSpace,
        options: Options,
    ) -> Self {
        let thread_tlab = Tlab::new(
            thread_id,
            from_space.clone(),
            options.initial_tlab_free_size,
        );
        Self {
            thread_id,
            thread_tlab,
            from_space,
            to_space,
            max_young_space_size: options.max_young_space_size,
        }
    }

    pub fn try_allocate(&mut self, layout: Layout) -> ErasedPtr {
        if let ptr @ Ptr(Some(_)) = self.thread_tlab.allocate_layout_for_heap(layout) {
            return ptr;
        }
        {
            let mut alloc = self.from_space.lock().unwrap();
            if alloc.bytes_allocated < self.max_young_space_size {
                let new_tlab =
                    self.thread_tlab
                        .get_new_tlab_locked(&mut alloc, layout.size(), layout.align());
                if new_tlab.is_some() {
                    return self.thread_tlab.allocate_layout_for_heap(layout);
                }
            }
            todo!(
                r"
                1. signal for threads that they should synchronize for gc
                2. wait for the threads
            "
            )
        }
        if self.should_perform_gc() {
            self.gc_young_generation();
        } else {
            todo!("wait for gc to end");
        }
        // either we overallocated or we could not allocate a new tlab
        todo!(
            r"
            1. remember to switch semispaces, from is now to.
            2. also we neeed a new tlab for every thread."
        );
        if let ptr @ Ptr(Some(_)) = self.thread_tlab.allocate_layout_for_heap(layout) {
            return ptr;
        }
        // after gc can still not have enough space
        self.allocate_in_old_space(layout)
    }

    fn should_perform_gc(&self) -> bool {
        todo!("check if this is the thread that should perform the gc")
    }

    fn gc_young_generation(&mut self) {
        todo!(
            r"perform the gc for the young generation.
            go through the pages of the from space and copy everything 
            from the from space to the to space"
        )
    }

    pub fn allocate_in_old_space(&mut self, _layout: Layout) -> ErasedPtr {
        todo!("Use old space allocator to allocate the layout")
    }
}
