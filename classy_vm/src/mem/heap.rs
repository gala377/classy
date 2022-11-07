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
        tlab::Tlab,
        ObjectAllocator,
    },
    runtime::thread_manager::{StopRequetError, ThreadManager},
};

pub type SemiSpace = Arc<Mutex<Allocator>>;

enum ShouldPerformGc {
    ShouldPerform,
    ShouldWait,
}

#[allow(dead_code)]
pub struct Heap {
    thread_id: std::thread::ThreadId,
    thread_tlab: Tlab,
    thread_manager: Arc<ThreadManager>,

    // we always allocate in the from_space
    from_space: SemiSpace,

    to_space: SemiSpace,
    // old generation allocator

    // new generation allocator
    // what do we need?
    //    - we need to somehow keep track of how much is allocated in pages for the young space
    //    - keep track of which pages are to space pages and which pages are from space pages
    //    - ....
    options: Options,
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
        thread_manager: Arc<ThreadManager>,
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
            thread_manager,
            from_space,
            to_space,
            options,
        }
    }

    fn swap_semispaces(&mut self) {
        std::mem::swap(&mut self.from_space, &mut self.to_space);
        self.thread_tlab = Tlab::new(
            self.thread_id,
            Arc::clone(&self.from_space),
            self.options.initial_tlab_free_size,
        );
    }

    fn stop_threads_for_gc(&mut self) -> ShouldPerformGc {
        // we possibly have made multiple requests
        match self.thread_manager.request_stop_for_gc() {
            Err(StopRequetError::NoThreadsToStop) => {
                panic!("should not happen")
            }
            Err(StopRequetError::AlreadyRequested) => ShouldPerformGc::ShouldWait,
            Ok(_) => ShouldPerformGc::ShouldPerform,
        }
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

impl ObjectAllocator for Heap {
    fn try_allocate(&mut self, layout: Layout) -> ErasedPtr {
        if let ptr @ Ptr(Some(_)) = self.thread_tlab.allocate(layout) {
            return ptr;
        }
        {
            let mut alloc = self.from_space.lock().unwrap();
            // todo: check if we should wait for gc here before we try to allocate
            // so that we don't need make concurrent calls to the stop_threads_for gc
            if alloc.bytes_allocated < self.options.max_young_space_size {
                let new_tlab =
                    self.thread_tlab
                        .get_new_tlab_locked(&mut alloc, layout.size(), layout.align());
                if new_tlab.is_some() {
                    return self.thread_tlab.allocate(layout);
                }
            }
            // todo: for the check above to make sense we need the request threads stop
            // while holding the lock for the allocator.
        };
        // either we overallocated or we could not allocate a new tlab
        match self.stop_threads_for_gc() {
            ShouldPerformGc::ShouldWait => self.thread_manager.stop_for_gc().unwrap(),
            ShouldPerformGc::ShouldPerform => {
                self.thread_manager.wait_for_all_threads_stopped().unwrap();
                self.gc_young_generation();
                self.thread_manager.release_stopped_threads();
            }
        }
        self.swap_semispaces();
        if let ptr @ Ptr(Some(_)) = self.thread_tlab.allocate(layout) {
            return ptr;
        }
        // after gc can still not have enough space
        self.allocate_in_old_space(layout)
    }
}
