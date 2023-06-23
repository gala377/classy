#![allow(unreachable_code)]

use std::{
    alloc::Layout,
    ptr::{addr_of, NonNull},
    sync::{Arc, Mutex},
    thread::ThreadId,
};

use crate::{
    mem::{
        allocator::Allocator,
        handle::Handle,
        ptr::{ErasedPtr, NonNullPtr, Ptr},
        tlab::Tlab,
        ObjectAllocator,
    },
    runtime::{
        thread_manager::{StopRequetError, ThreadManager},
        trace::Gc, class::{frame::Frame, Class},
    },
};

#[derive(Clone, Copy)]
pub enum SemiSpaceKind {
    FromSpace,
    ToSpace,
}

pub type SemiSpace = Arc<Mutex<SemiSpaceImpl>>;

pub struct SemiSpaceImpl {
    pub kind: SemiSpaceKind,
    pub allocator: Allocator,
}

impl SemiSpaceImpl {
    pub fn new_from_space(aloc: Allocator) -> SemiSpace {
        Arc::new(Mutex::new(Self {
            kind: SemiSpaceKind::FromSpace,
            allocator: aloc,
        }))
    }

    pub fn new_to_space(aloc: Allocator) -> SemiSpace {
        Arc::new(Mutex::new(Self {
            kind: SemiSpaceKind::ToSpace,
            allocator: aloc,
        }))
    }

    fn toggle_kind(&mut self) {
        use SemiSpaceKind::*;
        self.kind = match self.kind {
            FromSpace => ToSpace,
            ToSpace => FromSpace,
        }
    }

    fn reset_all_pages(&mut self) {
        self.allocator.reset_all_pages()
    }
}

enum ShouldPerformGc {
    ShouldPerform,
    ShouldWait,
}

/// Represents the whole heap of the virtual machine's instance.
/// Acts as a monitor on the heap. It should be cloned and used directly.
/// Do not put behind Mutex or Arc.
///
/// This struct is per thread, meaning that each thread holds its own copy.
/// It is safe to clone and send to another thread, though the thread_id and a tlab
/// have to be recomputed for the new thread.
#[allow(dead_code)]
pub struct Heap {
    thread_id: std::thread::ThreadId,
    thread_tlab: Tlab,
    thread_manager: Arc<ThreadManager>,
    options: Options,

    // Young space.
    // We always allocate in the from_space
    from_space: SemiSpace,
    to_space: SemiSpace,

    // todo: old generation allocator and collection
    // promotion from the young space to the old space.
    /// A list existing handles;
    handles: Arc<Mutex<Option<NonNull<HandleNode>>>>,
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
            Arc::clone(&from_space),
            options.initial_tlab_free_size,
        );
        Self {
            thread_id,
            thread_tlab,
            thread_manager,
            from_space,
            to_space,
            options,
            handles: Arc::new(Mutex::new(None)),
        }
    }

    fn swap_semispaces(&mut self, new_tlab_size: usize, new_tlab_align: usize) {
        assert!(new_tlab_align <= std::mem::align_of::<usize>());
        std::mem::swap(&mut self.from_space, &mut self.to_space);
        self.thread_tlab = Tlab::new(self.thread_id, Arc::clone(&self.from_space), new_tlab_size);
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

    fn scavenge_young_generation(&mut self) {
        let mut roots = Vec::new();
        self.collect_handles_to_roots(&mut roots);
        {
            let mut to_space = self.to_space.lock().unwrap();
            let mut gc = Gc::new(&mut to_space.allocator, self.options.initial_tlab_free_size);
            unsafe { gc.collect(&roots) };
        }
    }

    fn collect_handles_to_roots(&mut self, roots: &mut Vec<*mut ErasedPtr>) {
        let handles = self.handles.lock().unwrap();
        let mut curr = handles.clone();
        while let Some(node) = curr {
            unsafe {
                let ptr_addr = addr_of!((*node.as_ptr()).ptr) as *mut _;
                roots.push(ptr_addr);
                curr = (*node.as_ptr()).next.clone();
            }
        }
    }

    pub fn allocate_in_old_space(&mut self, _layout: Layout) -> ErasedPtr {
        todo!("Use old space allocator to allocate the layout")
    }

    pub fn create_handle<T>(&mut self, to: NonNullPtr<T>) -> Handle<T> {
        let node = Box::into_raw(Box::new(HandleNode {
            ptr: to.erase().into(),
            next: None,
            prev: None,
        }));
        let handle = Handle(unsafe { addr_of!((*node).ptr) as *mut _ });
        let new_node = NonNull::new(node).unwrap();
        let mut head = self.handles.lock().unwrap();
        match *head {
            None => {
                *head = Some(new_node);
            }
            Some(curr_head_ptr) => unsafe {
                (*new_node.as_ptr()).next = Some(curr_head_ptr);
                (*curr_head_ptr.as_ptr()).prev = Some(new_node);
                *head = Some(new_node);
            },
        }
        handle
    }

    pub fn revoke_handle<T>(&mut self, handle: Handle<T>) {
        unsafe {
            let mut head = self.handles.lock().unwrap();
            let mut curr = head.clone();
            while let Some(ptr) = curr {
                if addr_of!((*ptr.as_ptr()).ptr) as *mut _ == handle.0 {
                    let prev = (*ptr.as_ptr()).prev;
                    let next = (*ptr.as_ptr()).next;
                    match prev {
                        None => *head = next,
                        Some(prev_ptr) => (*prev_ptr.as_ptr()).next = next,
                    }
                    if let Some(next_ptr) = next {
                        (*next_ptr.as_ptr()).prev = prev;
                    }
                    drop(Box::from_raw(ptr.as_ptr()));
                    return;
                }
                curr = (*ptr.as_ptr()).next;
            }
        }
    }

    pub fn gc_young_space(&mut self, new_tlab_size: usize, new_tlab_align: usize) {
        match self.stop_threads_for_gc() {
            ShouldPerformGc::ShouldWait => self.thread_manager.stop_for_gc().unwrap(),
            ShouldPerformGc::ShouldPerform => {
                self.thread_manager.wait_for_all_threads_stopped().unwrap();
                self.scavenge_young_generation();
                self.from_space.lock().unwrap().reset_all_pages();
                // todo: might be unnecessary and slow down collection
                // so maybe remove this later? Only needed for the Vm struct
                // but I don't think the vm struct is actually needed.
                self.from_space.lock().unwrap().toggle_kind();
                self.to_space.lock().unwrap().toggle_kind();
                self.thread_manager.release_stopped_threads();
            }
        }
        self.swap_semispaces(new_tlab_size, new_tlab_align);
    }

    pub fn young_space_allocated(&self) -> usize {
        let from_space = self.from_space.lock().unwrap();
        let mut res = 0;
        let mut curr = from_space.allocator.pages.inner();
        while let Some(page) = curr {
            unsafe {
                res += (*page.as_ptr()).allocated();
                curr = (*page.as_ptr()).next
            }
        }
        res
    }

    pub fn run_gc(&mut self, for_class: NonNullPtr<Class>, stack: &mut [NonNullPtr<Frame>]) {
        let (new_tlab_size, new_tlab_align) = unsafe {
            let cls = &*for_class.get();
            cls.size_align()
        };
        
        match self.stop_threads_for_gc() {
            ShouldPerformGc::ShouldWait => self.thread_manager.stop_for_gc().unwrap(),
            ShouldPerformGc::ShouldPerform => {
                self.thread_manager.wait_for_all_threads_stopped().unwrap();
                self.scavenge_young_generation_with_stack(stack);
                self.from_space.lock().unwrap().reset_all_pages();
                self.from_space.lock().unwrap().toggle_kind();
                self.to_space.lock().unwrap().toggle_kind();
                self.thread_manager.release_stopped_threads();
            }
        }
        self.swap_semispaces(new_tlab_size, new_tlab_align);
    }


    fn scavenge_young_generation_with_stack(&mut self, stack: &mut [NonNullPtr<Frame>]) {
        let mut roots = Vec::new();
        self.collect_roots(&mut roots, stack);
        {
            let mut to_space = self.to_space.lock().unwrap();
            let mut gc = Gc::new(&mut to_space.allocator, self.options.initial_tlab_free_size);
            unsafe { gc.collect(&roots) };
        }
    }

    fn collect_roots(&mut self, roots: &mut Vec<*mut ErasedPtr>, curr_thread_stack: &mut [NonNullPtr<Frame>]) {
        let mut gc_thread_data = self.thread_manager.get_gc_data();
        gc_thread_data.remove(&std::thread::current().id());
        self.collect_handles_to_roots(roots);
        for frame in curr_thread_stack.iter_mut() {
            unsafe {
                let frame = &*frame.get();
                frame.get_references(roots);
            }
        }
        for (_, stack) in gc_thread_data {
            for frame in stack.iter() {
                unsafe {
                    let frame = &*frame.get();
                    frame.get_references(roots);
                }
            }
        }
    }
}

impl ObjectAllocator for Heap {
    fn try_allocate(&mut self, layout: Layout) -> ErasedPtr {
        if let ptr @ Ptr(Some(_)) = self.thread_tlab.allocate(layout) {
            return ptr;
        }
        {
            let mut semi_space = self.from_space.lock().unwrap();
            // todo: check if we should wait for gc here before we try to allocate
            // so that we don't need make concurrent calls to the stop_threads_for gc
            let new_tlab = self.thread_tlab.get_new_tlab_locked(
                &mut semi_space.allocator,
                layout.size(),
                layout.align(),
            );
            if new_tlab.is_some() {
                return self.thread_tlab.allocate(layout);
            }
            // todo: for the check above to make sense we need the request threads stop
            // while holding the lock for the allocator.
        };
        Ptr::null()
        // TODO:
        // Old code for when the old space was planned
        // // either we overallocated or we could not allocate a new tlab
        // self.gc_young_space(layout.size(), layout.align());
        // if let ptr @ Ptr(Some(_)) = self.thread_tlab.allocate(layout) {
        //     return ptr;
        // }
        // // after gc can still not have enough space
        // self.allocate_in_old_space(layout)
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            // we need to release the page
            self.thread_tlab.release_current_page();
            // drop handles
            let mut head = self.handles.lock().unwrap();
            let mut curr = head.take();
            while let Some(ptr) = curr {
                curr = (*ptr.as_ptr()).next;
                drop(Box::from_raw(ptr.as_ptr()));
            }
        }
    }
}

struct HandleNode {
    ptr: ErasedPtr,
    prev: Option<NonNull<HandleNode>>,
    next: Option<NonNull<HandleNode>>,
}
