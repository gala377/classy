#![allow(unreachable_code)]

use std::{
    alloc::Layout,
    mem::{align_of, size_of},
    sync::{Arc, Mutex},
    thread::ThreadId,
};

use crate::{
    mem::{
        allocator::Allocator,
        ptr::{ErasedPtr, Ptr},
        tlab::Tlab,
    },
    runtime::{class::{self, header::Header, Class}, thread_manager::{ThreadManager, StopRequetError}},
};

use super::ptr::NonNullPtr;

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

    pub fn try_allocate(&mut self, layout: Layout) -> ErasedPtr {
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
            ShouldPerformGc::ShouldWait => {
                self.thread_manager.stop_for_gc().unwrap()
            },
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

    fn swap_semispaces(&mut self) {
        std::mem::swap(&mut self.from_space, &mut self.to_space);
        self.thread_tlab = Tlab::new(self.thread_id, Arc::clone(&self.from_space), self.options.initial_tlab_free_size);
    }

    fn stop_threads_for_gc(&mut self) -> ShouldPerformGc {
        // we possibly have made multiple requests
        match self.thread_manager.request_stop_for_gc() {
            Err(StopRequetError::NoThreadsToStop) => { panic!("should not happen") }
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

    // Allocate zeroed array of class `array_cls` (class of an array not array's
    // elements) of `array_size` elements.
    pub fn allocate_array(&mut self, array_cls: NonNullPtr<Class>, length: usize) -> ErasedPtr {
        unsafe {
            assert!(
                (*array_cls.get()).kind.is_array(),
                "you can only allocate array with an array class"
            );
            let el_size = (*array_cls.get()).array_element_size();
            let size = size_of::<Header>() + el_size * length;
            let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
            let allocation = self.try_allocate(layout);
            let arr = initialise_array(allocation, array_cls, length, el_size);
            ErasedPtr::new(arr)
        }
    }

    pub fn allocate_class(
        &mut self,
        cls: Class,
        fields: &[class::Field],
        meta_class: NonNullPtr<Class>,
    ) -> Ptr<Class> {
        // there is no padding between header, class and fields as all
        // are sure to be word aligned.
        let size =
            size_of::<Header>() + size_of::<Class>() + fields.len() * size_of::<class::Field>();
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let allocation = self.try_allocate(layout);
        let cls = initialise_class(allocation, cls, fields, meta_class);
        Ptr::new(cls)
    }

    pub fn allocate_instance(&mut self, cls: NonNullPtr<Class>) -> ErasedPtr {
        let size = unsafe {
            assert!(
                (*cls.get()).instance_align <= 8,
                "Class alignment cannot be more than word alignment"
            );
            size_of::<Header>() + (*cls.get()).instance_size
        };
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let allocation = self.try_allocate(layout);
        let obj = initialize_instance(allocation, cls);
        ErasedPtr::new(obj)
    }
}

fn initialise_class(
    allocation: ErasedPtr,
    cls: Class,
    fields: &[class::Field],
    meta_class: NonNullPtr<Class>,
) -> *mut Class {
    let header = Header::for_class(meta_class, fields.len());
    match allocation.inner() {
        None => std::ptr::null_mut(),
        Some(ptr) => unsafe {
            let header_ptr = ptr.as_ptr() as *mut Header;
            header_ptr.write(header);
            let cls_ptr = header_ptr.add(1) as *mut Class;
            cls_ptr.write(cls);
            let mut fields_ptr = cls_ptr.add(1) as *mut class::Field;
            for val in fields.iter().cloned() {
                fields_ptr.write(val);
                fields_ptr = fields_ptr.add(1);
            }
            cls_ptr
        },
    }
}

fn initialize_instance(allocation: ErasedPtr, cls: NonNullPtr<Class>) -> *mut () {
    let header = Header::for_instance(cls);
    match allocation.inner() {
        None => std::ptr::null_mut(),
        Some(ptr) => unsafe {
            let header_ptr = ptr.as_ptr() as *mut Header;
            header_ptr.write(header);
            let obj_ptr = header_ptr.add(1) as *mut ();
            obj_ptr
        },
    }
}

fn initialise_array(
    allocation: ErasedPtr,
    array_cls: NonNullPtr<Class>,
    size: usize,
    el_size: usize,
) -> *mut () {
    let header = Header::for_array(array_cls, size);
    match allocation.inner() {
        None => std::ptr::null_mut(),
        Some(ptr) => unsafe {
            let header_ptr = ptr.as_ptr() as *mut Header;
            header_ptr.write(header);
            let arr_ptr: *mut u8 = header_ptr.add(1).cast();
            std::ptr::write_bytes(arr_ptr, 0, size * el_size);
            arr_ptr.cast()
        },
    }
}