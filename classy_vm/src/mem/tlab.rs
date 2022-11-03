
use std::sync::{Arc, Mutex};

use crate::mem::{self, bump::BumpAllocator, ptr::Ptr};

pub struct Tlab {
    id: std::thread::ThreadId,
    allocator: Arc<Mutex<mem::allocator::Allocator>>,
    local_buffer: BumpAllocator,
}

impl Tlab {
    pub fn new(
        id: std::thread::ThreadId,
        allocator: Arc<Mutex<mem::allocator::Allocator>>,
        initial_tlab_free_size: usize,
    ) -> Self {
        println!("Creating thread {id:?}");
        let tlab_page = {
            println!("Thrad {id:?} waiting for its page");
            let mut alloc = allocator.lock().expect("mutex poisoned");
            println!("Thread {id:?} getting its page");
            // todo: get_page should not depend on the information that the page metadata
            // is stored inside the page itself.
            let Ptr(page) = alloc.get_page(id, initial_tlab_free_size, std::mem::align_of::<usize>());
            let page = match page {
                Some(ptr) => {
                    println!("Thread {id:?} got page {addr:0x} from allocator", addr=ptr.as_ptr() as usize);
                    ptr
                }
                None => {
                    println!("Thread: {id:?} had to allocate a new page");
                    let Ptr(page) = alloc.allocate_page_for(id);
                    page.expect("could not allocate a page for a thread")
                }
            };
            println!("Thread {id:?} done got page {addr:0x}", addr=page.as_ptr() as usize);
            page
        }; 
        Self {
            id,
            allocator,

            local_buffer: BumpAllocator::new(tlab_page),
        }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        debug_assert!(self.local_buffer.page_as_ref().owner.lock().unwrap().unwrap() == self.id,
            "cannot alloc as tlab's owner is not the same as the current thred: c: {current:?} != tl: {tlab:?}",
            current=self.id, tlab=self.local_buffer.page_as_ref().owner.lock().unwrap().unwrap());
        let ptr = self.local_buffer.alloc::<T>();
        if let p @ Ptr(Some(_)) = ptr {
            return p;
        }
        match self.get_new_tlab(std::mem::size_of::<T>(), std::mem::align_of::<T>()) {
            None => Ptr::null(),
            Some(()) => self.local_buffer.alloc::<T>(),
        }
    }

    fn get_new_tlab(&mut self, size: usize, align: usize) -> Option<()> {
        let mut alloc = self.allocator.lock().expect("mutex poisoned");
        let new_tlab = {
            let Ptr(page) = alloc.get_page(self.id, size, align);
            match page {
                Some(ptr) => ptr,
                None => {
                    let Ptr(page) = alloc.allocate_page_for(self.id);
                    page?
                }
            }
        };
        let old_tlab = std::mem::replace(&mut self.local_buffer, BumpAllocator::new(new_tlab));
        alloc.release_page(self.id, old_tlab.into_inner());
        Some(())
    }

}
