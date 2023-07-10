use std::alloc::Layout;

use crate::mem::{
    allocator::Allocator,
    bump::BumpAllocator,
    heap::SemiSpace,
    ptr::{ErasedPtr, Ptr},
};

pub struct Tlab {
    id: std::thread::ThreadId,
    semi_space: SemiSpace,
    local_buffer: BumpAllocator,
}

impl Tlab {
    pub fn new(
        id: std::thread::ThreadId,
        semi_space: SemiSpace,
        initial_tlab_free_size: usize,
    ) -> Self {
        println!("Creating thread {id:?}");
        let tlab_page = {
            println!("Thread {id:?} waiting for its page");
            let mut semi = semi_space.lock().expect("mutex poisoned");
            println!("Thread {id:?} getting its page");
            let Ptr(page) =
                semi.allocator
                    .get_page(id, initial_tlab_free_size, std::mem::align_of::<usize>());
            let page = match page {
                Some(ptr) => {
                    println!(
                        "Thread {id:?} got page {addr:0x} from allocator",
                        addr = ptr.as_ptr() as usize
                    );
                    ptr
                }
                None => {
                    println!("Thread: {id:?} had to allocate a new page");
                    let Ptr(page) = semi.allocator.allocate_page(id);
                    page.expect("could not allocate a page for a thread")
                }
            };
            unsafe { assert_eq!((*page.as_ptr()).owner, Some(id)) };
            println!(
                "Thread {id:?} done got page {addr:0x}",
                addr = page.as_ptr() as usize
            );
            page
        };
        Self {
            id,
            semi_space,

            local_buffer: BumpAllocator::new(tlab_page),
        }
    }

    pub fn get_new_tlab(&mut self, size: usize, align: usize) -> Option<()> {
        let mut semi = self.semi_space.lock().expect("mutex poisoned");
        let new_tlab = {
            let Ptr(page) = semi.allocator.get_page(self.id, size, align);
            match page {
                Some(ptr) => ptr,
                None => {
                    let Ptr(page) = semi.allocator.allocate_page(self.id);
                    page?
                }
            }
        };
        let old_tlab = std::mem::replace(&mut self.local_buffer, BumpAllocator::new(new_tlab));
        semi.allocator.release_page(self.id, old_tlab.into_inner());
        Some(())
    }

    pub fn get_new_tlab_locked(
        &mut self,
        alloc: &mut Allocator,
        size: usize,
        align: usize,
    ) -> Option<()> {
        let new_tlab = {
            let Ptr(page) = alloc.get_page(self.id, size, align);
            match page {
                Some(ptr) => ptr,
                None => {
                    let Ptr(page) = alloc.allocate_page(self.id);
                    page?
                }
            }
        };
        let old_tlab = std::mem::replace(&mut self.local_buffer, BumpAllocator::new(new_tlab));
        alloc.release_page(self.id, old_tlab.into_inner());
        Some(())
    }

    pub fn allocate(&mut self, layout: Layout) -> ErasedPtr {
        debug_assert!(self.local_buffer.page_as_ref().owner.unwrap() == self.id,
            "cannot alloc as tlab's owner is not the same as the current thred: c: {current:?} != tl: {tlab:?}",
            current=self.id, tlab=self.local_buffer.page_as_ref().owner.unwrap());
        self.local_buffer.alloc_layout(layout).erase()
    }

    /// # Safety
    /// 
    /// User has to make sure that the current page is no longer used
    /// by an thread before realeasing it.
    pub unsafe fn release_current_page(&mut self) {
        self.semi_space
            .lock()
            .unwrap()
            .allocator
            .release_page(self.id, self.local_buffer.page_as_ptr());
    }
}
