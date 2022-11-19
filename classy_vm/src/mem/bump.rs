#![allow(unstable_name_collisions)]
use sptr::Strict;

use std::{alloc::Layout, ptr::NonNull};

use crate::mem::{page::Page, ptr::Ptr};

pub struct BumpAllocator {
    alloc_page: NonNull<Page>,
}

impl BumpAllocator {
    pub fn new(alloc_page: NonNull<Page>) -> Self {
        println!(
            "New bump allocator with page address {:0x}",
            alloc_page.as_ptr() as usize
        );
        BumpAllocator { alloc_page }
    }

    fn free(&self) -> NonNull<u8> {
        unsafe { (*self.alloc_page.as_ptr()).free }
    }

    fn start(&self) -> NonNull<u8> {
        unsafe { (*self.alloc_page.as_ptr()).start }
    }

    fn end(&self) -> NonNull<u8> {
        unsafe { (*self.alloc_page.as_ptr()).end() }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        unsafe { self.alloc_layout(Layout::new::<T>()).cast() }
    }

    pub fn alloc_layout(&mut self, layout: Layout) -> Ptr<u8> {
        let free = self.free();
        let end = self.end();
        let align = layout.align();
        let size = layout.size();
        unsafe {
            let ptr = free.as_ptr().addr();
            let Some(ptr) = ptr.checked_add(align - 1) else {
                return Ptr::null()
            };
            let aligned = ptr & !(align - 1);
            let Some(ptr) = aligned.checked_add(size) else {
                return Ptr::null()
            };
            if ptr > end.as_ptr().addr() {
                return Ptr::null();
            }
            self.set_free(self.add_provenance(ptr));
            Ptr::new_non_null(NonNull::new_unchecked(self.add_provenance(aligned)))
        }
    }

    fn add_provenance(&self, addr: usize) -> *mut u8 {
        self.start().as_ptr().with_addr(addr)
    }

    unsafe fn set_free(&mut self, ptr: *mut u8) {
        self.assert_ptr_within_page(ptr.addr());
        (*self.alloc_page.as_ptr()).free = NonNull::new_unchecked(ptr);
    }

    fn assert_ptr_within_page(&self, ptr: usize) {
        let end = self.end().as_ptr() as usize;
        let start = self.start().as_ptr() as usize;
        assert!(
            start <= ptr && ptr <= end,
            "the pointer is not a part of the page"
        );
    }

    pub fn into_inner(self) -> NonNull<Page> {
        self.alloc_page
    }

    pub fn page_as_ref(&self) -> &Page {
        unsafe { &*self.alloc_page.as_ptr() }
    }

    pub fn page_as_ptr(&self) -> NonNull<Page> {
        self.alloc_page
    }
}
