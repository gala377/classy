#![allow(unstable_name_collisions)]

use std::ptr::NonNull;

use sptr::Strict;

pub struct Page {
    // Address space
    pub start: NonNull<u8>,
    pub free: NonNull<u8>,
    pub size: usize,
    pub align: usize,

    // Thread data
    pub owner: Option<std::thread::ThreadId>,

    // Intrusive list
    pub next: Option<std::ptr::NonNull<Page>>,
}

impl Page {
    /// # Safety
    ///
    /// size and align have to correctly describe allocated memory
    /// starting at start
    pub unsafe fn new(start: NonNull<u8>, size: usize, align: usize) -> Self {
        Page {
            start,
            size,
            align,
            free: page_free_start(start, size),
            owner: None,
            next: None,
        }
    }

    /// # Safety
    ///
    /// `start` has to be a valid pointer to a page.
    /// Same as `next`/
    pub unsafe fn new_linked(
        start: NonNull<u8>,
        size: usize,
        align: usize,
        next: NonNull<Page>,
    ) -> Self {
        Page {
            next: Some(next),
            ..Page::new(start, size, align)
        }
    }

    pub fn end(&self) -> NonNull<u8> {
        // SAFETY: start + size will always be within the same allocation
        unsafe { NonNull::new_unchecked(self.start.as_ptr().add(self.size)) }
    }
    pub fn free_space(&self) -> usize {
        self.end().as_ptr().addr() - self.free.as_ptr().addr()
    }

    pub fn allocated(&self) -> usize {
        self.free.as_ptr().addr() - (self.start.as_ptr().addr() + std::mem::size_of::<Page>())
    }

    pub fn reset_page(&mut self) {
        self.owner = None;
        self.free = unsafe { page_free_start(self.start, self.size) };
    }
}

unsafe fn page_free_start(start: NonNull<u8>, size: usize) -> NonNull<u8> {
    debug_assert!(
        size >= std::mem::size_of::<Page>(),
        "the page is not big enough to hold its header"
    );
    NonNull::new_unchecked(start.as_ptr().add(std::mem::size_of::<Page>()))
}
