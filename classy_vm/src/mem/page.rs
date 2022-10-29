use std::{ptr::NonNull, sync::Mutex};

pub struct Page {
    // Address space
    pub start: NonNull<u8>,
    pub free: NonNull<u8>,
    pub size: usize,
    pub align: usize,

    // Thread data
    pub owner: Mutex<Option<std::thread::ThreadId>>,

    // Intrusive list
    pub next: Option<std::ptr::NonNull<Page>>,
}

impl Page {
    pub unsafe fn new(start: NonNull<u8>, size: usize, align: usize) -> Self {
        Page {
            start,
            size,
            align,
            free: page_free_start(start, size),
            owner: Mutex::new(None),
            next: None,
        }
    }

    pub unsafe fn new_linked(start: NonNull<u8>, size: usize, align: usize, next: NonNull<Page>) -> Self {
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
        self.end().as_ptr() as usize - self.free.as_ptr() as usize
    }
}

unsafe fn page_free_start(start: NonNull<u8>, size: usize) -> NonNull<u8> {
    debug_assert!(
        size >= std::mem::size_of::<Page>(),
        "the page is not big enough to hold its header"
    );
    NonNull::new_unchecked(start.as_ptr().add(std::mem::size_of::<Page>()))
}
