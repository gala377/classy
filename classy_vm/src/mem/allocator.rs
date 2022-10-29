#![allow(unstable_name_collisions)]

use std::{ptr::NonNull, thread::ThreadId};

use sptr::Strict;

use super::{page::Page, ptr::Ptr};

pub struct Allocator {
    pub pages: Ptr<Page>,
}

// SAFETY: We have to be very careful not to access
// contentents of pages that have an owner.
unsafe impl Send for Allocator {}

impl Allocator {
    pub fn new() -> Allocator {
        Allocator { pages: Ptr::null() }
    }

    /// Unsafe because the returned page does not have a set owner.
    /// Because of it trying to access memory of this page might lead to data races.
    /// To get a page that is already associated with some thread use
    /// `allocate_page_for`
    pub unsafe fn allocate_page(&mut self, size: usize, align: usize) -> Ptr<Page> {
        assert!(
            size >= std::mem::size_of::<Page>(),
            "Page needs to be able to store its metadata"
        );
        assert!(
            align % std::mem::align_of::<Page>() == 0,
            "Page needs to be aligned to store its metadata"
        );
        let layout =
            std::alloc::Layout::from_size_align(size, align).expect("expected a valid layout");
        let allocated = std::alloc::alloc(layout);
        let start = NonNull::new(allocated).expect("out of memory");
        // create page now because accessing uninitialized memory is UB
        // so we cannot just do something like (*allocated).size = size
        // as the memory under allocated is not properly initialized.
        // SAFETY: start + size is one allocation so this is safe.
        let page = 
            match self.pages.inner() {
                None => Page::new(start, size, align),
                Some(next) => Page::new_linked(start, size, align, next),
            };

        let page_ptr = start.cast::<Page>();
        // SAFETY: page_ptr is not null, properly aligned and can hold
        // the value of the type Page.
        // This has been checked with assertions earlier.
        std::ptr::write(page_ptr.as_ptr(), page);
        let page_ptr = Ptr::new_non_null(page_ptr);
        self.pages = page_ptr;
        page_ptr
    }

    pub fn allocate_page_for(&mut self, owner: ThreadId, size: usize, align: usize) -> Ptr<Page> {
        let page = unsafe { self.allocate_page(size, align) };
        if let Ptr(Some(ptr)) = page {
            unsafe {
                let mut page_owner = (*ptr.as_ptr()).owner.lock().unwrap();
                *page_owner = Some(owner);
            }
        };
        page
    }

    pub fn release_page(&mut self, owner: ThreadId, page: NonNull<Page>) {
        unsafe {
            let mut current_owner = (*page.as_ptr()).owner.lock().unwrap();
            match *current_owner {
                Some(co) if co == owner => 
                    *current_owner = None,
                Some(other_owner) =>
                    panic!("thread {owner:?} is trying to release a page of an other thread {other_owner:?}"),
                None => panic!("thread {owner:?} releasing unassigned memory page"),
            }
        }
    }

    pub fn get_page(&mut self, owner: ThreadId, size: usize, align: usize) -> Ptr<Page> {
        let Ptr(mut current) = self.pages;
        while let Some(page) = current {
            let mut page_owner = unsafe { (*page.as_ptr()).owner.lock().unwrap() };
            if page_owner.is_none() {
                let end = unsafe { (*page.as_ptr()).end().as_ptr().addr() };
                let free = unsafe { (*page.as_ptr()).free };
                let free = round_up_to_align(free.as_ptr().addr(), align);
                if free + size <= end {
                    *page_owner = Some(owner);
                    return Ptr(Some(page));
                }
            }
            current = unsafe { (*page.as_ptr()).next };
        }
        Ptr(None)
    }
}

const fn round_up_to_align(addr: usize, align: usize) -> usize {
    let addr = match addr.checked_add(align - 1) {
        None => panic!("overflow"),
        Some(v) => v,
    };
    addr & !(align - 1)
}

impl Drop for Allocator {
    fn drop(&mut self) {
        while let Ptr(Some(page)) = self.pages {
            unsafe {
                self.pages = Ptr((*page.as_ptr()).next );
                let size = (*page.as_ptr()).size;
                let align = (*page.as_ptr()).align;
                std::ptr::drop_in_place(page.as_ptr());
                let layout = std::alloc::Layout::from_size_align_unchecked(size, align);
                std::alloc::dealloc(page.cast().as_ptr(), layout);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Allocator;

    // 4kb
    const PAGE_SIZE: usize = 1 << 12;
    const PAGE_ALIGN: usize = 1 << 12;

    #[test]
    fn creating_empty_allocator_does_not_allcate() {
        let _alloc = Allocator::new();
    }

    #[test]
    fn allocating_a_page_does_work() {
        let mut alloc = Allocator::new();
        unsafe { alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN); }
    }

    #[test]
    fn allocating_mutliple_pages_does_work() {
        let mut alloc = Allocator::new();
        unsafe { 
            alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN);
            alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN);
            alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN);
        }
    }
}
