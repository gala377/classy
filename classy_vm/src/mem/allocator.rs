#![allow(unstable_name_collisions)]

use std::{ptr::NonNull, thread::ThreadId};

use sptr::Strict;

use crate::mem::{page::Page, ptr::Ptr};

pub struct Allocator {
    pub pages: Ptr<Page>,

    pub page_size: usize,
    pub page_align: usize,

    pub bytes_allocated: usize,
    pub allocated_limit: usize,
}

// SAFETY: We have to be very careful not to access
// contents of a pages that already has an owner.
unsafe impl Send for Allocator {}

impl Allocator {
    pub fn new(page_size: usize, page_align: usize, allocated_limit: usize) -> Allocator {
        Allocator {
            pages: Ptr::null(),
            page_size,
            page_align,
            bytes_allocated: 0,
            allocated_limit,
        }
    }

    /// # Safety
    ///
    /// Unsafe because the returned page does not have a set owner.
    /// Because of it trying to access memory of this page might lead to data
    /// races. To get a page that is already associated with some thread use
    /// `allocate_page_for`
    pub unsafe fn allocate_custom_page_unowned(&mut self, size: usize, align: usize) -> Ptr<Page> {
        if self.bytes_allocated + size > self.allocated_limit {
            return Ptr::null();
        }
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
        let page = match self.pages.inner() {
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
        self.bytes_allocated += size;
        page_ptr
    }

    /// Same as `allocate_custom_page` however immediately sets the owner of the
    /// page. This way the page cannot be given to other thread, which migh
    /// have requested for it with the `get_page` method.
    pub fn allocate_custom_page(
        &mut self,
        owner: ThreadId,
        size: usize,
        align: usize,
    ) -> Ptr<Page> {
        let page = unsafe { self.allocate_custom_page_unowned(size, align) };
        if let Ptr(Some(ptr)) = page {
            unsafe { (*ptr.as_ptr()).owner = Some(owner) }
        };
        page
    }

    /// Same as `allocate_custom_page` but uses allocator's settings.
    ///
    /// # Safety
    ///
    /// Allocates page unowned which means that any thread can take it in
    /// the meantime (I think).
    pub unsafe fn allocate_page_unowned(&mut self) -> Ptr<Page> {
        self.allocate_custom_page_unowned(self.page_size, self.page_align)
    }

    /// Same as `allocate_custom_page_for` but uses allocator's settings.
    pub fn allocate_page(&mut self, owner: ThreadId) -> Ptr<Page> {
        self.allocate_custom_page(owner, self.page_size, self.page_align)
    }

    /// Releases the page from the current owner. The page has to be owned by
    /// the `owner` thread.
    pub fn release_page(&mut self, owner: ThreadId, page: NonNull<Page>) {
        unsafe {
            let current_owner = (*page.as_ptr()).owner;
            match current_owner {
                Some(co) if co == owner => (*page.as_ptr()).owner = None,
                Some(other_owner) => panic!(
                    "thread {owner:?} is trying to release a page of an other thread \
                     {other_owner:?}"
                ),
                None => panic!("thread {owner:?} releasing unassigned memory page"),
            }
        }
    }

    /// Returns a pointer to the page that can be used to allocate at least
    /// `size` amount of bytes with aligned to `align`.
    /// The page will be owned by the requesting `owner` thread.
    pub fn get_page(&mut self, owner: ThreadId, size: usize, align: usize) -> Ptr<Page> {
        let page = self.get_page_unowned(size, align);
        page.with_ptr(|ptr| unsafe { (*ptr.as_ptr()).owner = Some(owner) })
    }

    pub fn get_page_unowned(&mut self, size: usize, align: usize) -> Ptr<Page> {
        let Ptr(mut current) = self.pages;
        while let Some(page) = current {
            let page_owner = unsafe { (*page.as_ptr()).owner };
            if page_owner.is_none() {
                let end = unsafe { (*page.as_ptr()).end().as_ptr().addr() };
                let free = unsafe { (*page.as_ptr()).free };
                let free = round_up_to_align(free.as_ptr().addr(), align);
                if free + size <= end {
                    return Ptr(Some(page));
                }
            }
            current = unsafe { (*page.as_ptr()).next };
        }
        Ptr(None)
    }

    pub fn reset_all_pages(&mut self) {
        let mut curr = self.pages.inner();
        while let Some(inner) = curr {
            unsafe {
                (*inner.as_ptr()).reset_page();
                curr = (*inner.as_ptr()).next;
            }
        }
    }
}

const fn round_up_to_align(addr: usize, align: usize) -> usize {
    let Some(addr) = addr.checked_add(align - 1) else {
        panic!("overflow")
    };
    addr & !(align - 1)
}

impl Drop for Allocator {
    fn drop(&mut self) {
        while let Ptr(Some(page)) = self.pages {
            unsafe {
                self.pages = Ptr((*page.as_ptr()).next);
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
    const ALLOCATION_LIMIT: usize = 1 << 32;

    #[test]
    fn creating_empty_allocator_does_not_allcate() {
        let _alloc = Allocator::new(PAGE_SIZE, PAGE_ALIGN, ALLOCATION_LIMIT);
    }

    #[test]
    fn allocating_a_page_does_work() {
        let mut alloc = Allocator::new(PAGE_SIZE, PAGE_ALIGN, ALLOCATION_LIMIT);
        unsafe {
            alloc.allocate_page_unowned();
        }
    }

    #[test]
    fn allocating_mutliple_pages_does_work() {
        let mut alloc = Allocator::new(PAGE_SIZE, PAGE_ALIGN, ALLOCATION_LIMIT);
        unsafe {
            alloc.allocate_page_unowned();
            alloc.allocate_page_unowned();
            alloc.allocate_page_unowned();
        }
    }
}
