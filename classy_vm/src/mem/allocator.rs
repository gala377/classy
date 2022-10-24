use std::ptr::NonNull;

use super::{ ptr::Ptr, page::Page };

pub struct Allocator {
    pub pages: Ptr<Page>,
}

impl Allocator {
    pub fn new() -> Allocator {
        Allocator {
            pages: Ptr::null(),
        }
    }

    pub fn allocate_page(&mut self, size: usize, align: usize) -> Ptr<Page> {
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
        let allocated = unsafe { std::alloc::alloc(layout) };
        let start = NonNull::new(allocated).expect("out of memory");
        // create page now because accessing uninitialized memory is UB
        // so we cannot just do something like (*allocated).size = size
        // as the memory under allocated is not properly initialized.
        let page = match self.pages.inner() {
            None => Page::new(start, size, align),
            Some(next) => Page::new_linked(start, size, align, next),
        };
        
        let page_ptr = start.cast::<Page>();
        // SAFETY: page_ptr is not null, properly aligned and can hold 
        // the value of the type Page.
        // This has been checked with assertions earlier.
        unsafe {
            std::ptr::write(page_ptr.as_ptr(), page);
        }
        let page_ptr = Ptr::new_non_null(page_ptr);
        self.pages = page_ptr;
        page_ptr
    }
}

impl Drop for Allocator {
    fn drop(&mut self) {
        while let Ptr(Some(page)) = self.pages {
            unsafe { 
                self.pages = Ptr((*page.as_ptr()).next);
                let size = (*page.as_ptr()).size;
                let align = (*page.as_ptr()).align;
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
        alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN);
    }


    #[test]
    fn allocating_mutliple_pages_does_work() {
        let mut alloc = Allocator::new();
        alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN);
        alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN);
        alloc.allocate_page(PAGE_SIZE, PAGE_ALIGN);
    }
}