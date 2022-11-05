use std::{
    alloc::Layout,
    mem::{align_of, size_of},
    sync::{Arc, Mutex},
};

use crate::{
    mem::{
        self,
        allocator::Allocator,
        bump::BumpAllocator,
        ptr::{ErasedPtr, NonNullPtr, Ptr},
    },
    runtime::class::{self, header::Header, Class},
};

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
            let Ptr(page) =
                alloc.get_page(id, initial_tlab_free_size, std::mem::align_of::<usize>());
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
                    let Ptr(page) = alloc.allocate_page_for(id);
                    page.expect("could not allocate a page for a thread")
                }
            };
            println!(
                "Thread {id:?} done got page {addr:0x}",
                addr = page.as_ptr() as usize
            );
            page
        };
        Self {
            id,
            allocator,

            local_buffer: BumpAllocator::new(tlab_page),
        }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        debug_assert!(self.local_buffer.page_as_ref().owner.unwrap() == self.id,
            "cannot alloc as tlab's owner is not the same as the current thred: c: {current:?} != tl: {tlab:?}",
            current=self.id, tlab=self.local_buffer.page_as_ref().owner.unwrap());
        let ptr = self.local_buffer.alloc::<T>();
        if let p @ Ptr(Some(_)) = ptr {
            return p;
        }
        match self.get_new_tlab(std::mem::size_of::<T>(), std::mem::align_of::<T>()) {
            None => Ptr::null(),
            Some(()) => self.local_buffer.alloc::<T>(),
        }
    }

    pub fn get_new_tlab(&mut self, size: usize, align: usize) -> Option<()> {
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
                    let Ptr(page) = alloc.allocate_page_for(self.id);
                    page?
                }
            }
        };
        let old_tlab = std::mem::replace(&mut self.local_buffer, BumpAllocator::new(new_tlab));
        alloc.release_page(self.id, old_tlab.into_inner());
        Some(())
    }

    fn allocate_layout(&mut self, layout: Layout) -> ErasedPtr {
        debug_assert!(self.local_buffer.page_as_ref().owner.unwrap() == self.id,
            "cannot alloc as tlab's owner is not the same as the current thred: c: {current:?} != tl: {tlab:?}",
            current=self.id, tlab=self.local_buffer.page_as_ref().owner.unwrap());
        let ptr = self.local_buffer.alloc_layout(layout);
        if let p @ Ptr(Some(_)) = ptr {
            return p.erase();
        }
        match self.get_new_tlab(layout.size(), layout.align()) {
            // todo: at this point the caller has to perform a gc because
            // there are no free pages left.
            None => Ptr::null(),
            Some(()) => self.local_buffer.alloc_layout(layout).erase(),
        }
    }

    pub fn allocate_layout_for_heap(&mut self, layout: Layout) -> ErasedPtr {
        debug_assert!(self.local_buffer.page_as_ref().owner.unwrap() == self.id,
            "cannot alloc as tlab's owner is not the same as the current thred: c: {current:?} != tl: {tlab:?}",
            current=self.id, tlab=self.local_buffer.page_as_ref().owner.unwrap());
        self.local_buffer.alloc_layout(layout).erase()
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
            let allocation = self.allocate_layout(layout);
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
        let allocation = self.allocate_layout(layout);
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
        let allocation = self.allocate_layout(layout);
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
