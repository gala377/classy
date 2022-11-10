pub mod allocator;
pub mod bump;
pub mod heap;
pub mod page;
pub mod permament_heap;
pub mod ptr;
pub mod tlab;

use std::{
    alloc::Layout,
    mem::{align_of, size_of},
};

use crate::{
    mem::ptr::{ErasedPtr, NonNullPtr, Ptr},
    runtime::class::{self, header::Header, string::StringInst, Class},
};

pub trait ObjectAllocator {
    fn try_allocate(&mut self, layout: Layout) -> ErasedPtr;

    fn adjust_header(&mut self, _header: &mut Header) {}

    // Allocate zeroed array of class `array_cls` (class of an array not array's
    // elements) of `array_size` elements.
    fn allocate_array(&mut self, array_cls: NonNullPtr<Class>, length: usize) -> ErasedPtr {
        unsafe {
            assert!(
                (*array_cls.get()).kind.is_array(),
                "you can only allocate array with an array class"
            );
            let el_size = (*array_cls.get()).array_element_size();
            let size = size_of::<Header>() + el_size * length;
            let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
            let allocation = self.try_allocate(layout);
            let arr = self.initialise_array(allocation, array_cls, length, el_size);
            ErasedPtr::new(arr)
        }
    }

    fn allocate_class(
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
        let cls = self.initialise_class(allocation, cls, fields, meta_class);
        Ptr::new(cls)
    }

    fn allocate_instance(&mut self, cls: NonNullPtr<Class>) -> ErasedPtr {
        let size = unsafe {
            assert!(
                (*cls.get()).instance_align <= 8,
                "Class alignment cannot be more than word alignment"
            );
            size_of::<Header>() + (*cls.get()).instance_size
        };
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let allocation = self.try_allocate(layout);
        let obj = self.initialize_instance(allocation, cls);
        ErasedPtr::new(obj)
    }

    fn initialise_class(
        &mut self,
        allocation: ErasedPtr,
        cls: Class,
        fields: &[class::Field],
        meta_class: NonNullPtr<Class>,
    ) -> *mut Class {
        let mut header = Header::for_class(meta_class, fields.len());
        self.adjust_header(&mut header);
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

    fn allocate_static_string(&mut self, strcls: NonNullPtr<Class>, val: &str) -> Ptr<StringInst> {
        let buff = self.allocate_array(strcls, val.as_bytes().len());
        unsafe {
            match buff.inner() {
                None => buff.cast(),
                Some(ptr) => {
                    std::ptr::copy_nonoverlapping(
                        val.as_ptr(),
                        ptr.as_ptr() as *mut u8,
                        val.as_bytes().len(),
                    );
                    buff.cast()
                }
            }
        }
    }

    fn initialize_instance(&mut self, allocation: ErasedPtr, cls: NonNullPtr<Class>) -> *mut () {
        let mut header = Header::for_instance(cls);
        self.adjust_header(&mut header);
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
        &mut self,
        allocation: ErasedPtr,
        array_cls: NonNullPtr<Class>,
        size: usize,
        el_size: usize,
    ) -> *mut () {
        let mut header = Header::for_array(array_cls, size);
        self.adjust_header(&mut header);
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
}
