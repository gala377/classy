pub mod class;
pub mod thread;
pub mod thread_manager;
pub mod trace;

use std::{sync::Arc, alloc::Layout, mem::{size_of, align_of}};

use crate::{mem::{ptr::{NonNullPtr, Ptr}, ObjectAllocator}, runtime::class::header::{Header, self}};

use self::class::Class;

#[derive(Clone)]
pub struct Runtime {
    pub classes: Arc<RuntimeClasses>,
}

pub struct RuntimeClasses {
    pub klass: NonNullPtr<Class>,
    pub string: NonNullPtr<Class>,
    pub byte: NonNullPtr<Class>,
    // pub int: NonNullPtr<class::Int>,
    // pub bool: NonNullPtr<class::Bool>,
}

// classes are immutable after being initialized
unsafe impl Send for RuntimeClasses {}

impl RuntimeClasses {
    pub fn init_runtime_classes<Heap: ObjectAllocator>(heap: &mut Heap) -> RuntimeClasses {
        let klass = setup_klass(heap);
        let byte = setup_byte_class(heap, klass);
        let string = setup_string_class(heap, klass, byte);
        RuntimeClasses { klass, string, byte }
    }
}

fn setup_klass<Heap: ObjectAllocator>(heap: &mut Heap) -> NonNullPtr<Class> {
    unsafe {
        let size = size_of::<Header>() + size_of::<Class>();
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let klass_ptr = heap.try_allocate(layout);
        assert!(!klass_ptr.is_null());
        let klass_ptr = match klass_ptr {
            Ptr(Some(ptr)) => ptr,
            Ptr(None) => unreachable!("checked that it's not null"),
        };
        let header_ptr = klass_ptr.as_ptr() as *mut Header;
        let class_ptr = header_ptr.add(1) as *mut Class;
        std::ptr::write(header_ptr, Header {
            class: NonNullPtr::new_unchecked(class_ptr),
            flags: header::Flags::PermamentHeap as usize,
            data: 0
        });
        std::ptr::write(class_ptr, class::klass::KLASS_CLASS);
        NonNullPtr::new_unchecked(class_ptr)
    }
}
fn setup_byte_class<Heap: ObjectAllocator>(heap: &mut Heap, klass: NonNullPtr<Class>) -> NonNullPtr<Class> {
    unsafe { 
        let size = size_of::<Header>() + size_of::<Class>();
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let allocation = heap.try_allocate(layout);
        assert!(!allocation.is_null());
        let ptr = match allocation {
            Ptr(Some(ptr)) => ptr,
            Ptr(None) => unreachable!("checked that it's not null"),
        };
        let header_ptr = ptr.as_ptr() as *mut Header;
        std::ptr::write(header_ptr, Header {
            class: klass,
            flags: header::Flags::PermamentHeap as usize,
            data: 0,
        });
        let class_ptr = header_ptr.add(1) as *mut Class;
        std::ptr::write(class_ptr, class::byte::BYTE_CLASS);
        NonNullPtr::new_unchecked(class_ptr)
    }
}

pub fn setup_string_class<Heap: ObjectAllocator>(heap: &mut Heap, klass: NonNullPtr<Class>, bytes: NonNullPtr<Class>) -> NonNullPtr<Class> {
    unsafe {
    let size = size_of::<Header>() + size_of::<Class>();
    let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
    let allocation = heap.try_allocate(layout);
    assert!(!allocation.is_null());
    let ptr = match allocation {
        Ptr(Some(ptr)) => ptr,
        Ptr(None) => unreachable!("checked that it's not null"),
    };
    let header_ptr = ptr.as_ptr() as *mut Header;
    std::ptr::write(header_ptr, Header {
        class: klass,
        flags: header::Flags::PermamentHeap as usize,
        data: 0,
    });
    let class_ptr = header_ptr.add(1) as *mut Class;
    std::ptr::write(class_ptr, class::string::make_string_class(bytes));
    NonNullPtr::new_unchecked(class_ptr)
}
}

// we need something like
// impl RuntimeClasses {
//
// pub fn init_runtime_classes(h: &mut Heap) -> RuntimeClasses
//   let klass = h.alloc_class_in_old_space(klass::KLASS_CLASS)
//   let string_class = Class { class: klass, ..string::STRING_CLASS};
//   let string_class_ptr = h.alloc_class_in_old_space(string_class);
//   let klass_name = h.allocate_string_in_old_space("klass");
//   (*klass.as_ptr()).name = klass_name;
//   // init the rest of the classes
//
//   RuntimeClasses { klass, string, ... }
//}

#[cfg(test)]
mod tests {
    use std::{sync::Arc, mem::size_of, mem::align_of};

    use crate::mem::{permament_heap::PermamentHeap, ptr::Ptr, ObjectAllocator};

    use super::{RuntimeClasses, Runtime, class::{Class, drop_instance, instance_trace, self}};

    #[test]
    fn setup_runtime_in_permament_heap() {
        let mut heap = PermamentHeap::new();
        let classes = RuntimeClasses::init_runtime_classes(&mut heap);
        let _runtime = Runtime {
            classes: Arc::new(classes),
        };
    }

    #[test]
    fn use_runtime_to_allocate_a_class() {
        let mut heap = PermamentHeap::new();
        let runtime = Runtime {
            classes: Arc::new(RuntimeClasses::init_runtime_classes(&mut heap)),
        };
        let class = Class {
            name: Ptr::null(),
            drop: None,
            trace: class::instance_trace,
            instance_size: 10 * size_of::<usize>(),
            instance_align: align_of::<usize>(),
            actual_instance_size: None,
            kind: class::Kind::Instance,
        };
        let _allocated = heap.allocate_class(class, &[], runtime.classes.klass);
    }
}