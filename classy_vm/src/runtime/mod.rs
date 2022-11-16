pub mod class;
pub mod thread;
pub mod thread_manager;
pub mod trace;

use std::{
    alloc::Layout,
    mem::{align_of, size_of},
    sync::Arc,
};

use crate::{
    mem::{ptr::NonNullPtr, ObjectAllocator},
    runtime::class::{
        header::{self, Header},
        Class,
    },
};

#[derive(Clone)]
pub struct Runtime {
    pub classes: Arc<RuntimeClasses>,
}

impl Runtime {
    pub fn init<Heap: ObjectAllocator>(heap: &mut Heap) -> Self {
        Self {
            classes: Arc::new(RuntimeClasses::init_runtime_classes(heap)),
        }
    }
}

pub struct RuntimeClasses {
    pub klass: NonNullPtr<Class>,
    pub string: NonNullPtr<Class>,
    pub byte: NonNullPtr<Class>,
    // pub int: NonNullPtr<class::Int>,
    // pub bool: NonNullPtr<class::Bool>,
}

// classes are immutable after being initialized
// it is a pinky promise as we cannot guarantee it
unsafe impl Send for RuntimeClasses {}
unsafe impl Sync for RuntimeClasses {}

impl RuntimeClasses {
    pub fn init_runtime_classes<Heap: ObjectAllocator>(heap: &mut Heap) -> RuntimeClasses {
        let klass = setup_klass(heap);
        let byte = setup_byte_class(heap, klass);
        let string = setup_string_class(heap, klass, byte);
        fill_in_class_names(heap, klass, byte, string);
        RuntimeClasses {
            klass,
            string,
            byte,
        }
    }
}

fn setup_klass<Heap: ObjectAllocator>(heap: &mut Heap) -> NonNullPtr<Class> {
    unsafe {
        let size = size_of::<Header>() + size_of::<Class>();
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let klass_ptr = heap.try_allocate(layout);
        let Some(klass_ptr) = klass_ptr.inner() else {
            panic!("could not allocate klass inside the permament heap")
        };
        let header_ptr = klass_ptr.as_ptr() as *mut Header;
        let class_ptr = header_ptr.add(1) as *mut Class;
        std::ptr::write(
            header_ptr,
            Header {
                class: NonNullPtr::new_unchecked(class_ptr),
                flags: header::Flags::PermamentHeap as usize,
                data: 0,
            },
        );
        std::ptr::write(class_ptr, class::klass::KLASS_CLASS);
        NonNullPtr::new_unchecked(class_ptr)
    }
}
fn setup_byte_class<Heap: ObjectAllocator>(
    heap: &mut Heap,
    klass: NonNullPtr<Class>,
) -> NonNullPtr<Class> {
    unsafe {
        let size = size_of::<Header>() + size_of::<Class>();
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let allocation = heap.try_allocate(layout);
        assert!(!allocation.is_null());
        let Some(ptr) = allocation.inner() else {
            panic!("cannot allocate a bute class inside the permament heap")
        };
        let header_ptr = ptr.as_ptr() as *mut Header;
        std::ptr::write(
            header_ptr,
            Header {
                class: klass,
                flags: header::Flags::PermamentHeap as usize,
                data: 0,
            },
        );
        let class_ptr = header_ptr.add(1) as *mut Class;
        std::ptr::write(class_ptr, class::byte::BYTE_CLASS);
        NonNullPtr::new_unchecked(class_ptr)
    }
}

pub fn setup_string_class<Heap: ObjectAllocator>(
    heap: &mut Heap,
    klass: NonNullPtr<Class>,
    bytes: NonNullPtr<Class>,
) -> NonNullPtr<Class> {
    unsafe {
        let size = size_of::<Header>() + size_of::<Class>();
        let layout = Layout::from_size_align(size, align_of::<Header>()).unwrap();
        let allocation = heap.try_allocate(layout);
        let Some(ptr) = allocation.inner() else {
            panic!("Could not allocate a string class inside the permament heap")
        };
        let header_ptr = ptr.as_ptr() as *mut Header;
        std::ptr::write(
            header_ptr,
            Header {
                class: klass,
                flags: header::Flags::PermamentHeap as usize,
                data: 0,
            },
        );
        let class_ptr = header_ptr.add(1) as *mut Class;
        std::ptr::write(class_ptr, class::string::make_string_class(bytes));
        NonNullPtr::new_unchecked(class_ptr)
    }
}

fn fill_in_class_names<Heap: ObjectAllocator>(
    heap: &mut Heap,
    klass: NonNullPtr<Class>,
    byte: NonNullPtr<Class>,
    string: NonNullPtr<Class>,
) {
    let klass_name = heap.allocate_static_string(string, "Klass");
    let byte_name = heap.allocate_static_string(string, "Byte");
    let string_name = heap.allocate_static_string(string, "String");
    unsafe {
        (*klass.get()).name = klass_name;
        (*byte.get()).name = byte_name;
        (*string.get()).name = string_name;
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
    use std::{
        mem::{align_of, size_of},
        sync::Arc,
    };

    use crate::{
        mem::{permament_heap::PermamentHeap, ObjectAllocator},
        runtime::{
            class::{self, Class},
            Runtime, RuntimeClasses,
        },
    };

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
        let name = heap.allocate_static_string(runtime.classes.string, "TestClass");
        let class = Class {
            name,
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
