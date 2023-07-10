pub mod class;
pub mod linker;
pub mod thread;
pub mod thread_manager;
pub mod trace;

use std::{
    alloc::Layout,
    collections::HashMap,
    mem::{align_of, size_of},
    sync::Arc,
};

use crate::{
    mem::{
        ptr::{NonNullPtr, Ptr},
        ObjectAllocator,
    },
    runtime::class::{
        header::{self, Header},
        Class,
    },
};

#[derive(Clone)]
pub struct Runtime {
    pub classes: Arc<RuntimeClasses>,
    pub user_classes: Arc<UserClasses>,
}

impl Runtime {
    pub fn init<Heap: ObjectAllocator>(heap: &mut Heap, user_classes: UserClasses) -> Self {
        Self {
            classes: Arc::new(RuntimeClasses::init_runtime_classes(heap)),
            user_classes: Arc::new(user_classes),
        }
    }
}

pub struct RuntimeClasses {
    pub klass: NonNullPtr<Class>,
    pub string: NonNullPtr<Class>,
    pub byte: NonNullPtr<Class>,
    pub int: NonNullPtr<Class>,
    pub frame: NonNullPtr<Class>,
    pub code: NonNullPtr<Class>,
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
        let string = setup_string_class(heap, klass);
        fill_in_class_names(heap, klass, byte, string);
        let int = setup_class(
            heap,
            klass,
            string,
            "Int",
            &class::integer::INTEGER_CLASS,
            &[],
        );
        let frame = setup_class(
            heap,
            klass,
            string,
            "Frame",
            &class::frame::make_frame_class(),
            &[],
        );
        let code = setup_class(
            heap,
            klass,
            string,
            "Code",
            &class::code::make_code_class(),
            &[],
        );
        RuntimeClasses {
            klass,
            string,
            byte,
            int,
            frame,
            code,
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
            panic!("cannot allocate a byte class inside the permament heap")
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
        std::ptr::write(class_ptr, class::string::make_string_class());
        NonNullPtr::new_unchecked(class_ptr)
    }
}

fn setup_class<Heap: ObjectAllocator>(
    heap: &mut Heap,
    klass: NonNullPtr<Class>,
    strcls: NonNullPtr<Class>,
    name: &str,
    class: &Class,
    fields: &[class::Field],
) -> NonNullPtr<Class> {
    let class_name = heap.allocate_static_string(strcls, name);
    let class = Class {
        name: class_name,
        ..*class
    };
    let Ptr(Some(cls_ptr)) = heap.allocate_class(class, fields, klass) else {
        panic!("could not allocate {name} class in the permament heap");
    };
    NonNullPtr::new(cls_ptr)
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
#[derive(Default)]
pub struct UserClasses {
    // mapping from symbol address to class
    classes: HashMap<usize, NonNullPtr<Class>>,
}

// classes are immutable after being initialized
// it is a pinky promise as we cannot guarantee it
unsafe impl Sync for UserClasses {}
unsafe impl Send for UserClasses {}

impl UserClasses {
    pub fn new() -> Self {
        Self {
            classes: HashMap::new(),
        }
    }

    pub fn get_class_ptr(&self, ptr: usize) -> NonNullPtr<Class> {
        self.classes.get(&ptr).cloned().unwrap()
    }

    pub fn add_class(&mut self, key: usize, ptr: NonNullPtr<Class>) {
        self.classes.insert(key, ptr);
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<usize, NonNullPtr<Class>> {
        self.classes.iter()
    }
}

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

    use super::UserClasses;

    #[test]
    fn setup_runtime_in_permament_heap() {
        let mut heap = PermamentHeap::new();
        let classes = RuntimeClasses::init_runtime_classes(&mut heap);
        let _runtime = Runtime {
            classes: Arc::new(classes),
            user_classes: Arc::new(UserClasses::new()),
        };
    }

    #[test]
    fn use_runtime_to_allocate_a_class() {
        let mut heap = PermamentHeap::new();
        let runtime = Runtime {
            classes: Arc::new(RuntimeClasses::init_runtime_classes(&mut heap)),
            user_classes: Arc::new(UserClasses::new()),
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
