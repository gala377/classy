pub mod class;
pub mod thread;
pub mod thread_manager;
pub mod trace;

use std::sync::Arc;

use crate::mem::ptr::NonNullPtr;

use self::class::Class;

#[derive(Clone)]
pub struct Runtime {
    pub classes: Arc<RuntimeClasses>,
}

unsafe impl Send for Runtime {}

pub struct RuntimeClasses {
    pub klass: NonNullPtr<Class>,
    pub string: NonNullPtr<Class>,
    // pub int: NonNullPtr<class::Int>,
    // pub bool: NonNullPtr<class::Bool>,
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