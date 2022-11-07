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
