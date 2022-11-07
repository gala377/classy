pub mod class;
pub mod thread;
pub mod thread_manager;
pub mod trace;

use std::sync::Arc;

#[derive(Clone)]
pub struct Runtime {
    pub classes: Arc<RuntimeClasses>,
}

unsafe impl Send for Runtime {}

pub struct RuntimeClasses {
    // pub klass: NonNull<class::Klass>,
    // pub string: NonNull<class::String>,
    // pub int: NonNull<class::Int>,
    // pub bool: NonNull<class::Bool>,
}
