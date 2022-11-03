pub mod class;
pub mod trace;
pub mod thread;
pub mod tlab;

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