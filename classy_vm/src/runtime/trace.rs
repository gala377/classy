use crate::mem::ptr::{ErasedPtr, ErasedNonNull};



pub trait Tracer {

    fn trace_pointer(&mut self, ptr: ErasedPtr) -> *mut () {
        if ptr.is_null() {
            std::ptr::null_mut()
        } else {
            self.trace_nonnull_pointer(ErasedNonNull::from_ptr(ptr))
        }
    }

    fn trace_nonnull_pointer(&mut self, ptr: ErasedNonNull) -> *mut ();
}