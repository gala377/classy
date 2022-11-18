use crate::mem::ptr::Ptr;

pub struct Handle<T>(pub *mut Ptr<T>);

impl<T> Handle<T> {
    /// Unsafe because the location of the object might change
    /// during gc. User has to ensure that the gc
    /// won't happen between calling this function and dereferencing
    /// the pointer.
    pub unsafe fn as_ptr(&self) -> *mut T {
        unsafe { (*self.0).unwrap() }
    }
}
