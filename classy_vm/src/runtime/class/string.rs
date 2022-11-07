use crate::{mem::ptr::NonNullPtr, runtime::class::header::Header};


#[derive(Debug)]
pub struct StringInst;

impl StringInst {
    pub fn as_rust_string(&self) -> String {
        // SAFETY:
        // We cast reference to a *mut Self which would be illegal but we
        // never derefernce it as mutable, only as immutable so this is fine.
        unsafe {
            as_rust_string({ NonNullPtr::new_unchecked(
                self as *const StringInst as *mut StringInst,
            )})
        }
    }
    pub fn as_rust_str(&self) -> &str {
        let ptr = unsafe { NonNullPtr::new_unchecked(self as *const Self as *mut Self) };
        // SAFETY:
        // Assumption is that we are pointing to the valid data.
        // That is not really true but is true enough.
        unsafe {
            let len = Header::data_from_nonnull(ptr);
            let as_slice = std::slice::from_raw_parts(ptr.get() as *const u8, len);
            std::str::from_utf8(as_slice).unwrap()
        }
    }
}
unsafe fn as_rust_str(ptr: &NonNullPtr<StringInst>) -> &str {
    let len = Header::data_from_nonnull(*ptr);
    let as_slice = std::slice::from_raw_parts(ptr.get() as *const u8, len);
    std::str::from_utf8(as_slice).unwrap()
}

unsafe fn as_rust_string(ptr: NonNullPtr<StringInst>) -> String {
    as_rust_str(&ptr).to_owned()
}