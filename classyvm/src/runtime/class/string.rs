use std::mem::align_of;

use crate::{
    mem::ptr::{NonNullPtr, Ptr},
    runtime::class::header::Header,
};

use super::{array, Class, Kind};

pub fn make_string_class() -> Class {
    Class {
        name: Ptr::null(),
        instance_size: 0,
        instance_align: align_of::<u8>(),
        drop: None,
        kind: Kind::Array {
            element_size: 1,
            element_align: 1,
            is_ref: false,
        },
        actual_instance_size: Some(array::actual_size),
        trace: array::trace,
    }
}

pub struct StringInst;

impl std::fmt::Debug for StringInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let contents = self.as_rust_str();
        f.debug_tuple("String").field(&contents).finish()
    }
}

impl StringInst {
    pub fn as_rust_string(&self) -> String {
        // SAFETY:
        // We cast reference to a *mut Self which would be illegal but we
        // never derefernce it as mutable, only as immutable so this is fine.
        unsafe {
            as_rust_string({
                NonNullPtr::new_unchecked(self as *const StringInst as *mut StringInst)
            })
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

/// # Safety
///
/// Expects that the pointer is valid and points to the correct instance.
pub unsafe fn as_rust_str(ptr: &NonNullPtr<StringInst>) -> &str {
    let len = Header::data_from_nonnull(*ptr);
    let as_slice = std::slice::from_raw_parts(ptr.get() as *const u8, len);
    std::str::from_utf8(as_slice).unwrap()
}

/// # Safety
///
/// Expects that the pointer is valid and points to the correct instance.
pub unsafe fn as_rust_string(ptr: NonNullPtr<StringInst>) -> String {
    as_rust_str(&ptr).to_owned()
}
