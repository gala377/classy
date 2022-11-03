use crate::mem::ptr::NonNullPtr;

use super::Class;

pub enum Flags {
    Forwards = 1,
    PermamentHeap = 1 << 1,
}

impl Flags {
    pub const fn empty() -> usize {
        0
    }
}

#[repr(align(8))]
pub struct Header {
    pub class: NonNullPtr<Class>,
    pub flags: usize,
    pub data: usize,
}

impl Header {

    pub fn for_class(meta_class: NonNullPtr<Class>, fields_count: usize) -> Self {
        Self {
            class: meta_class,
            flags: Flags::empty(),
            data: fields_count,
        }
    }

    pub fn for_array(class: NonNullPtr<Class>, size: usize) -> Self {
        Self {
            class,
            flags: Flags::empty(),
            data: size,
        }
    }

    pub fn for_instance(class: NonNullPtr<Class>) -> Self {
        Self {
            class,
            flags: Flags::empty(),
            data: 0,
        }
    }
}