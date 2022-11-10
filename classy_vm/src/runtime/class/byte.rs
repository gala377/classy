use std::mem::{align_of, size_of};

use crate::mem::ptr::Ptr;

use super::{trace_none, Class, Kind};

pub const BYTE_CLASS: Class = Class {
    name: Ptr::null(),
    drop: None,
    trace: trace_none,
    instance_size: size_of::<u8>(),
    instance_align: align_of::<u8>(),
    actual_instance_size: None,
    kind: Kind::Byte,
};
