use std::mem::{align_of, size_of};

use crate::{
    mem::ptr::Ptr,
    runtime::class::{trace_none, Class, Kind},
};

pub const INTEGER_CLASS: Class = Class {
    name: Ptr::null(),
    drop: None,
    trace: trace_none,
    instance_size: size_of::<isize>(),
    instance_align: align_of::<isize>(),
    actual_instance_size: None,
    kind: Kind::Isize,
};
