// Class definitions

use std::mem::{size_of, align_of};

use crate::mem::ptr::Ptr;

use super::{Class, drop_class, Kind, class_trace, actual_class_size};

pub const KLASS_CLASS: Class = Class {
    name: Ptr::null(),
    instance_size: size_of::<Class>(),
    instance_align: align_of::<Class>(),
    drop: Some(drop_class),
    kind: Kind::Klass,
    actual_instance_size: Some(actual_class_size),
    trace: class_trace,
};
