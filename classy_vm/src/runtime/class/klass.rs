// Class definitions

use std::mem::{align_of, size_of};

use crate::mem::ptr::Ptr;

use super::{actual_class_size, class_trace, drop_class, Class, Kind};

pub const KLASS_CLASS: Class = Class {
    name: Ptr::null(),
    instance_size: size_of::<Class>(),
    instance_align: align_of::<Class>(),
    drop: Some(drop_class),
    kind: Kind::Klass,
    actual_instance_size: Some(actual_class_size),
    trace: class_trace,
};
