use std::mem::{align_of, size_of};

use crate::{mem::ptr::Ptr, runtime::trace::Tracer};

use super::{Class, Kind};

pub const GENERIC_REF_CLASS: Class = Class {
    name: Ptr::null(),
    drop: None,
    trace: panic_trace,
    instance_size: size_of::<usize>(),
    instance_align: align_of::<usize>(),
    actual_instance_size: Some(panic_actual_size),
    kind: Kind::Instance,
};

unsafe fn panic_trace(_: *mut (), _: &mut dyn Tracer) {
    panic!("tracing a generic class");
}

unsafe fn panic_actual_size(_: *const ()) -> usize {
    panic!("getting actual size of a generic class");
}
