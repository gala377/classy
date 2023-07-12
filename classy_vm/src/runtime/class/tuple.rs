use std::mem::{align_of, size_of};

use crate::{mem::ptr::Ptr, runtime::trace::Tracer};

use super::{header::Header, Class, Kind};

const SIZE_MASK: usize = 0b111111;
const TAG_SHIFT: usize = 6;

/// Tuple uses its data field to store its length
/// (on the first 6 bytes). The rest of the bytes
/// are used as a bitmap to indicate which fields
/// are references.
/// 
/// So teh format of the data looks like so
/// 
/// [bitmap: 64-6 bytes][length: 6 bytes]
/// 
/// the most rightmost bit of the bitmap corresponds
/// to the first field of the tuple and so on.
pub const TUPLE_CLASS: Class = Class {
    name: Ptr::null(),
    instance_size: size_of::<usize>(),
    instance_align: align_of::<usize>(),
    drop: None,
    kind: Kind::Instance,
    actual_instance_size: Some(actual_tuple_size),
    trace: tuple_trace,
};

fn tuple_len(data: usize) -> usize {
    data & SIZE_MASK
}

fn tuple_trace_map(data: usize) -> Vec<bool> {
    let len = tuple_len(data);
    let masked = data >> TAG_SHIFT;
    let mut vec = Vec::with_capacity(len);
    for i in 0..len {
        vec.push(masked & (1 << i) != 0);
    }
    vec
}

unsafe fn actual_tuple_size(tuple: *const ()) -> usize {
    let header = tuple.cast::<Header>().sub(1);
    let data = (*header).data;
    let len = tuple_len(data);
    len * size_of::<usize>()
}

fn tuple_trace(obj: *mut (), tracer: &mut dyn Tracer) {
    unsafe {
        let header = obj.cast::<Header>().sub(1);
        let data = (*header).data;
        let map = tuple_trace_map(data);
        for (i, field) in map.into_iter().enumerate() {
            if field {
                let field_ptr = *(obj.cast::<u64>().add(i));
                let forward = tracer.trace_pointer(std::mem::transmute(field_ptr));
                *(obj.cast::<u64>().add(i)) = forward as u64;
            }
        }
    }
}
