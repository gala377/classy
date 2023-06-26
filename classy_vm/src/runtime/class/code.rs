use crate::mem::ptr::Ptr;

use super::{Class, instance_trace};

#[repr(C, align(8))]
pub struct Code {
    pub code: classy_c::code::Code,
}

unsafe fn drop_code(code_inst: *mut ()) {
    let code = code_inst as *mut Code;
    std::ptr::drop_in_place(std::ptr::addr_of_mut!((*code).code));
}


pub fn make_code_class() -> Class {
   Class {
    name: Ptr::null(),
    drop: Some(drop_code),
    trace: instance_trace,
    instance_size: std::mem::size_of::<Code>(),
    instance_align: std::mem::align_of::<Code>(),
    actual_instance_size: None,
    kind: crate::runtime::class::Kind::Instance,
}
}