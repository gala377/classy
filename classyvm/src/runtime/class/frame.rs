use classyclib::code::GcStackMapEntry;

use crate::{
    mem::ptr::{ErasedPtr, NonNullPtr, Ptr},
    runtime::{thread::Word, trace::Tracer},
};

use super::Class;

#[repr(C, align(8))]
pub struct Frame {
    pub ip: usize,
    pub stack: Vec<Word>,
    pub code: NonNullPtr<super::code::Code>,
}

impl Frame {
    pub fn get_references(&self, references: &mut Vec<*mut ErasedPtr>) {
        let ip = self.ip;
        let stack_map = {
            let mut stack_map = None;
            let code = unsafe { &*self.code.get() };
            for GcStackMapEntry { line, references } in code.code.stack_map.iter() {
                if *line < ip {
                    stack_map = Some(references);
                } else {
                    break;
                }
            }
            stack_map.expect("no matching stack map")
        };
        println!("Stack map found {:?}", stack_map);
        assert_eq!(
            self.stack.len(),
            stack_map.len(),
            "mismatched stack and stack map"
        );
        for (i, reference) in stack_map.iter().enumerate() {
            if *reference {
                let stack_entry = unsafe {
                    self.stack.get_unchecked(i) as *const u64 as *mut u64 as *mut ErasedPtr
                };
                references.push(stack_entry)
            }
        }
    }
}

unsafe fn drop_frame(frame_inst: *mut ()) {
    let frame = frame_inst as *mut Frame;
    std::ptr::drop_in_place(std::ptr::addr_of_mut!((*frame).stack));
    std::ptr::drop_in_place(std::ptr::addr_of_mut!((*frame).code));
}

unsafe fn frame_trace(frame_inst: *mut (), tracer: &mut dyn Tracer) {
    let frame = (frame_inst as *mut Frame).as_mut().unwrap();
    let ip = frame.ip;
    let stack_map = {
        let mut stack_map = None;
        let code = &*frame.code.get();
        for GcStackMapEntry { line, references } in code.code.stack_map.iter() {
            println!("Line: {}, references: {:?}", line, references);
            if *line <= ip {
                stack_map = Some(references);
            } else {
                break;
            }
        }
        stack_map.expect("no matching stack map")
    };
    println!("Current IP: {}", ip);
    println!("Stack map found {:?}", stack_map);
    println!("Stack is {:?}", frame.stack);
    assert_eq!(
        frame.stack.len(),
        stack_map.len(),
        "mismatched stack and stack map"
    );
    for (i, reference) in stack_map.iter().enumerate() {
        let stack_entry = frame.stack.get_unchecked(i);
        if *reference {
            let forward = tracer.trace_pointer(std::mem::transmute(*stack_entry));
            frame.stack[i] = forward as u64;
        }
    }
}

pub fn make_frame_class() -> Class {
    Class {
        name: Ptr::null(),
        drop: Some(drop_frame),
        trace: frame_trace,
        instance_size: std::mem::size_of::<Frame>(),
        instance_align: std::mem::align_of::<Frame>(),
        actual_instance_size: None,
        kind: crate::runtime::class::Kind::Instance,
    }
}
