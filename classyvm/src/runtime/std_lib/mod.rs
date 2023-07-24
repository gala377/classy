use std::collections::HashMap;

use crate::{
    mem::ptr::{ErasedNonNull, NonNullPtr},
    runtime::class::string::StringInst,
};

use super::{
    class::frame::Frame,
    thread::{Thread, Word},
};

pub type RuntimeFn = fn(&mut Thread, &mut [NonNullPtr<Frame>], &[Word]) -> Word;

pub fn functions() -> HashMap<String, RuntimeFn> {
    let mut m: HashMap<String, RuntimeFn> = HashMap::new();
    // print: (String) -> ()
    m.insert("print".to_owned(), native_print);
    // header_data: forall a => (a) -> Int
    m.insert("header_data".to_owned(), header_data);
    // itos: (Int) -> String
    m.insert("itos".to_owned(), itos);
    // print_n_times: (String, Int) -> ()
    m.insert("print_n_times".to_owned(), print_n_times);
    // byte_copy: forall a b => (a, b, Int, Int) -> ()
    // byte_copy: (src, dst, offset, size) -> ()
    m.insert("byte_copy".to_owned(), byte_copy);
    // add: (Int, Int) -> Int
    m.insert("add".to_owned(), add);
    // forall a b => (a) -> b
    m.insert("unsafe_cast".to_owned(), unsafe_cast);
    // forall a => (a) -> String
    m.insert("class_name".to_owned(), class_name);
    // forall a => (String) -> a
    // terminates execution
    m.insert("error".to_owned(), error);
    m
}

fn native_print(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    if args.len() != 1 {
        panic!("print accepts only one argument");
    }
    // safety: type checking ensures its safe
    unsafe {
        let str_ptr: NonNullPtr<StringInst> = std::mem::transmute(args[0]);
        println!("{}", (*str_ptr.get()).as_rust_str());
    }
    0
}
fn itos(t: &mut Thread, stack: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    if args.len() != 1 {
        panic!("parse_int accepts only one argument");
    }
    // safety: type checking ensures its safe
    unsafe {
        let as_s = args[0].to_string();
        let mut s = t.allocate_string(as_s.len());
        if s.is_none() {
            let mut frames = stack.iter_mut().map(|f| f as *mut _).collect::<Vec<_>>();
            t.heap.run_gc(t.runtime.classes.string, &mut frames);
            s = t.allocate_string(as_s.len());
            if s.is_none() {
                panic!("out of memory")
            }
        }
        let s = s.unwrap();
        std::ptr::copy_nonoverlapping(as_s.as_ptr(), s.0.as_ptr() as *mut u8, as_s.len());
        std::mem::transmute(s)
    }
}
fn byte_copy(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    let [source, dest, offset, len] = args else {
        panic!("byte_copy accepts only four arguments");
    };
    unsafe {
        let source: NonNullPtr<u8> = std::mem::transmute(*source);
        let dest: NonNullPtr<u8> = std::mem::transmute(*dest);
        let offset = *offset;
        let len = *len;
        std::ptr::copy(
            source.0.as_ptr(),
            dest.0.as_ptr().add(offset as usize),
            len as usize,
        );
    }
    0
}

fn unsafe_cast(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    args[0]
}

fn add(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    if args.len() != 2 {
        panic!("add accepts only two arguments");
    }
    args[0] + args[1]
}
fn header_data(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    if args.len() != 1 {
        panic!("array_len accepts only one argument");
    }
    unsafe {
        let arg = args[0];
        let arr_ptr: NonNullPtr<()> = std::mem::transmute(arg);
        let p = arr_ptr.data() as Word;
        println!("NATIVE DATA IS {p}");
        p
    }
}
fn print_n_times(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    if args.len() != 2 {
        panic!("print_n_times accepts only two arguments");
    }
    let str_ptr = args[0];
    let times = args[1];
    // safety: type checking ensures its safe
    unsafe {
        let str_ptr: NonNullPtr<StringInst> = std::mem::transmute(str_ptr);
        let str = (*str_ptr.get()).as_rust_str();
        for _ in 0..times {
            println!("{}", str);
        }
    }
    0
}

fn class_name(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    if args.len() != 1 {
        panic!("class_name accepts only one argument");
    }
    let arg = args[0];
    let ptr: ErasedNonNull = unsafe { std::mem::transmute(arg) };
    unsafe { (*ptr.class().get()).name().as_ptr() as Word }
}

fn error(_: &mut Thread, _: &mut [NonNullPtr<Frame>], args: &[Word]) -> Word {
    if args.len() != 1 {
        panic!("error accepts only one argument");
    }
    let arg = args[0];
    let ptr: NonNullPtr<StringInst> = unsafe { std::mem::transmute(arg) };
    let str = unsafe { (*ptr.get()).as_rust_str() };
    panic!("Error: {}", str);
}
