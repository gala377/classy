use std::{
    alloc::Layout,
    collections::HashMap,
    mem::size_of,
    sync::{Arc, Mutex},
};

use classy_c::code::{Code, OpCode};

use crate::{
    mem::{
        handle::Handle,
        heap::{self, Heap, SemiSpace},
        permament_heap,
        ptr::{NonNullPtr, Ptr},
        ObjectAllocator,
    },
    runtime::{
        class::{self, string::StringInst, Class},
        thread_manager::ThreadManager,
        Runtime,
    },
};

use super::class::frame::Frame;

pub type Word = u64;

pub struct Thread {
    heap: Heap,
    _permament_heap: Arc<Mutex<permament_heap::PermamentHeap>>,
    runtime: Runtime,
    _thread_manager: Arc<ThreadManager>,
    code: Code,
    debug: bool,
    // todo: this is temporary
    native_functions: HashMap<String, fn(&mut Thread, &[Word]) -> Word>,
}

impl Thread {
    pub fn new(
        runtime: Runtime,
        thread_manager: Arc<ThreadManager>,
        from_space: SemiSpace,
        to_space: SemiSpace,
        permament_heap: Arc<Mutex<permament_heap::PermamentHeap>>,
        initial_tlab_free_size: usize,
        max_young_space_size: usize,
        code: Code,
        debug: bool,
    ) -> Self {
        let id = std::thread::current().id();
        Thread {
            heap: Heap::new(
                id,
                from_space,
                to_space,
                thread_manager.clone(),
                heap::Options {
                    initial_tlab_free_size,
                    max_young_space_size,
                },
            ),
            _permament_heap: permament_heap,
            runtime,
            _thread_manager: thread_manager,
            code,
            debug,
            native_functions: {
                let mut m = HashMap::new();
                fn native_print(_: &mut Thread, args: &[Word]) -> Word {
                    if args.len() != 1 {
                        panic!("print accepts only one argument");
                    }
                    // safety: type checking ensures its safe
                    unsafe {
                        let str_ptr: NonNullPtr<StringInst> = std::mem::transmute(args[0]);
                        println!("{}", (*str_ptr.get()).as_rust_str());
                    }
                    return 0;
                }
                m.insert(
                    "print".to_owned(),
                    native_print as fn(&mut Thread, &[Word]) -> Word,
                );
                m
            },
        }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        unsafe { self.heap.try_allocate(Layout::new::<T>()).cast() }
    }

    // returned instance type T must be valid with the given class.
    pub unsafe fn allocate_instance<T>(&mut self, cls: NonNullPtr<Class>) -> Ptr<T> {
        unsafe { self.heap.allocate_instance(cls).cast() }
    }

    pub unsafe fn create_handle<T>(&mut self, ptr: NonNullPtr<T>) -> Handle<T> {
        self.heap.create_handle(ptr)
    }

    pub unsafe fn revoke_handle<T>(&mut self, handle: Handle<T>) {
        self.heap.revoke_handle(handle);
    }

    pub fn run_young_gc(&mut self) {
        self.heap.gc_young_space(0, 0);
    }

    pub fn young_space_allocated(&self) -> usize {
        self.heap.young_space_allocated()
    }

    pub fn interpert(&mut self) {
        let mut instr = 0;
        let mut frames = vec![self.alloc_frame(Arc::new(self.code.clone()))];
        let code_end = self.code.instructions.len();
        macro_rules! read_word {
            () => {{
                let bytes = &self.code.instructions[instr..instr + size_of::<u64>()];
                assert!(bytes.len() == 8);
                let mut address_bytes_array: [u8; 8] = [0; 8];
                for i in 0..8 {
                    address_bytes_array[i] = bytes[i];
                }
                u64::from_le_bytes(address_bytes_array)
            }};
        }
        let mut c_frame = frames[0];

        macro_rules! push {
            ($v:expr) => {
                #[allow(unused_unsafe)]
                unsafe {
                    (*c_frame.get()).stack.push($v);
                }
            };
        }

        macro_rules! pop {
            () => {
                unsafe { (*c_frame.get()).stack.pop().unwrap() }
            };
        }

        macro_rules! stack_get {
            ($offset:expr) => {
                unsafe { (*c_frame.get()).stack[$offset] }
            };
        }

        macro_rules! stack_set {
            ($offset:expr, $value:expr) => {
                unsafe { (*c_frame.get()).stack[$offset] = $value }
            };
        }

        while instr < code_end {
            let opcode: OpCode = unsafe { (*c_frame.get()).code.instructions[instr].into() };
            self.log(|| format!("{instr} => {opcode:?}"));
            match opcode {
                OpCode::AddInteger => todo!(),
                OpCode::AddFloat => todo!(),
                OpCode::ConstLoadFloat => {
                    instr += 1;
                    let word = read_word!();
                    push!(word);
                    instr += OpCode::ConstLoadFloat.argument_size();
                }
                OpCode::ConstLoadInteger => {
                    instr += 1;
                    let word = read_word!();
                    push!(word);
                    instr += OpCode::ConstLoadInteger.argument_size();
                }
                OpCode::ConstLoadString => {
                    instr += 1;
                    let address = read_word!();
                    let instance: Ptr<StringInst> = unsafe { std::mem::transmute(address) };
                    self.log(|| unsafe {
                        let ptr = NonNullPtr::from_ptr(instance);
                        let val = class::string::as_rust_str(&ptr);
                        format!("vm: loaded string literal: {val}")
                    });
                    push!(instance
                        .inner()
                        .expect("could not allocate a string literal")
                        .as_ptr() as Word);
                    instr += OpCode::ConstLoadString.argument_size();
                }
                OpCode::LookUpGlobal => {
                    // TODO: temporary so we just have something working
                    // on the top of the stack is a pointer to string that we need to look up
                    instr += 1;
                    let address = read_word!();
                    let name: NonNullPtr<StringInst> = unsafe { std::mem::transmute(address) };
                    self.log(|| unsafe {
                        let val = class::string::as_rust_str(&name);
                        format!("vm: looking up global: {val}")
                    });
                    let name = unsafe { (*name.get()).as_rust_string() };
                    let to_push = *self.native_functions.get(&name).expect("Unknown name");
                    push!(to_push as *mut () as Word);
                    instr += OpCode::LookUpGlobal.argument_size();
                }
                OpCode::Return => {
                    let ret = pop!();
                    frames.pop();
                    if frames.is_empty() {
                        break;
                    }
                    c_frame = frames.last().unwrap().clone();
                    instr = unsafe { (*c_frame.get()).ip };
                    push!(ret);
                }
                OpCode::Call1 => {
                    // TODO: temporary just so something works
                    // Only calls rust functions now
                    let func = pop!();
                    let arg = pop!();
                    unsafe {
                        let func: fn(&mut Thread, &[Word]) -> Word = std::mem::transmute(func);
                        let res = func(self, &[arg]);
                        push!(res);
                    }
                    instr += 1;
                }
                OpCode::LastMarker => {
                    panic!("This instruction should have never been emitted")
                }
                OpCode::Pop => {
                    pop!();
                    instr += 1;
                }
                OpCode::StackAlloc => {
                    instr += 1;
                    let size = read_word!();
                    for _ in 0..size {
                        push!(0);
                    }
                    self.log(|| format!("vm: allocated {size} words on the stack"));
                    instr += OpCode::AllocHeap.argument_size();
                }
                OpCode::StackAssign => {
                    instr += 1;
                    let offset = read_word!();
                    let value = pop!();
                    stack_set!(offset as usize, value);
                    instr += OpCode::StackAssign.argument_size();
                }
                OpCode::StackCopyBottom => {
                    instr += 1;
                    let offset = read_word!();
                    let value = stack_get!(offset as usize);
                    push!(value);
                    instr += OpCode::StackCopyBottom.argument_size();
                }
                OpCode::AllocHeap => {
                    instr += 1;
                    let class = read_word!();
                    let inst: Ptr<()> =
                        unsafe { self.allocate_instance(std::mem::transmute(class)) };
                    assert!(!inst.is_null());
                    push!(std::mem::transmute(inst));
                    instr += OpCode::AllocHeap.argument_size();
                }
                OpCode::SetOffset => {
                    instr += 1;
                    let offset = read_word!();
                    let value = pop!();
                    let address = pop!();
                    let address = address as *mut u64;
                    unsafe {
                        *address.add(offset as usize) = value;
                    }
                    instr += OpCode::SetOffset.argument_size();
                }
                OpCode::PushOffsetDeref => {
                    instr += 1;
                    let offset = read_word!();
                    let address = pop!();
                    let address = address as *mut u64;
                    let value = unsafe { address.add(offset as usize).read() };
                    push!(value);
                    instr += OpCode::PushOffsetDeref.argument_size();
                }
                i => todo!("Instruction not supported yet {i:?}"),
            }
        }
    }

    pub fn alloc_frame(&mut self, code: Arc<Code>) -> NonNullPtr<Frame> {
        let frame = unsafe { self.allocate_instance::<Frame>(self.runtime.classes.frame) };
        if frame.is_null() {
            panic!("Cannot allocate a frame should call gc");
        }
        unsafe {
            frame.inner().unwrap().as_ptr().write(Frame {
                stack: Vec::new(),
                ip: 0,
                code,
            });
        }
        NonNullPtr::from_ptr(frame)
    }

    fn log(&self, f: impl FnOnce() -> String) {
        if self.debug {
            let res = f();
            println!("vm: {res}");
        }
    }
}
