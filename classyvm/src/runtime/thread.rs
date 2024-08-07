use core::panic;
use std::{
    alloc::Layout,
    collections::HashMap,
    mem::size_of,
    sync::{Arc, Mutex},
};

use classyclib::code::OpCode;

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

use super::{
    class::frame::Frame,
    std_lib::{self, RuntimeFn},
};

pub type Word = u64;

pub struct ThreadArgs {
    pub runtime: Runtime,
    pub thread_manager: Arc<ThreadManager>,
    pub from_space: SemiSpace,
    pub to_space: SemiSpace,
    pub permament_heap: Arc<Mutex<permament_heap::PermamentHeap>>,
    pub initial_tlab_free_size: usize,
    pub max_young_space_size: usize,
    pub code: NonNullPtr<class::code::Code>,
    pub debug: bool,
}

pub struct Thread {
    pub heap: Heap,
    pub _permament_heap: Arc<Mutex<permament_heap::PermamentHeap>>,
    pub runtime: Runtime,
    pub thread_manager: Arc<ThreadManager>,
    pub code: NonNullPtr<class::code::Code>,
    pub debug: bool,
    // todo: this is temporary, ideally linker should
    // replace symbolic references to runtime functions
    // with their addresses
    pub native_functions: HashMap<String, RuntimeFn>,
}

macro_rules! log {
    ($on:expr, $fmt:literal, $($args:expr),*) => {
        $on.log(|| format!($fmt, $($args),*))
    };
}

impl Thread {
    pub fn new(
        ThreadArgs {
            runtime,
            thread_manager,
            from_space,
            to_space,
            permament_heap,
            initial_tlab_free_size,
            max_young_space_size,
            code,
            debug,
        }: ThreadArgs,
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
            thread_manager,
            code,
            debug,
            native_functions: std_lib::functions(),
        }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        unsafe { self.heap.try_allocate(Layout::new::<T>()).cast() }
    }

    /// # Safety
    ///
    /// Returned instance type T must be valid with the given class.
    pub unsafe fn allocate_instance<T>(&mut self, cls: NonNullPtr<Class>) -> Ptr<T> {
        self.heap.allocate_instance(cls).cast()
    }

    /// # Safety
    ///
    /// Pointer has to be non null.
    pub unsafe fn create_handle<T>(&mut self, ptr: NonNullPtr<T>) -> Handle<T> {
        self.heap.create_handle(ptr)
    }

    /// # Safety
    ///
    /// Handle has to be valid.
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
        let mut frames = vec![self.alloc_frame(self.code).expect("Out of memory")];
        let mut code_end = unsafe { (*self.code.get()).code.instructions.len() };
        let mut code = unsafe { &*self.code.get() };
        macro_rules! read_word {
            () => {{
                let bytes = &code.code.instructions[instr..instr + size_of::<u64>()];
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

        macro_rules! safepoint {
            () => {
                unsafe {
                    (*c_frame.get()).ip = instr;
                    let frame_ptrs = frames.iter_mut().map(|f| f as *mut _).collect::<Vec<_>>();
                    self.gc_safepoint(&frame_ptrs);
                }
            };
        }

        while instr < code_end {
            let opcode: OpCode = code.code.instructions[instr].into();
            log!(self, "{} => {:?}", instr, opcode);
            match opcode {
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
                    instr += 1;
                    let address = read_word!();
                    push!(address);
                    instr += OpCode::LookUpGlobal.argument_size();
                }
                OpCode::Return => {
                    log!(self, "{}", "Executing return");
                    let ret = pop!();
                    frames.pop();
                    if frames.is_empty() {
                        break;
                    }
                    c_frame = *frames.last().unwrap();
                    instr = unsafe { (*c_frame.get()).ip };
                    code = unsafe { &*(*c_frame.get()).code.get() };
                    code_end = code.code.instructions.len();
                    push!(ret);
                }
                OpCode::CallNative1 => {
                    safepoint!();
                    let func = pop!();
                    let arg = pop!();
                    let func: RuntimeFn = unsafe { std::mem::transmute(func) };
                    let ret = func(self, &mut frames, &[arg]);
                    push!(ret);
                    instr += 1;
                }
                OpCode::CallNative => {
                    safepoint!();
                    instr += 1;
                    let n = read_word!();
                    let func = pop!();
                    let mut args = Vec::new();
                    for _ in 0..n {
                        args.push(pop!());
                    }
                    let func: RuntimeFn = unsafe { std::mem::transmute(func) };
                    let ret = func(
                        self,
                        &mut frames,
                        &args.into_iter().rev().collect::<Vec<_>>(),
                    );
                    push!(ret);
                    instr += OpCode::CallNative1.argument_size();
                }
                OpCode::RuntimeCall => {
                    instr += 1;
                    let name_ptr = read_word!();
                    unsafe {
                        let name_ptr: NonNullPtr<StringInst> = std::mem::transmute(name_ptr);
                        let name = class::string::as_rust_str(&name_ptr);
                        let func = *self.native_functions.get(name).unwrap();
                        push!(func as usize as u64);
                    };
                    instr += OpCode::RuntimeCall.argument_size();
                }
                OpCode::Call0 => {
                    safepoint!();
                    let func = pop!();
                    let code_inst = unsafe {
                        let code: Ptr<class::code::Code> = std::mem::transmute(func);
                        if code.is_null() {
                            panic!("Null pointer dereference");
                        }
                        NonNullPtr::from_ptr(code)
                    };
                    let mut new_frame = self.alloc_frame(code_inst);
                    if new_frame.is_none() {
                        // we need to gc
                        log!(self, "{}", "RUNNING GC BECAUSE I CANNOT ALLOCATE THE FRAME");

                        push!(func);
                        let frame_cls = self.runtime.classes.frame;
                        unsafe { (*c_frame.get()).ip = instr };
                        let mut frames = frames.iter_mut().map(|f| f as *mut _).collect::<Vec<_>>();
                        self.heap.run_gc(frame_cls, &mut frames);
                        new_frame = self.alloc_frame(code_inst);
                        if new_frame.is_none() {
                            panic!("Out of memory");
                        }
                        pop!();
                    }
                    let new_frame = new_frame.unwrap();
                    unsafe {
                        std::ptr::write(
                            new_frame.get(),
                            Frame {
                                ip: 0,
                                stack: vec![],
                                code: code_inst,
                            },
                        )
                    };
                    unsafe { (*c_frame.get()).ip = instr + 1 };
                    frames.push(new_frame);
                    code = unsafe { &*code_inst.get() };
                    instr = 0;
                    c_frame = new_frame;
                    code_end = code.code.instructions.len();
                }
                OpCode::Call1 => {
                    safepoint!();
                    let func = pop!();
                    let code_inst = unsafe {
                        let code: Ptr<class::code::Code> = std::mem::transmute(func);
                        if code.is_null() {
                            panic!("Null pointer dereference");
                        }
                        NonNullPtr::from_ptr(code)
                    };
                    let mut new_frame = self.alloc_frame(code_inst);
                    if new_frame.is_none() {
                        // we need to gc
                        log!(self, "{}", "RUNNING GC BECAUSE I CANNOT ALLOCATE THE FRAME");

                        push!(func);
                        let frame_cls = self.runtime.classes.frame;
                        unsafe { (*c_frame.get()).ip = instr };
                        let mut frames = frames.iter_mut().map(|f| f as *mut _).collect::<Vec<_>>();
                        self.heap.run_gc(frame_cls, &mut frames);
                        new_frame = self.alloc_frame(code_inst);
                        if new_frame.is_none() {
                            panic!("Out of memory");
                        }
                        pop!();
                    }
                    let new_frame = new_frame.unwrap();
                    let arg = pop!();
                    unsafe {
                        std::ptr::write(
                            new_frame.get(),
                            Frame {
                                ip: 0,
                                stack: vec![arg],
                                code: code_inst,
                            },
                        )
                    };
                    unsafe { (*c_frame.get()).ip = instr + 1 };
                    frames.push(new_frame);
                    code = unsafe { &*code_inst.get() };
                    instr = 0;
                    c_frame = new_frame;
                    code_end = code.code.instructions.len();
                }
                OpCode::CallN => {
                    safepoint!();
                    instr += 1;
                    let func = pop!();
                    let code_inst = unsafe {
                        let code: Ptr<class::code::Code> = std::mem::transmute(func);
                        if code.is_null() {
                            panic!("Null pointer dereference");
                        }
                        NonNullPtr::from_ptr(code)
                    };
                    let mut new_frame = self.alloc_frame(code_inst);
                    if new_frame.is_none() {
                        // we need to gc
                        log!(self, "{}", "RUNNING GC BECAUSE I CANNOT ALLOCATE THE FRAME");

                        push!(func);
                        let frame_cls = self.runtime.classes.frame;
                        unsafe { (*c_frame.get()).ip = instr };
                        let mut frames = frames.iter_mut().map(|f| f as *mut _).collect::<Vec<_>>();
                        self.heap.run_gc(frame_cls, &mut frames);
                        new_frame = self.alloc_frame(code_inst);
                        if new_frame.is_none() {
                            panic!("Out of memory");
                        }
                        pop!();
                    }
                    let new_frame = new_frame.unwrap();
                    let n = read_word!();
                    let mut args = Vec::new();
                    for _ in 0..n {
                        args.push(pop!());
                    }
                    args.reverse();
                    unsafe {
                        std::ptr::write(
                            new_frame.get(),
                            Frame {
                                ip: 0,
                                stack: args,
                                code: code_inst,
                            },
                        )
                    };
                    unsafe { (*c_frame.get()).ip = instr + OpCode::CallN.argument_size() };
                    frames.push(new_frame);
                    code = unsafe { &*code_inst.get() };
                    instr = 0;
                    c_frame = new_frame;
                    code_end = code.code.instructions.len();
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
                    log!(self, "vm: allocated {} words on the stack", size);
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
                    safepoint!();
                    instr += 1;
                    let class = read_word!();
                    let mut inst: Ptr<()> = unsafe {
                        self.allocate_instance(std::mem::transmute::<u64, NonNullPtr<Class>>(class))
                    };
                    if inst.is_null() {
                        // we need to gc
                        let mut frames = frames.iter_mut().map(|f| f as *mut _).collect::<Vec<_>>();
                        unsafe {
                            self.heap.run_gc(
                                std::mem::transmute::<u64, NonNullPtr<Class>>(class),
                                &mut frames,
                            );
                        }
                        inst = unsafe {
                            self.allocate_instance(std::mem::transmute::<u64, NonNullPtr<Class>>(
                                class,
                            ))
                        };
                        if inst.is_null() {
                            panic!("Out of memory");
                        }
                    }
                    push!(std::mem::transmute::<Ptr<()>, u64>(inst));
                    instr += OpCode::AllocHeap.argument_size();
                }
                OpCode::AllocArray => {
                    safepoint!();
                    instr += 1;
                    let array_cls = read_word!();
                    let size = pop!();
                    let array_cls: NonNullPtr<Class> = unsafe { std::mem::transmute(array_cls) };
                    let mut arr = self.heap.allocate_array(array_cls, size as usize);
                    if arr.is_null() {
                        let mut frames = frames.iter_mut().map(|f| f as *mut _).collect::<Vec<_>>();
                        self.heap.run_gc(array_cls, &mut frames);
                        arr = self.heap.allocate_array(array_cls, size as usize);
                        if arr.is_null() {
                            panic!("Out of memory");
                        }
                    }
                    push!(std::mem::transmute::<Ptr<()>, u64>(arr));
                    instr += OpCode::AllocArray.argument_size();
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
                OpCode::SetOffsetNegative => {
                    instr += 1;
                    let offset = read_word!();
                    let value = pop!();
                    let address = pop!();
                    let address = address as *mut u64;
                    unsafe {
                        *address.sub(offset as usize) = value;
                    }
                    instr += OpCode::SetOffsetNegative.argument_size();
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
                OpCode::PushOffsetDerefNegative => {
                    instr += 1;
                    let offset = read_word!();
                    let address = pop!();
                    let address = address as *mut u64;
                    let value = unsafe { address.sub(offset as usize).read() };
                    push!(value);
                    instr += OpCode::PushOffsetDerefNegative.argument_size();
                }
                OpCode::EqInteger => {
                    instr += 1;
                    let a = pop!();
                    let b = pop!();
                    push!(if a == b { 1 } else { 0 });
                }
                OpCode::PushUnit => {
                    push!(0);
                    instr += 1;
                }
                OpCode::JumpFrontIfFalse => {
                    instr += 1;
                    let offset = read_word!();
                    let value = pop!();
                    if value == 0 {
                        instr += (offset as usize) - 1;
                    } else {
                        instr += OpCode::JumpFrontIfFalse.argument_size();
                    }
                }
                OpCode::JumpFront => {
                    instr += 1;
                    let offset = read_word!();
                    instr += offset as usize - 1;
                }
                // OpCode::AddInteger => {
                //     instr += 1;
                //     let a = pop!();
                //     let b = pop!();
                //     push!(a + b);
                // }
                OpCode::PushTrue => {
                    push!(1);
                    instr += 1;
                }
                OpCode::PushFalse => {
                    push!(0);
                    instr += 1;
                }
                i => todo!("Instruction not supported yet {i:?}"),
            }
        }
    }

    pub fn allocate_string(&mut self, size: usize) -> Option<NonNullPtr<StringInst>> {
        let s = self.heap.allocate_array(self.runtime.classes.string, size);
        if s.is_null() {
            return None;
        }
        Some(NonNullPtr::from_ptr(unsafe { s.cast() }))
    }

    pub fn alloc_frame(
        &mut self,
        code: NonNullPtr<class::code::Code>,
    ) -> Option<NonNullPtr<Frame>> {
        log!(self, "{}", "vm: Allocating frame");
        let frame = unsafe { self.allocate_instance::<Frame>(self.runtime.classes.frame) };
        if frame.is_null() {
            log!(self, "{}", "COULD NOT ALLOCATE FRAME ITS NULL");
            return None;
        }
        unsafe {
            frame.inner().unwrap().as_ptr().write(Frame {
                stack: Vec::new(),
                ip: 0,
                code,
            });
        }
        Some(NonNullPtr::from_ptr(frame))
    }

    fn log(&self, f: impl FnOnce() -> String) {
        if self.debug {
            let res = f();
            println!("vm: {res}");
        }
    }

    #[inline]
    fn gc_safepoint(&mut self, stack: &[*mut NonNullPtr<Frame>]) {
        if self.thread_manager.should_stop_thread_for_gc() {
            self.export_gc_data(stack);
            self.thread_manager.stop_for_gc().unwrap();
        }
    }

    fn export_gc_data(&mut self, stack: &[*mut NonNullPtr<Frame>]) {
        self.thread_manager
            .update_gc_data(std::thread::current().id(), stack.into());
    }
}
