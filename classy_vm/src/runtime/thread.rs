use std::{alloc::Layout, sync::Arc};

use classy_c::code::{Code, OpCode};


use crate::{
    mem::{
        handle::Handle,
        heap::{self, Heap, SemiSpace},
        permament_heap,
        ptr::{NonNullPtr, Ptr},
        ObjectAllocator,
    },
    runtime::{class::Class, thread_manager::ThreadManager, Runtime},
};

type Word = u64;

pub struct Thread {
    heap: Heap,
    _permament_heap: Arc<permament_heap::PermamentHeap>,
    _runtime: Runtime,
    _thread_manager: Arc<ThreadManager>,
    code: Code,
    stack: Vec<Word>,

}

impl Thread {
    pub fn new(
        runtime: Runtime,
        thread_manager: Arc<ThreadManager>,
        from_space: SemiSpace,
        to_space: SemiSpace,
        permament_heap: Arc<permament_heap::PermamentHeap>,
        initial_tlab_free_size: usize,
        max_young_space_size: usize,
        code: Code,
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
            _runtime: runtime,
            _thread_manager: thread_manager,
            code,
            stack: Vec::new(),
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
        let code_end = self.code.instructions.len();
        while instr < code_end {
            let opcode: OpCode = self.code.instructions[instr].into();
            match opcode {
                OpCode::AddInteger => todo!(),
                OpCode::AddFloat => todo!(),
                OpCode::ConstLoadInteger => todo!(),
                OpCode::ConstLoadFloat => todo!(),
                OpCode::ConstLoadString => {
                    // TODO: this is wrong, we should not allocate every time 
                    // if we just load a string literal
                    // we should probably go through code when we first see it and allocate
                    // every string into a static heap, then replace the instruction by
                    // const load string ref and put string reference instead of an id to
                    // a string table. This requires us to instead of having a byte after the instruction
                    // have a full word to store the pointer.
                    instr += 1;
                    let str = self.code.constant_pool.get::<String>(instr).expect("checked by instruction");
                    let strcls = self._runtime.classes.string.clone();
                    let instance = self.heap.allocate_static_string(strcls, &str);
                    // unsafe as heck, there is a possiblity we could not allocate
                    self.stack.push(instance.inner().expect("could not allocate a string literal").as_ptr() as Word)
                },
                OpCode::LookUpGlobal => todo!(),
                OpCode::Return => todo!(),
                OpCode::Call1 => todo!(),
                OpCode::LastMarker => { panic!("This instruction should have never been emitted")},
            }
        }
    }

}
