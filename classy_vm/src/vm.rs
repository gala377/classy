use std::{sync::{Arc, Mutex}, thread::ThreadId, collections::HashMap};

use classy_c::code;

use crate::{
    mem::{
        allocator::Allocator,
        heap::{self, SemiSpaceKind},
        permament_heap, ObjectAllocator,
    },
    runtime::{self, thread_manager::ThreadManager, Runtime},
};

#[derive(Clone)]
pub struct Vm {
    original_thread_id: ThreadId,
    runtime: Runtime,
    permament_heap: Arc<Mutex<permament_heap::PermamentHeap>>,
    thread_manager: Arc<ThreadManager>,
    options: Options,
    // invalid at the moment threads start an evalu
    semispaces: SemiSpaces,
    interned_strings: HashMap<u64, u64>,
}

#[derive(Clone)]
pub struct Options {
    pub page_size: usize,
    pub page_align: usize,
    pub young_space_size: usize,
    pub initial_tlab_size: usize,
}

impl Vm {
    pub fn new_default(options: Options) -> Vm {
        let mut permament_heap = permament_heap::PermamentHeap::new();
        let runtime = Runtime::init(&mut permament_heap);
        let thread_manager = ThreadManager::new();
        thread_manager
            .manually_register_thread(std::thread::current().id())
            .expect("gc should not be requested before the vm is created");
        Self {
            original_thread_id: std::thread::current().id(),
            semispaces: setup_semispaces(
                options.page_size,
                options.page_align,
                options.young_space_size,
            ),
            permament_heap: Arc::new(Mutex::new(permament_heap)),
            runtime,
            options,
            thread_manager,
            interned_strings: HashMap::new(),
        }
    }

    fn clone_semispaces(&mut self) -> SemiSpaces {
        let kind = { self.semispaces.from_space.lock().unwrap().kind };
        if let SemiSpaceKind::FromSpace = kind {
            self.semispaces.clone()
        } else {
            self.semispaces = SemiSpaces {
                from_space: self.semispaces.to_space.clone(),
                to_space: self.semispaces.from_space.clone(),
            };
            self.semispaces.clone()
        }
    }

    /// Should be called inside the os thread that this evaluation
    /// thread is created for.
    pub fn create_evaluation_thread(
        &mut self,
        mut code: classy_c::code::Code,
    ) -> runtime::thread::Thread {
        while self.thread_manager.should_stop_thread_for_gc() {
            self.thread_manager.stop_for_gc().unwrap();
        }
        let SemiSpaces {
            from_space,
            to_space,
        } = self.clone_semispaces();
        self.allocate_static_objects(&mut code);
        runtime::thread::Thread::new(
            self.runtime.clone(),
            self.thread_manager.clone(),
            from_space,
            to_space,
            self.permament_heap.clone(),
            self.options.initial_tlab_size,
            self.options.young_space_size,
            code,
        )
    }

    pub fn thread_manager(&self) -> Arc<ThreadManager> {
        self.thread_manager.clone()
    }

    pub fn runtime(&self) -> Runtime {
        self.runtime.clone()
    }

    pub fn allocate_static_objects(&mut self, code: &mut classy_c::code::Code) {
        let mut instr = 0;
        let end = code.instructions.len();
        while instr < end {
            let opcode = code::OpCode::from(code.instructions[instr]);
            instr += match opcode {
                code::OpCode::ConstLoadString | code::OpCode::LookUpGlobal => {
                    assert!(code::OpCode::ConstLoadString.argument_size() == code::OpCode::LookUpGlobal.argument_size());
                    instr += 1;
                    let index_bytes = &code.instructions[instr..instr + std::mem::size_of::<u64>()];
                    let mut index_bytes_array: [u8; 8] = [0; 8];
                    assert!(index_bytes.len() == 8);
                    for i in 0..8 {
                        index_bytes_array[i] = index_bytes[i];
                    }
                    let index = u64::from_le_bytes(index_bytes_array);
                    match self.interned_strings.get(&index) {
                        Some(instance_word) => {
                            let instance_word_bytes = instance_word.to_le_bytes();
                            // overwrite the id with static pointer
                            assert!(instance_word_bytes.len() == std::mem::size_of::<u64>());
                            for i in 0..std::mem::size_of::<u64>() {
                                code.instructions[instr + i] = instance_word_bytes[i];
                            }
                        },
                        None => {
                            // retrieve the string
                            let str = code
                                .constant_pool
                                .get::<String>(index as usize)
                                .expect("checked by instruction");
                            // allocate string in the permament heap
                            let strcls = self.runtime.classes.string.clone();
                            let instance = self.permament_heap.lock().unwrap().allocate_static_string(strcls, &str);
                            assert!(!instance.is_null());
                            let instance_word = instance.unwrap() as u64;
                            let instance_word_bytes = instance_word.to_le_bytes();
                            // overwrite the id with static pointer
                            assert!(instance_word_bytes.len() == std::mem::size_of::<u64>());
                            for i in 0..std::mem::size_of::<u64>() {
                                code.instructions[instr + i] = instance_word_bytes[i];
                            }
                            // add it as interned
                            self.interned_strings.insert(index, instance_word);
                        },
                    }
                    code::OpCode::ConstLoadString.argument_size()
                }
                instr => 1 + instr.argument_size(),
            }
        }
    }
}

impl Drop for Vm {
    fn drop(&mut self) {
        if std::thread::current().id() != self.original_thread_id {
            return;
        }
        while let Err(_) = self.thread_manager.cleanup_thread() {
            self.thread_manager.stop_for_gc().unwrap();
        }
    }
}

#[derive(Clone)]
struct SemiSpaces {
    from_space: heap::SemiSpace,
    to_space: heap::SemiSpace,
}

fn setup_semispaces(page_size: usize, page_align: usize, allocated_limit: usize) -> SemiSpaces {
    let from_space =
        heap::SemiSpaceImpl::new_from_space(Allocator::new(page_size, page_align, allocated_limit));
    let to_space =
        heap::SemiSpaceImpl::new_to_space(Allocator::new(page_size, page_align, allocated_limit));
    SemiSpaces {
        from_space,
        to_space,
    }
}

#[cfg(test)]
mod tests {
    use std::mem::{align_of, size_of};

    use crate::{
        mem::{
            page::Page,
            ptr::{NonNullPtr, Ptr},
        },
        vm::{self, Vm},
    };

    fn setup_vm(page_size: usize, page_count: usize) -> Vm {
        let actual_page_size = size_of::<Page>() + page_size;
        Vm::new_default(vm::Options {
            page_size: actual_page_size,
            page_align: align_of::<usize>(),
            young_space_size: actual_page_size * page_count,
            initial_tlab_size: page_size,
        })
    }

    #[test]
    fn gc_changes_the_handles_address_but_preserves_the_value() {
        let mut vm = setup_vm(4 * size_of::<usize>(), 1);
        let mut t = vm.create_evaluation_thread(classy_c::code::Code::new());
        unsafe {
            let ptr: Ptr<isize> = t.allocate_instance(vm.runtime.classes.int);
            assert!(!ptr.is_null());
            (*ptr.unwrap()) = 123456;
            let handle = t.create_handle(NonNullPtr::from_ptr(ptr));
            let expected = t.young_space_allocated();
            assert_eq!((*handle.as_ptr()), 123456);
            assert_eq!(handle.as_ptr(), ptr.unwrap());
            t.run_young_gc();
            let actual = t.young_space_allocated();
            assert_eq!((*handle.as_ptr()), 123456);
            assert_ne!(handle.as_ptr(), ptr.unwrap());
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn revoking_a_handle_allows_gc_to_collect_garbage() {
        let mut vm = setup_vm(4 * size_of::<usize>(), 1);
        let mut t = vm.create_evaluation_thread(classy_c::code::Code::new());
        unsafe {
            let ptr: Ptr<isize> = t.allocate_instance(vm.runtime.classes.int);
            assert!(!ptr.is_null());
            let expected = t.young_space_allocated();
            let handle = t.create_handle(NonNullPtr::from_ptr(ptr));
            t.revoke_handle(handle);
            t.run_young_gc();
            let actual = t.young_space_allocated();
            assert_eq!(actual, expected - 4 * size_of::<usize>());
        }
    }
}
