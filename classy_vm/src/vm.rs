use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    thread::ThreadId,
};

use classy_c::{code::constant_pool::ConstantPool, typecheck::type_context::TypCtx};

use crate::{
    mem::{
        allocator::Allocator,
        heap::{self, SemiSpaceKind},
        permament_heap,
    },
    runtime::{self, linker::Linker, thread_manager::ThreadManager, Runtime, UserClasses},
};

#[derive(Clone)]
pub struct Vm {
    original_thread_id: ThreadId,
    pub runtime: Runtime,
    pub permament_heap: Arc<Mutex<permament_heap::PermamentHeap>>,
    thread_manager: Arc<ThreadManager>,
    options: Options,
    // invalid at the moment threads start an evalu
    semispaces: SemiSpaces,
    /// Maps index from the code object ot the static string pointer
    /// TODO: This does not work as we have different code objects for each function.
    /// And as such we will get colision when allocating strings between the same constant pools.
    /// Which is a problem. All code objects should then share the same constant pool
    pub interned_strings: HashMap<String, u64>,
}

#[derive(Clone)]
pub struct Options {
    pub page_size: usize,
    pub page_align: usize,
    pub young_space_size: usize,
    pub initial_tlab_size: usize,
    pub debug: bool,
}

impl Vm {
    pub fn new_default(options: Options) -> Vm {
        let mut permament_heap = permament_heap::PermamentHeap::new();
        // TODO: Define user classes
        let runtime = Runtime::init(&mut permament_heap, UserClasses::new());
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
        constant_pool: &ConstantPool,
    ) -> runtime::thread::Thread {
        Linker::new(self, constant_pool).link_code(&mut code);
        while self.thread_manager.should_stop_thread_for_gc() {
            self.thread_manager.stop_for_gc().unwrap();
        }
        let SemiSpaces {
            from_space,
            to_space,
        } = self.clone_semispaces();
        // TODO: CODE SHOULD GO THROUGH LINKER BEFORE THIS
        //self.allocate_static_objects(&mut code);
        runtime::thread::Thread::new(
            self.runtime.clone(),
            self.thread_manager.clone(),
            from_space,
            to_space,
            self.permament_heap.clone(),
            self.options.initial_tlab_size,
            self.options.young_space_size,
            code,
            self.options.debug,
        )
    }

    pub fn thread_manager(&self) -> Arc<ThreadManager> {
        self.thread_manager.clone()
    }

    pub fn runtime(&self) -> Runtime {
        self.runtime.clone()
    }

    pub fn load_types(&mut self, type_ctx: &TypCtx, constant_pool: &ConstantPool) {
        Linker::new(self, constant_pool).link_types(type_ctx);
    }

    pub fn link_code(&mut self, constant_pool: &ConstantPool, code: &mut classy_c::code::Code) {
        Linker::new(self, constant_pool).link_code(code);
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

    use classy_c::code::constant_pool::ConstantPool;

    use crate::{
        mem::{
            page::Page,
            ptr::{NonNullPtr, Ptr},
        },
        vm::{self, Vm},
    };

    fn setup_vm(page_size: usize, page_count: usize, debug: bool) -> Vm {
        let actual_page_size = size_of::<Page>() + page_size;
        Vm::new_default(vm::Options {
            page_size: actual_page_size,
            page_align: align_of::<usize>(),
            young_space_size: actual_page_size * page_count,
            initial_tlab_size: page_size,
            debug,
        })
    }

    #[test]
    fn gc_changes_the_handles_address_but_preserves_the_value() {
        let mut vm = setup_vm(4 * size_of::<usize>(), 1, true);
        let mut t = vm.create_evaluation_thread(classy_c::code::Code::new(), &ConstantPool::new());
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
        let mut vm = setup_vm(4 * size_of::<usize>(), 1, false);
        let mut t = vm.create_evaluation_thread(classy_c::code::Code::new(), &ConstantPool::new());
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
