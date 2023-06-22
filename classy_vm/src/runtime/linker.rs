use classy_c::code::constant_pool::ConstantPool;
use classy_c::code::{self, Code};
use classy_c::typecheck::r#type::Type;
use classy_c::typecheck::type_context::TypCtx;

use crate::mem::ptr::{NonNullPtr, Ptr};
use crate::runtime::class::{Class, self};
use crate::vm::Vm;

use crate::mem::ObjectAllocator;

use super::class::string::StringInst;

pub struct Linker<'vm, 'pool> {
    vm: &'vm mut Vm,
    constant_pool: &'pool ConstantPool,
}

impl<'vm, 'pool> Linker<'vm, 'pool> {
    // pub fn allocate_user_classes_keep_symbolic_references(&mut self, tctx: &TypCtx) {
    //     let names = tctx.types.clone();
    //     for (name, tid) in names {
    //         let str_instance = self.intern_and_allocte_static_string(&name);
    //         let class = Class {
    //             name: unsafe { std::mem::transmute(str_instance) },
    //             drop: None,
    //             trace: class::instance_trace,
    //             instance_size: todo!(),
    //             instance_align: std::mem::align_of::<usize>(),
    //             actual_instance_size: ,
    //             kind: todo!(),
    //         }
    //         self.vm.permament_heap.lock().unwrap().allocate_class(
    //             str_instance,
    //             self.vm.runtime.classes.class,
    //             self.vm.runtime.classes.klass.clone(),
    //         );
    //         let typ = tctx.definitions.get(&tid).unwrap();
    //         match typ {
    //             Type::Struct { fields, .. } => {
                    
    //             },
    //             t => {
    //                 panic!("Allocation of type {t:?} is no supported");
    //             }
    //         }
    //     }
    //     todo!(
    //         r"
    //         Go through type ctx and allocate all classes needed for the program to work.
    //         Important part is that this classes will have symbolic references that need to
    //         be replaced with pointers to the actual class.
    //         We also need to check for simple cases like String, Integer, Float, etc.
    //         Where no allocation needs to happen
    //     "
    //     )
    // }

    /// Returns a pointer to the string on a permament heap
    fn intern_and_allocte_static_string(&mut self, val: &str) -> u64 {
        match self.vm.interned_strings.get(val) {
            Some(val) => *val,
            None => {
                // allocate string in the permament heap
                let strcls = self.vm.runtime.classes.string.clone();
                let instance = self
                    .vm
                    .permament_heap
                    .lock()
                    .unwrap()
                    .allocate_static_string(strcls, &val);
                assert!(!instance.is_null());
                let instance_word = instance.unwrap() as u64;
                let instance_word_bytes = instance_word.to_le_bytes();
                // add it as interned
                self.vm.interned_strings.insert(val.to_owned(), instance_word);
                instance_word
            }
        }
    } 

    pub fn replace_symbolic_references_in_code(&mut self, code: &mut Code) {
        let mut instr = 0;
        let end = code.instructions.len();
        while instr < end {
            let opcode = code::OpCode::from(code.instructions[instr]);
            instr += match opcode {
                code::OpCode::ConstLoadString | code::OpCode::LookUpGlobal => {
                    assert!(
                        code::OpCode::ConstLoadString.argument_size()
                            == code::OpCode::LookUpGlobal.argument_size()
                    );
                    self.replace_static_string_or_symbol(instr, code)
                }
                code::OpCode::AllocHeap => self.replace_with_class_pointer(instr, code),
                code::OpCode::ConstLoadFloat | code::OpCode::ConstLoadInteger => {
                    self.replace_numeric_constant(instr, code)
                }
                i => 1 + i.argument_size(),
            }
        }
    }

    fn replace_numeric_constant(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let index_bytes = &code.instructions[instr..instr + std::mem::size_of::<u64>()];
        let mut index_bytes_array: [u8; 8] = [0; 8];
        assert!(index_bytes.len() == 8);
        for i in 0..8 {
            index_bytes_array[i] = index_bytes[i];
        }
        let index = u64::from_le_bytes(index_bytes_array);
        let val = self.constant_pool.get_raw(index as usize);
        let val_bytes = val.to_le_bytes();
        // overwrite the id with static pointer
        assert!(val_bytes.len() == std::mem::size_of::<u64>());
        for i in 0..std::mem::size_of::<u64>() {
            code.instructions[instr + i] = val_bytes[i];
        }
        code::OpCode::ConstLoadInteger.argument_size() + 1
    }

    fn replace_with_class_pointer(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let index_bytes = &code.instructions[instr..instr + std::mem::size_of::<u64>()];
        let mut index_bytes_array: [u8; 8] = [0; 8];
        assert!(index_bytes.len() == 8);
        for i in 0..8 {
            index_bytes_array[i] = index_bytes[i];
        }
        let index = u64::from_le_bytes(index_bytes_array);
        let str = self
            .constant_pool
            .get::<String>(index as usize)
            .expect("checked by instruction");
        let type_name = *self
            .vm
            .interned_strings
            .get(&str)
            .expect("expected type symbols to already be allocated");
        let cls_ptr = self
            .vm
            .runtime
            .user_classes
            .get_class_ptr(type_name as usize);
        let cls_ptr_bytes = (cls_ptr.get() as u64).to_le_bytes();
        // overwrite the id with static pointer
        assert!(cls_ptr_bytes.len() == std::mem::size_of::<u64>());
        for i in 0..std::mem::size_of::<u64>() {
            code.instructions[instr + i] = cls_ptr_bytes[i];
        }
        code::OpCode::AllocHeap.argument_size() + 1
    }

    fn replace_static_string_or_symbol(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let index_bytes = &code.instructions[instr..instr + std::mem::size_of::<u64>()];
        let mut index_bytes_array: [u8; 8] = [0; 8];
        assert!(index_bytes.len() == 8);
        for i in 0..8 {
            index_bytes_array[i] = index_bytes[i];
        }
        let index = u64::from_le_bytes(index_bytes_array);
        let cp_str_val = self
            .constant_pool
            .get::<String>(index as usize)
            .expect("checked by instruction");
        match self.vm.interned_strings.get(&cp_str_val) {
            Some(instance_word) => {
                let instance_word_bytes = instance_word.to_le_bytes();
                // overwrite the id with static pointer
                assert!(instance_word_bytes.len() == std::mem::size_of::<u64>());
                for i in 0..std::mem::size_of::<u64>() {
                    code.instructions[instr + i] = instance_word_bytes[i];
                }
            }
            None => {
                // retrieve the string
                let str = self
                    .constant_pool
                    .get::<String>(index as usize)
                    .expect("checked by instruction");
                // allocate string in the permament heap
                let strcls = self.vm.runtime.classes.string.clone();
                let instance = self
                    .vm
                    .permament_heap
                    .lock()
                    .unwrap()
                    .allocate_static_string(strcls, &str);
                assert!(!instance.is_null());
                let instance_word = instance.unwrap() as u64;
                let instance_word_bytes = instance_word.to_le_bytes();
                // overwrite the id with static pointer
                assert!(instance_word_bytes.len() == std::mem::size_of::<u64>());
                for i in 0..std::mem::size_of::<u64>() {
                    code.instructions[instr + i] = instance_word_bytes[i];
                }
                // add it as interned
                self.vm.interned_strings.insert(cp_str_val, instance_word);
            }
        }
        code::OpCode::ConstLoadString.argument_size() + 1
    }
}
