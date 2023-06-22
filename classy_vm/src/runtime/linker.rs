use std::sync::Arc;

use classy_c::code::constant_pool::ConstantPool;
use classy_c::code::{self, Code};
use classy_c::typecheck::r#type::Type;
use classy_c::typecheck::type_context::TypCtx;

use crate::mem::ptr::NonNullPtr;
use crate::runtime::class::string::StringInst;
use crate::runtime::class::{self, Class};
use crate::vm::Vm;

use crate::mem::ObjectAllocator;

use super::UserClasses;

pub struct Linker<'vm, 'pool> {
    vm: &'vm mut Vm,
    constant_pool: &'pool ConstantPool,
}

impl<'vm, 'pool> Linker<'vm, 'pool> {

    pub fn new(vm: &'vm mut Vm, constant_pool: &'pool ConstantPool) -> Self {
        Self {
            vm,
            constant_pool,
        }
    }

    pub fn link_types(&mut self, tctx: &TypCtx) {
        let mut user_classes = self.allocate_user_classes_keep_symbolic_references(tctx);
        self.resolve_symbolic_references_in_classes(&mut user_classes);
        self.vm.runtime.user_classes = Arc::new(user_classes);
    }

    pub fn link_code(&mut self, code: &mut Code) {
        self.replace_symbolic_references_in_code(code);
    }

    fn allocate_user_classes_keep_symbolic_references(&mut self, tctx: &TypCtx) -> UserClasses {
        let names = tctx.types.clone();
        let mut user_classes = UserClasses::new();
        for (name, tid) in names {
            let str_instance = self.intern_and_allocte_static_string(&name);
            let Type::Struct { fields, .. } = tctx.definitions.get(&tid).unwrap() else {
                panic!("Allocation of types different that structs is unsupported")
            };
            let class = Class {
                name: unsafe { std::mem::transmute(str_instance) },
                drop: None,
                trace: class::instance_trace,
                instance_size: std::mem::size_of::<usize>() * fields.len(),
                instance_align: std::mem::align_of::<usize>(),
                actual_instance_size: None,
                kind: class::Kind::Instance,
            };
            let sym_fields = fields
                .iter()
                .enumerate()
                .map(|(i, (name, typ))| {
                    let field_name = unsafe {
                        std::mem::transmute(self.intern_and_allocte_static_string(&name))
                    };
                    let sym_t_name = match typ {
                        Type::Int => "Int".to_owned(),
                        Type::UInt => "UInt".to_owned(),
                        Type::Bool => "Bool".to_owned(),
                        Type::String => "String".to_owned(),
                        Type::Float => "Float".to_owned(),
                        Type::Unit => "Unit".to_owned(),
                        Type::Struct { def, .. } => {
                            let tid = tctx.def_id_to_typ_id(*def);
                            tctx.get_name(tid).unwrap()
                        }
                        _ => {
                            panic!("Not supported yet")
                        }
                    };
                    let sym_t_name = self.intern_and_allocte_static_string(&sym_t_name);
                    class::Field {
                        name: field_name,
                        offset: i as isize,
                        class: unsafe { std::mem::transmute(sym_t_name) },
                        reference: typ.is_ref().unwrap(),
                    }
                })
                .collect::<Vec<_>>();
            let cls_ptr = self.vm.permament_heap.lock().unwrap().allocate_class(
                class,
                &sym_fields,
                self.vm.runtime.classes.klass.clone(),
            );
            user_classes.add_class(str_instance as usize, NonNullPtr::from_ptr(cls_ptr));
        }
        user_classes
    }

    fn resolve_symbolic_references_in_classes(&mut self, user_classes: &mut UserClasses) {
        unsafe fn read_field_sym_ref(
            cls_ptr: NonNullPtr<Class>,
            offset: usize,
        ) -> NonNullPtr<StringInst> {
            class::fields(cls_ptr)[offset].class.clone().cast()
        }

        unsafe fn set_field_cls(cls_ptr: NonNullPtr<Class>, offset: usize, val: NonNullPtr<Class>) {
            class::fields_mut(cls_ptr)[offset].class = val;
        }

        for (_, cls_ptr) in user_classes.iter() {
            let fields_count = unsafe { class::fields_count(cls_ptr.clone()) };
            for i in 0..fields_count {
                unsafe {
                    let sym = read_field_sym_ref(cls_ptr.clone(), i);
                    let sym_str = class::string::as_rust_string(sym);
                    let field_cls_addr = match sym_str.as_str() {
                        "Integer" => {
                            self.vm.runtime.classes.int.clone()
                        }
                        "String" => {
                            self.vm.runtime.classes.string.clone()
                        }
                        "Byte" => {
                            self.vm.runtime.classes.byte.clone()
                        }
                        _ => {
                            user_classes.get_class_ptr(std::mem::transmute(sym))
                        }
                    };
                    set_field_cls(cls_ptr.clone(), i, field_cls_addr);
                }
            }
        }
    }

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
                // add it as interned
                self.vm
                    .interned_strings
                    .insert(val.to_owned(), instance_word);
                instance_word
            }
        }
    }

    fn replace_symbolic_references_in_code(&mut self, code: &mut Code) {
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
