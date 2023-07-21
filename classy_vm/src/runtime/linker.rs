use std::collections::HashMap;

use std::sync::Arc;

use classy_c::code::constant_pool::ConstantPool;
use classy_c::code::{self, Code};

use classy_c::typecheck::r#type::Type;
use classy_c::typecheck::type_context::TypCtx;

use crate::mem::ptr::{ErasedNonNull, NonNullPtr};

use crate::runtime::class::{self, Class};
use crate::vm::Vm;

use crate::mem::ObjectAllocator;

use super::UserClasses;

pub struct Linker<'vm, 'pool> {
    vm: &'vm mut Vm,
    constant_pool: &'pool ConstantPool,
    functions: HashMap<String, ErasedNonNull>,
    array_classes: HashMap<String, NonNullPtr<Class>>,
}

impl<'vm, 'pool> Linker<'vm, 'pool> {
    pub fn new(vm: &'vm mut Vm, constant_pool: &'pool ConstantPool) -> Self {
        Self {
            vm,
            constant_pool,
            functions: HashMap::new(),
            array_classes: HashMap::new(),
        }
    }

    pub fn link_types(&mut self, tctx: &TypCtx) {
        let mut user_classes = self.allocate_user_classes_keep_symbolic_references(tctx);
        self.resolve_symbolic_references_in_classes(&mut user_classes);
        self.vm.runtime.user_classes = Arc::new(user_classes);
    }

    pub fn link_functions(
        &mut self,
        functions: &mut Vec<(String, Code)>,
    ) -> HashMap<String, ErasedNonNull> {
        self.allocate_code_objects(functions);
        let codes = self.functions.values().cloned().collect::<Vec<_>>();
        for code in codes {
            unsafe {
                let mut code_ptr: NonNullPtr<class::code::Code> = code.clone().cast();
                let code_inst = code_ptr.0.as_mut();
                let code = &mut code_inst.code;
                self.link_code(code);
            }
        }
        self.functions.clone()
    }

    fn allocate_code_objects(&mut self, functions: &mut Vec<(String, Code)>) {
        for (name, code) in functions {
            let code_ptr = self
                .vm
                .permament_heap
                .lock()
                .unwrap()
                .allocate_instance(self.vm.runtime.classes.code);
            let code_ptr = unsafe {
                let code_ptr_non_null = NonNullPtr::from_ptr(code_ptr).cast::<class::code::Code>();
                std::ptr::write(
                    code_ptr_non_null.get(),
                    class::code::Code { code: code.clone() },
                );
                code_ptr_non_null.cast()
            };
            self.functions.insert(name.clone(), code_ptr);
        }
    }

    pub fn link_code(&mut self, code: &mut Code) {
        self.replace_symbolic_references_in_code(code);
    }

    fn instance_until_class(&self, tctx: &TypCtx, tid: usize) -> Option<Type> {
        fn implementation(tctx: &TypCtx, t: Type) -> Option<Type> {
            match t {
                t @ Type::Struct { .. } => Some(t),
                t @ Type::ADT { .. } => Some(t),
                Type::Scheme { typ, .. } => implementation(tctx, *typ),
                Type::App { typ, .. } => implementation(tctx, *typ),
                Type::Alias(for_t) => {
                    let t = tctx.definitions.get(&for_t).unwrap();
                    implementation(tctx, t.clone())
                }
                _ => None,
            }
        }
        let t = tctx.definitions.get(&tid).unwrap();
        implementation(tctx, t.clone())
    }

    fn get_type_name(tctx: &TypCtx, t: Type) -> String {
        match t {
            Type::Int => "Int".to_owned(),
            Type::UInt => "UInt".to_owned(),
            Type::Bool => "Bool".to_owned(),
            Type::String => "String".to_owned(),
            Type::Float => "Float".to_owned(),
            Type::Tuple(_) => "Tuple".to_owned(),
            Type::Unit => "Unit".to_owned(),
            Type::Generic(_, _) => "@GenericRef".to_owned(),
            Type::Struct { def, .. } => {
                let tid = tctx.def_id_to_typ_id(def);
                tctx.get_name(tid).unwrap()
            }
            Type::App { typ, .. } => Self::get_type_name(tctx, *typ),
            Type::Scheme { typ, .. } => Self::get_type_name(tctx, *typ),
            Type::Alias(for_t) => {
                let t = tctx.definitions.get(&for_t).unwrap();
                Self::get_type_name(tctx, t.clone())
            }
            Type::Function { .. } => "Code".to_owned(),
            Type::ADT { def, .. } => {
                let tid = tctx.def_id_to_typ_id(def);
                tctx.get_name(tid).unwrap()
            }
            t => {
                panic!("Not supported yet {t:?}")
            }
        }
    }

    fn allocate_user_classes_keep_symbolic_references(&mut self, tctx: &TypCtx) -> UserClasses {
        let names = tctx.types.clone();
        let mut user_classes = UserClasses::new();
        for (name, tid) in names {
            let str_instance = self.intern_and_allocte_static_string(&name);
            match self.instance_until_class(tctx, tid) {
                Some(Type::Struct { fields, .. }) => {
                    println!("Linking struct {name:?}");
                    self.create_struct_class(str_instance, &fields, tctx, &mut user_classes);
                }
                Some(Type::ADT { constructors, .. }) => {
                    println!("Linking ADT {name:?}");
                    self.create_adt_class(
                        str_instance,
                        tctx,
                        &mut user_classes,
                        constructors,
                        name,
                    );
                }
                _ => {
                    println!(
                        "Linking of {:?} skipped",
                        tctx.definitions.get(&tid).unwrap()
                    );
                    continue;
                }
            }
        }
        user_classes
    }

    fn create_adt_class(
        &mut self,
        str_instance: u64,
        tctx: &TypCtx,
        user_classes: &mut UserClasses,
        constructors: Vec<(String, Type)>,
        name: String,
    ) {
        self.create_struct_class(str_instance, &[], tctx, user_classes);
        for (pos, (_, t)) in constructors.iter().enumerate() {
            let name = format!("{}@cons@{}", name, pos);
            let str_instance = self.intern_and_allocte_static_string(&name);
            match t {
                Type::Unit => self.create_struct_class(str_instance, &[], tctx, user_classes),
                Type::Struct { fields, .. } => {
                    self.create_struct_class(str_instance, fields, tctx, user_classes)
                }
                Type::Tuple(fields) => {
                    let fields = fields
                        .iter()
                        .enumerate()
                        .map(|(i, t)| (format!("{}", i), t.clone()))
                        .collect::<Vec<_>>();
                    self.create_struct_class(str_instance, &fields, tctx, user_classes);
                }
                _ => panic!("Unexpected type in ADT constructor"),
            }
        }
    }

    fn create_struct_class(
        &mut self,
        str_instance: u64,
        fields: &[(String, Type)],
        tctx: &TypCtx,
        user_classes: &mut UserClasses,
    ) {
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
                let field_name =
                    unsafe { std::mem::transmute(self.intern_and_allocte_static_string(name)) };
                let sym_t_name = Self::get_type_name(tctx, typ.clone());
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
            self.vm.runtime.classes.klass,
        );
        user_classes.add_class(str_instance as usize, NonNullPtr::from_ptr(cls_ptr));
    }

    fn resolve_symbolic_references_in_classes(&mut self, user_classes: &mut UserClasses) {
        unsafe fn read_field_sym_ref(cls_ptr: NonNullPtr<Class>, offset: usize) -> class::Field {
            class::fields(cls_ptr)[offset].clone()
        }

        unsafe fn set_field_cls(cls_ptr: NonNullPtr<Class>, offset: usize, val: NonNullPtr<Class>) {
            class::fields_mut(cls_ptr)[offset].class = val;
        }

        for (name, cls_ptr) in user_classes.iter() {
            unsafe {
                let sym_str = class::string::as_rust_string(std::mem::transmute(*name));
                println!("resolving symbolic references of class {sym_str}");
            }
            let fields_count = unsafe { class::fields_count(*cls_ptr) };
            for i in 0..fields_count {
                unsafe {
                    let field = read_field_sym_ref(*cls_ptr, i);
                    {
                        let sym = class::string::as_rust_string(std::mem::transmute(field.class));
                        let name = class::string::as_rust_string(field.name);
                        println!("resolving symbolic references of field {name} with type {sym}");
                    }
                    let sym = field.class.cast();
                    let sym_str = class::string::as_rust_string(sym);
                    let field_cls_addr = match sym_str.as_str() {
                        "Int" => self.vm.runtime.classes.int,
                        "String" => self.vm.runtime.classes.string,
                        "Byte" => self.vm.runtime.classes.byte,
                        "@GenericRef" => self.vm.runtime.classes.gref,
                        "Code" => self.vm.runtime.classes.code,
                        "Tuple" => self.vm.runtime.classes.tuple,
                        _ => user_classes.get_class_ptr(std::mem::transmute(sym)),
                    };
                    set_field_cls(*cls_ptr, i, field_cls_addr);
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
                let strcls = self.vm.runtime.classes.string;
                let instance = self
                    .vm
                    .permament_heap
                    .lock()
                    .unwrap()
                    .allocate_static_string(strcls, val);
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
                code::OpCode::ConstLoadString => self.replace_static_string_or_symbol(instr, code),
                code::OpCode::LookUpGlobal => self.replace_symbol(instr, code),
                code::OpCode::RuntimeCall => self.replace_static_string_or_symbol(instr, code),
                code::OpCode::AllocHeap => self.replace_with_class_pointer(instr, code),
                code::OpCode::ConstLoadFloat | code::OpCode::ConstLoadInteger => {
                    self.replace_numeric_constant(instr, code)
                }
                code::OpCode::AllocArray => self.alloc_array_class_and_replace_args(instr, code),
                i => 1 + i.argument_size(),
            }
        }
    }
    fn read_word(&self, instr: usize, code: &Code) -> u64 {
        let bytes = &code.instructions[instr..instr + std::mem::size_of::<u64>()];
        let mut bytes_array: [u8; 8] = [0; 8];
        assert!(bytes.len() == 8);
        bytes_array[..8].copy_from_slice(&bytes[..8]);
        u64::from_le_bytes(bytes_array)
    }

    fn alloc_array_class_and_replace_args(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let elem_size = self.read_word(instr, code);
        let elem_align = self.read_word(instr + std::mem::size_of::<usize>(), code);
        let is_ref = self.read_word(instr + std::mem::size_of::<u64>() * 2, code);
        let name = format!("[{elem_size};{elem_align};{is_ref}");
        let array_cls_ptr = match self.array_classes.get(&name).cloned() {
            Some(cls) => cls,
            None => {
                let klass = self.vm.runtime.classes.klass;
                let fields = &[];
                let array_cls = self.vm.permament_heap.lock().unwrap().allocate_class(
                    class::array::mk_array_cls(
                        elem_size as usize,
                        elem_align as usize,
                        is_ref == 1,
                    ),
                    fields,
                    klass,
                );
                let array_cls = NonNullPtr::from_ptr(array_cls);
                let name_ptr = self
                    .vm
                    .permament_heap
                    .lock()
                    .unwrap()
                    .allocate_static_string(self.vm.runtime.classes.string, &name);
                self.array_classes.insert(name, array_cls);
                unsafe {
                    if name_ptr.is_null() {
                        panic!("Out of memeory");
                    }
                    let name_ptr = name_ptr.cast::<class::string::StringInst>();
                    (*array_cls.get()).name = name_ptr;
                }
                array_cls
            }
        };
        let array_cls_ptr_bytes = (array_cls_ptr.get() as u64).to_le_bytes();
        assert!(array_cls_ptr_bytes.len() == std::mem::size_of::<u64>());
        code.instructions[instr..(std::mem::size_of::<u64>() + instr)]
            .copy_from_slice(&array_cls_ptr_bytes[..std::mem::size_of::<u64>()]);
        code::OpCode::AllocArray.argument_size() + 1
    }

    fn replace_symbol(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let index = self.read_word(instr, code);
        let function_name = self
            .constant_pool
            .get::<String>(index as usize)
            .expect("checked by instruction");
        let function_ptr = self.functions.get(&function_name).cloned().expect(&format!(
            "all function should have been already allocated: {function_name}"
        ));
        let fn_ptr_bytes = (function_ptr.get() as u64).to_le_bytes();
        // overwrite the id with static pointer
        assert!(fn_ptr_bytes.len() == std::mem::size_of::<u64>());
        code.instructions[instr..(std::mem::size_of::<u64>() + instr)]
            .copy_from_slice(&fn_ptr_bytes[..std::mem::size_of::<u64>()]);
        code::OpCode::LookUpGlobal.argument_size() + 1
    }

    fn replace_numeric_constant(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let index = self.read_word(instr, code);
        let val = self.constant_pool.get_raw(index as usize);
        let val_bytes = val.to_le_bytes();
        // overwrite the id with static pointer
        assert!(val_bytes.len() == std::mem::size_of::<u64>());
        code.instructions[instr..(std::mem::size_of::<u64>() + instr)]
            .copy_from_slice(&val_bytes[..std::mem::size_of::<u64>()]);
        code::OpCode::ConstLoadInteger.argument_size() + 1
    }

    fn replace_with_class_pointer(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let index = self.read_word(instr, code);
        let str = self
            .constant_pool
            .get::<String>(index as usize)
            .expect("checked by instruction");
        let cls_ptr = match str.as_str() {
            // TODO: ugly, make interner point to runtime classes as well
            "Tuple" => self.vm.runtime.classes.tuple,
            str => {
                let type_name = *self.vm.interned_strings.get(str).expect(&format!(
                    "expected type symbols to already be allocated {str}"
                ));
                self.vm
                    .runtime
                    .user_classes
                    .get_class_ptr(type_name as usize)
            }
        };
        let cls_ptr_bytes = (cls_ptr.get() as u64).to_le_bytes();
        // overwrite the id with static pointer
        assert!(cls_ptr_bytes.len() == std::mem::size_of::<u64>());
        code.instructions[instr..(instr + std::mem::size_of::<u64>())]
            .copy_from_slice(&cls_ptr_bytes[..std::mem::size_of::<u64>()]);
        code::OpCode::AllocHeap.argument_size() + 1
    }

    fn replace_static_string_or_symbol(&mut self, mut instr: usize, code: &mut Code) -> usize {
        instr += 1;
        let index = self.read_word(instr, code);
        let cp_str_val = self
            .constant_pool
            .get::<String>(index as usize)
            .expect("checked by instruction");
        match self.vm.interned_strings.get(&cp_str_val) {
            Some(instance_word) => {
                let instance_word_bytes = instance_word.to_le_bytes();
                // overwrite the id with static pointer
                assert!(instance_word_bytes.len() == std::mem::size_of::<u64>());
                code.instructions[instr..(instr + std::mem::size_of::<u64>())]
                    .copy_from_slice(&instance_word_bytes[..std::mem::size_of::<u64>()]);
            }
            None => {
                // retrieve the string
                let str = self
                    .constant_pool
                    .get::<String>(index as usize)
                    .expect("checked by instruction");
                // allocate string in the permament heap
                let strcls = self.vm.runtime.classes.string;
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
                code.instructions[instr..(instr + std::mem::size_of::<u64>())]
                    .copy_from_slice(&instance_word_bytes[..std::mem::size_of::<u64>()]);
                // add it as interned
                self.vm.interned_strings.insert(cp_str_val, instance_word);
            }
        }
        code::OpCode::ConstLoadString.argument_size() + 1
    }
}
