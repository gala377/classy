#![allow(unstable_name_collisions)]

pub mod array;
pub mod header;
pub mod string;

use std::{fmt::Debug, mem::size_of, ops::Index};

use sptr::Strict;

use crate::{
    mem::ptr::{ErasedNonNull, ErasedPtr, NonNullPtr},
    runtime::{class::string::StringInst, trace::Tracer},
};

/// Class is aligned to the word boundary so that
/// value of any aligment can go right after it.
/// In particular, class fields can be placed right
/// after the struct no matter their alignment.
///
/// Header's data field for classes holds number
/// of fields.
#[derive(Clone)]
#[repr(align(8))]
pub struct Class {
    pub name: NonNullPtr<StringInst>,
    pub drop: Option<unsafe fn(*mut ())>,
    pub trace: unsafe fn(*mut (), &mut dyn Tracer),
    pub instance_size: usize,
    pub instance_align: usize,
    pub actual_instance_size: Option<unsafe fn(*const ()) -> usize>,
    pub kind: Kind,
    // Variable length sequence of fields
}

impl Class {
    /// Unsafe becaues it reaches past the class for variadic arguments.
    /// Can only be used on classes allocated within the managed heap.
    pub unsafe fn fields(&self) -> &[Field] {
        std::slice::from_raw_parts(self.fields_ptr(), self.fields_count())
    }

    /// Unsafe becaues it reaches past the class for variadic arguments.
    /// Can only be used on classes allocated within the managed heap.
    pub unsafe fn fields_mut(&mut self) -> &mut [Field] {
        std::slice::from_raw_parts_mut(self.fields_ptr_mut(), self.fields_count())
    }

    pub unsafe fn drop_instance(&self, instance: *mut ()) {
        // todo: assert instance class is self
        if let Some(drop) = self.drop {
            drop(instance);
        }
    }

    /// Unsafe becaues it reaches past the class for variadic arguments.
    /// Can only be used on classes allocated within the managed heap.
    pub unsafe fn fields_ptr(&self) -> *const Field {
        let ptr = self as *const Class;
        let fields_ptr = ptr.add(1) as *const Field;
        fields_ptr
    }

    /// Unsafe becaues it reaches past the class for variadic arguments.
    /// Can only be used on classes allocated within the managed heap.
    pub unsafe fn fields_ptr_mut(&mut self) -> *mut Field {
        let ptr = self as *mut Class;
        let fields_ptr = ptr.add(1) as *mut Field;
        fields_ptr
    }

    pub fn size_align(&self) -> (usize, usize) {
        (self.instance_size, self.instance_align)
    }

    /// Unsafe becaues it reaches to the header before the class.
    /// Can only be used on classes allocated within the managed heap.
    pub unsafe fn fields_count(&self) -> usize {
        let data_ptr = (self as *const _ as *const usize).sub(1);
        *data_ptr
    }

    pub fn array_element_type(&self) -> NonNullPtr<Class> {
        if let Kind::Array { ref element_type } = self.kind {
            return element_type.clone();
        }
        panic!("Class is not an array");
    }

    /// Unsafe becaues it reaches past the class for variadic arguments.
    /// Can only be used on classes allocated within the managed heap.
    pub unsafe fn read_field_indexed<T>(&self, instance: ErasedNonNull, index: usize) -> T {
        // todo: assert instance is this class
        let field = self.fields().index(index);
        let offset: isize = field.offset * size_of::<usize>() as isize;
        let field_ptr: *const T = (instance.get() as *const u8).offset(offset).cast();
        field_ptr.read()
    }

    /// Unsafe becaues it reaches past the class for variadic arguments.
    /// Can only be used on classes allocated within the managed heap.
    pub unsafe fn set_field_indexed<T>(&self, instance: ErasedNonNull, index: usize, val: T) {
        // todo: assert instance is this class
        let field = self.fields().index(index);
        // todo: special cases for pointers.
        // if field.is_reference then field size is size_of(usize) end
        let offset: isize = field.offset * size_of::<usize>() as isize;
        let field_ptr: *mut T = (instance.get() as *mut u8).offset(offset).cast();
        field_ptr.write(val)
    }

    pub fn is_reference_class(&self) -> bool {
        use Kind::*;
        match self.kind {
            Klass | Instance | Array { .. } => true,
            _ => false,
        }
    }

    pub fn array_element_size(&self) -> usize {
        let el_ty = self.array_element_type();
        unsafe {
            if (*el_ty.get()).is_reference_class() {
                size_of::<*mut ()>()
            } else {
                // non reference types cannot be variable sized
                (*el_ty.get()).instance_size
            }
        }
    }

    pub fn name(&self) -> &str {
        unsafe { (*self.name.get()).as_rust_str() }
    }
}

impl Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Class")
            .field("name", unsafe { &(*self.name.get()).as_rust_str() })
            .field("drop", &self.drop)
            .field("trace", &"<trace-function>")
            .field("instance_size", &self.instance_size)
            .field("instance_align", &self.instance_align)
            .field("kind", &self.kind)
            .finish()
    }
}

#[derive(Clone, Debug)]
pub enum Kind {
    Klass,
    Instance,
    Isize,
    Bool,
    Float64,
    Byte,
    Usize,
    Char,
    Array { element_type: NonNullPtr<Class> },
}

macro_rules! trivial_kind_predicate {
    ($v:vis $name:ident, $kind:ident) => {
        $v fn $name(&self) -> bool {
            if let Self::$kind = *self {
                true
            } else {
                false
            }
        }
    };
}

impl Kind {
    trivial_kind_predicate!(pub is_klass, Klass);
    trivial_kind_predicate!(pub is_instane, Instance);
    trivial_kind_predicate!(pub is_isize, Isize);
    trivial_kind_predicate!(pub is_bool, Bool);
    trivial_kind_predicate!(pub is_float64, Float64);
    trivial_kind_predicate!(pub is_byte, Byte);
    trivial_kind_predicate!(pub is_usize, Usize);
    trivial_kind_predicate!(pub is_char, Char);

    pub fn is_array(&self) -> bool {
        if let Self::Array { .. } = *self {
            true
        } else {
            false
        }
    }

    pub fn is_kind(&self, other: &Self) -> bool {
        use Kind::*;
        match (self, other) {
            (Klass, Klass) => true,
            (Isize, Isize) => true,
            (Float64, Float64) => true,
            (Bool, Bool) => true,
            (Array { .. }, Array { .. }) => true,
            (Instance, Instance) => true,
            _ => false,
        }
    }
}

pub unsafe fn instance_trace(obj: *mut (), tracer: &mut dyn Tracer) {
    let ptr = ErasedNonNull::new_unchecked(obj);
    let cls = ptr.class();
    let cls_forward = tracer.trace_nonnull_pointer(cls.as_untyped());
    let header = ptr.header();
    (*header.as_ptr()).class = NonNullPtr::new_unchecked(cls_forward as *mut Class);
    for field in (*cls.get()).fields().iter().filter(|f| f.reference) {
        let offset = field.offset;
        let field_ptr = (obj.map_addr(|addr| (addr as isize + offset) as usize)) as *mut ErasedPtr;
        let forward = tracer.trace_pointer((*field_ptr).clone());
        std::ptr::write(field_ptr, ErasedPtr::new(forward));
    }
}

pub unsafe fn class_trace(obj: *mut (), tracer: &mut dyn Tracer) {
    let this = obj as *mut Class;
    let name = tracer.trace_nonnull_pointer((*this).name.as_untyped());
    (*this).name = NonNullPtr::new_unchecked(name as *mut StringInst);
    let kind = (*this).kind.clone();
    match kind {
        Kind::Instance => {
            for field in (*this).fields_mut() {
                let forward = tracer.trace_nonnull_pointer(field.class.as_untyped());
                field.class = NonNullPtr::new_unchecked(forward as *mut Class);
                let name = tracer.trace_nonnull_pointer(field.name.as_untyped());
                field.name = NonNullPtr::new_unchecked(name as *mut StringInst);
            }
        }
        Kind::Array { element_type } => {
            let forward = tracer.trace_nonnull_pointer(element_type.as_untyped()) as *mut Class;
            (*this).kind = Kind::Array {
                element_type: NonNullPtr::new_unchecked(forward),
            };
        }
        // Other class kinds don't have anything to scan.
        _ => (),
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: NonNullPtr<StringInst>,
    // field offset in bytes
    pub offset: isize,
    pub class: NonNullPtr<Class>,
    pub reference: bool,
}
