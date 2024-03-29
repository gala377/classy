#![allow(unstable_name_collisions)]
pub mod array;
pub mod byte;
pub mod code;
pub mod frame;
pub mod generic;
pub mod header;
pub mod integer;
pub mod klass;
pub mod string;
pub mod tuple;

use std::{fmt::Debug, mem::size_of, ops::Index};

use sptr::Strict;

use crate::{
    mem::ptr::{ErasedNonNull, ErasedPtr, NonNullPtr, Ptr},
    runtime::{class::string::StringInst, header::Header, trace::Tracer},
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
    pub name: Ptr<StringInst>,
    pub drop: Option<unsafe fn(*mut ())>,
    pub trace: unsafe fn(*mut (), &mut dyn Tracer),
    pub instance_size: usize,
    pub instance_align: usize,
    pub actual_instance_size: Option<unsafe fn(*const ()) -> usize>,
    pub kind: Kind,
    // Variable length sequence of fields
}

impl Class {
    pub fn size_align(&self) -> (usize, usize) {
        (self.instance_size, self.instance_align)
    }

    pub fn is_reference_class(&self) -> bool {
        use Kind::*;
        matches!(self.kind, Klass | Instance | Array { .. })
    }

    pub fn is_array_of_ref(&self) -> bool {
        use Kind::*;
        match self.kind {
            Array { is_ref, .. } => is_ref,
            _ => false,
        }
    }

    pub fn array_element_size(&self) -> usize {
        match self.kind {
            Kind::Array { element_size, .. } => element_size,
            _ => panic!("not an array class"),
        }
    }

    pub fn name(&self) -> &str {
        match self.name {
            Ptr(Some(inner)) => unsafe { (*inner.as_ptr()).as_rust_str() },
            Ptr(None) => "<null string ptr>",
        }
    }
}

/// # Safety
///
/// `instance` has to be na instance of the class `class`
/// Has to be a free standing function because Klass drops itself
pub unsafe fn drop_instance(class: NonNullPtr<Class>, instance: *mut ()) {
    let drop = (*class.get()).drop;
    // todo: assert instance class is self
    if let Some(drop) = drop {
        drop(instance);
    }
}

/// # Safety
///
/// Unsafe because it reaches past the class for variadic arguments.
/// Can only be used on classes allocated within the managed heap.
pub unsafe fn fields<'a>(this: NonNullPtr<Class>) -> &'a [Field] {
    std::slice::from_raw_parts(fields_ptr(this), fields_count(this))
}

/// # Safety
///
/// Unsafe because it reaches past the class for variadic arguments.
/// Can only be used on classes allocated within the managed heap.
pub unsafe fn fields_mut<'a>(this: NonNullPtr<Class>) -> &'a mut [Field] {
    std::slice::from_raw_parts_mut(fields_ptr_mut(this), fields_count(this))
}

/// # Safety
///
/// Unsafe becaues it reaches past the class for variadic arguments.
/// Can only be used on classes allocated within the managed heap.
pub unsafe fn fields_ptr(this: NonNullPtr<Class>) -> *const Field {
    let ptr = this.get();

    ptr.add(1) as *const Field
}

/// # Safety
///
/// Unsafe becaues it reaches past the class for variadic arguments.
/// Can only be used on classes allocated within the managed heap.
pub unsafe fn fields_ptr_mut(this: NonNullPtr<Class>) -> *mut Field {
    let ptr = this.get();

    ptr.add(1) as *mut Field
}

/// # Safety
///
/// Unsafe becaues it reaches past the class for variadic arguments.
/// Can only be used on classes allocated within the managed heap.
pub unsafe fn read_field_indexed<T>(
    this: NonNullPtr<Class>,
    instance: ErasedNonNull,
    index: usize,
) -> T {
    // todo: assert instance is this class
    let field = fields(this).index(index);
    let offset: isize = field.offset * size_of::<usize>() as isize;
    let field_ptr: *const T = (instance.get() as *const u8).offset(offset).cast();
    field_ptr.read()
}

/// # Safety
///
/// Unsafe becaues it reaches past the class for variadic arguments.
/// Can only be used on classes allocated within the managed heap.
pub unsafe fn set_field_indexed<T>(
    this: NonNullPtr<Class>,
    instance: ErasedNonNull,
    index: usize,
    val: T,
) {
    // todo: assert instance is this class
    let field = fields(this).index(index);
    // todo: special cases for pointers.
    // if field.is_reference then field size is size_of(usize) end
    let offset: isize = field.offset * size_of::<usize>() as isize;
    let field_ptr: *mut T = (instance.get() as *mut u8).offset(offset).cast();
    field_ptr.write(val)
}

/// # Safety
///
/// Unsafe becaues it reaches to the header before the class.
/// Can only be used on classes allocated within the managed heap.
pub unsafe fn fields_count(this: NonNullPtr<Class>) -> usize {
    let data_ptr = (this.get() as *const usize).sub(1);
    *data_ptr
}

impl Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name())
            .field("drop", &self.drop)
            .field("trace", &"<trace-function>")
            .field("instance_size", &self.instance_size)
            .field("instance_align", &self.instance_align)
            .field("kind", &self.kind)
            .finish()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Kind {
    Klass,
    Instance,
    Isize,
    Bool,
    Float64,
    Byte,
    Usize,
    Char,
    Array {
        element_size: usize,
        element_align: usize,
        is_ref: bool,
    },
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
        matches!(*self, Self::Array { .. })
    }

    pub fn is_kind(&self, other: &Self) -> bool {
        use Kind::*;
        matches!(
            (self, other),
            (Klass, Klass)
                | (Isize, Isize)
                | (Float64, Float64)
                | (Bool, Bool)
                | (Array { .. }, Array { .. })
                | (Instance, Instance)
        )
    }
}

/// # Safety
///
/// Passed `obj` pointer has to be an instance object.
pub unsafe fn instance_trace(obj: *mut (), tracer: &mut dyn Tracer) {
    let ptr = ErasedNonNull::new_unchecked(obj);
    let cls = ptr.class();
    let cls_forward = tracer.trace_nonnull_pointer(cls.as_untyped());
    let header = ptr.header();
    (*header.as_ptr()).class = NonNullPtr::new_unchecked(cls_forward as *mut Class);
    for field in fields(cls).iter().filter(|f| f.reference) {
        let offset = field.offset;
        let field_ptr = (obj.map_addr(|addr| (addr as isize + offset) as usize)) as *mut ErasedPtr;
        let forward = tracer.trace_pointer(*field_ptr);
        std::ptr::write(field_ptr, ErasedPtr::new(forward));
    }
}

/// # Safety
///
/// Passed `obj` pointer has to be a class object.
pub unsafe fn class_trace(obj: *mut (), tracer: &mut dyn Tracer) {
    let this = obj as *mut Class;
    let name = tracer.trace_pointer((*this).name.erase());
    (*this).name = Ptr::new(name as *mut StringInst);
    let kind = (*this).kind;
    let this_nonnull = NonNullPtr::new_unchecked(this);
    if let Kind::Instance = kind {
        for field in fields_mut(this_nonnull) {
            let forward = tracer.trace_nonnull_pointer(field.class.as_untyped());
            field.class = NonNullPtr::new_unchecked(forward as *mut Class);
            let name = tracer.trace_nonnull_pointer(field.name.as_untyped());
            field.name = NonNullPtr::new_unchecked(name as *mut StringInst);
        }
    }
}

unsafe fn drop_class(cls: *mut ()) {
    let cls = cls as *mut Class;
    let cls_nonnull = NonNullPtr::new_unchecked(cls);
    let mut fields_ptr = fields_ptr_mut(cls_nonnull);
    let fields_count = fields_count(cls_nonnull);
    std::ptr::drop_in_place(cls);
    for _ in 0..fields_count {
        std::ptr::drop_in_place(fields_ptr);
        fields_ptr = fields_ptr.offset(1);
    }
}

unsafe fn actual_class_size(cls: *const ()) -> usize {
    let header = (cls as *const Header).sub(1);
    let fields_count = (*header).data;
    size_of::<Class>() + fields_count * size_of::<Field>()
}

/// # Safety
/// Actually totally safe, it does nothing
pub unsafe fn trace_none(_obj: *mut (), _tracer: &mut dyn Tracer) {}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: NonNullPtr<StringInst>,
    // field offset in bytes
    pub offset: isize,
    pub class: NonNullPtr<Class>,
    pub reference: bool,
}
