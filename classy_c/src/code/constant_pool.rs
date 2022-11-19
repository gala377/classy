use thiserror::Error;

use crate::code::Word;

#[derive(Error, Debug)]
#[error("Constant pool entry had unexpected type {actual:?}. Expected type {expected:?}")]
pub struct TypeMismatched {
    expected: EntryType,
    actual: EntryType,
}

/// Vector of type erased values paired with their
/// type information. Used to store constant information
/// for the execution.
pub struct ConstantPool {
    entries: Vec<Entry>,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn add_entry(&mut self, entry: TypedEntry) {
        // SAFETY: All transmutes are used only for the type erasure.
        unsafe {
            match entry {
                TypedEntry::Int(val) => self.entries.push(Entry {
                    typ: EntryType::Int,
                    val: std::mem::transmute(val),
                }),
                TypedEntry::Float(val) => self.entries.push(Entry {
                    typ: EntryType::Float,
                    val: std::mem::transmute(val),
                }),
            }
        }
    }

    pub unsafe fn get_unchecked<T>(&self, index: usize) -> T {
        assert_eq!(
            std::mem::size_of::<T>(),
            std::mem::size_of::<Word>(),
            "cannot get constant of type {} as it is not the same size as Word",
            std::any::type_name::<T>()
        );
        std::mem::transmute_copy(&self.entries[index].val)
    }

    pub fn get<T>(&self, typ: EntryType, index: usize) -> Result<T, TypeMismatched> {
        let actual_typ = self.entries[index].typ;
        if actual_typ != typ {
            Err(TypeMismatched {
                expected: typ,
                actual: actual_typ,
            })
        } else {
            // SAFETY: safe, we checked the entry's type.
            Ok(unsafe { self.get_unchecked(index) })
        }
    }
}

/// Constant pool's entry.
///
/// Each entry can only hold a word-sized value and should be Copy.
/// Values held in the entry are typed erased and have to be
/// bit-casted to retrieve the original value back.
/// The `typ` field is there to hold type information of the
/// hold value to make sure the value is properly casted.
///
/// If the value in the constant pool would be larger than a word then
/// one can allocate it in some memory (for example heap) and store
/// a pointer to it. However raw pointers are not owned so a care has
/// to be put not to dereference freed memory and to free the memory
/// when it is no longer needed.
pub struct Entry {
    /// Type of the entry. One could want to
    /// retrieve f64 from isize. To prevent
    /// that this field should be checked when
    /// bit-casting val.
    typ: EntryType,
    /// Type erased value.
    val: Word,
}

/// Type of the entry in the constant pool.
/// Used for validation.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum EntryType {
    Int,
    Float,
}

/// A type safe wrapped around `EntryType`.
/// Used in public, safe interfaces of the constant pool.
///
/// If one wants to omit checks for speed there are unsafe
/// iterfaces using EntryType directly.
///
/// This type also provides `From` implementations for basic types
/// to create this enum without caring about its inner representation.
#[derive(Clone)]
pub enum TypedEntry {
    Int(isize),
    Float(f64),
}

impl From<isize> for TypedEntry {
    fn from(val: isize) -> Self {
        Self::Int(val)
    }
}

impl From<f64> for TypedEntry {
    fn from(val: f64) -> Self {
        Self::Float(val)
    }
}

/// Convienience trait to retrieve type tag
/// for the constant pool entry of some generic T: Tagged.
pub trait Tagged {
    fn tag() -> EntryType;
}

impl Tagged for isize {
    fn tag() -> EntryType {
        EntryType::Int
    }
}

impl Tagged for f64 {
    fn tag() -> EntryType {
        EntryType::Float
    }
}
