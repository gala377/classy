use std::hash::Hash;

use thiserror::Error;

use crate::code::Word;

use super::interner::Interner;

#[derive(Error, Debug)]
#[error("Constant pool entry had unexpected type {actual:?}. Expected type {expected:?}")]
pub struct TypeMismatched {
    expected: EntryType,
    actual: EntryType,
}

const STRING_LEN_MASK: usize = 0xFFFFFFFF00000000;
const STRING_OFFSET_MASK: usize = 0x00000000FFFFFFFF;

/// Vector of type erased values paired with their
/// type information. Used to store constant information
/// for the execution.
pub struct ConstantPool {
    entries: Vec<Entry>,

    /// Byte vector containing all of the strings tightly packed.
    /// See documentation for EntryType on how to retrieve them.
    strings: Vec<u8>,

    interner: Interner,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            strings: Vec::new(),
            interner: Interner::new(),
        }
    }

    pub fn add_entry(&mut self, entry: TypedEntry) -> usize {
        if let Some(id) = self.interner.check(&entry) {
            return id;
        }
        // SAFETY: All transmutes are used only for the type erasure.
        unsafe {
            match &entry {
                TypedEntry::Bool(b) => self.entries.push(Entry {
                    typ: EntryType::Bool,
                    val: if *b { 1 } else { 0 },
                }),
                TypedEntry::Int(val) => self.entries.push(Entry {
                    typ: EntryType::Int,
                    val: std::mem::transmute_copy(val),
                }),
                TypedEntry::Float(val) => self.entries.push(Entry {
                    typ: EntryType::Float,
                    val: std::mem::transmute_copy(val),
                }),
                TypedEntry::String(val) => {
                    let offset = self.strings.len();
                    let len = val.as_bytes().len();
                    assert!(
                        offset <= std::u32::MAX as usize,
                        "string table cannot be larger than max of u32"
                    );
                    assert!(
                        len <= std::u32::MAX as usize,
                        "string literal cannot be longer than max of u32"
                    );
                    let string_word = offset | len << 32;
                    assert!(
                        string_word & STRING_OFFSET_MASK == offset,
                        "offset has been wrongly encoded"
                    );
                    assert!(
                        string_word & STRING_LEN_MASK == (len << 32),
                        "len has been wrongly encoded"
                    );
                    self.entries.push(Entry {
                        typ: EntryType::String,
                        val: string_word,
                    });
                    self.strings.extend_from_slice(val.as_bytes());
                }
            };
        };
        let id = self.entries.len() - 1;
        self.interner.add(&entry, id);
        id
    }

    pub unsafe fn get_unchecked<T>(&self, index: usize) -> T {
        assert_eq!(
            std::mem::size_of::<T>(),
            std::mem::size_of::<Word>(),
            "cannot get constant of type {} as it is not the same size as Word",
            std::any::type_name::<T>()
        );
        let entry_val = self.entries[index].val;
        std::mem::transmute_copy(&entry_val)
    }

    pub fn get<T: Tagged>(&self, index: usize) -> Result<T, TypeMismatched> {
        let actual_typ = self.entries[index].typ;
        if actual_typ != T::tag() {
            return Err(TypeMismatched {
                expected: T::tag(),
                actual: actual_typ,
            });
        }
        match T::tag() {
            EntryType::Float | EntryType::Int => {
                // SAFETY: safe, we checked the entry's type.
                Ok(unsafe { self.get_unchecked::<T>(index) })
            }
            EntryType::Bool => {
                Ok(T::from_usize(self.entries[index].val))
            }
            EntryType::String => {
                let string_word = self.entries[index].val;
                let offset = string_word & STRING_OFFSET_MASK;
                let len = (string_word & STRING_LEN_MASK) >> 32;
                // todo: we could return str reference instead of allocating a string here
                let string_val = String::from_utf8(self.strings[offset..offset + len].to_vec())
                    .expect("This string has not been encoded properly");
                Ok(T::from_string(string_val))
            }
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
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
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
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum EntryType {
    /// The value of this tag is the int itself.
    Int,
    /// The value of this tag is a float itself.
    Float,
    /// The word erased value of this entry type is an u32 offset
    /// into a string table followed by an u32 lenght of the string.
    String,
    /// The value of this tag is a bool extended to u64 but it will
    /// either have 1 or 0
    Bool,

}

/// A type safe wrapped around `EntryType`.
/// Used in public, safe interfaces of the constant pool.
///
/// If one wants to omit checks for speed there are unsafe
/// iterfaces using EntryType directly.
///
/// This type also provides `From` implementations for basic types
/// to create this enum without caring about its inner representation.
#[derive(Clone, Debug)]
pub enum TypedEntry {
    Int(isize),
    Float(f64),
    String(String),
    Bool(bool),
}

impl PartialEq for TypedEntry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            _ => false,
        }
    }
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

impl From<String> for TypedEntry {
    fn from(val: String) -> Self {
        Self::String(val)
    }
}

impl From<bool> for TypedEntry {
    fn from(val: bool) -> Self {
        Self::Bool(val)
    }
}

/// Convienience trait to retrieve type tag
/// for the constant pool entry of some generic T: Tagged.
pub trait Tagged: Sized {
    fn tag() -> EntryType;

    fn from_string(_: String) -> Self {
        panic!("This entry does not support converting from string")
    }
    fn from_usize(_: usize) -> Self {
        panic!("This entry does not support this conversion")
    }
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

impl Tagged for String {
    fn tag() -> EntryType {
        EntryType::String
    }

    fn from_string(s: String) -> Self {
        s
    }
}

impl Tagged for bool {
    fn tag() -> EntryType {
        EntryType::Bool
    }

    fn from_usize(val: usize) -> Self {
        if val == 0 {
            return false
        } else if val == 1 {
            return true
        }
        panic!("Illegal value of bool")
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_storing_and_retrieving_int_value() {
        let mut cp = ConstantPool::new();
        let id = cp.add_entry(TypedEntry::Int(10));
        let val = cp.get::<isize>(id).unwrap();
        assert_eq!(val, 10)
    }

    #[test]
    fn test_storing_flaot_value() {
        let mut cp = ConstantPool::new();
        let id = cp.add_entry(TypedEntry::Float(1.12));
        assert_eq!(cp.get::<f64>(id).unwrap(), 1.12);
    }

    #[test]
    fn test_storing_string_value() {
        let mut cp = ConstantPool::new();
        let id = cp.add_entry(TypedEntry::String("Hello".into()));
        assert_eq!(cp.get::<String>(id).unwrap(), "Hello");
    }
    #[test]
    fn test_storing_2_string_values() {
        let mut cp = ConstantPool::new();
        let id_1 = cp.add_entry(TypedEntry::String("Hello".into()));
        let id_2 = cp.add_entry(TypedEntry::String("World".into()));
        assert_eq!(cp.get::<String>(id_1).unwrap(), "Hello");
        assert_eq!(cp.get::<String>(id_2).unwrap(), "World");
    }
}
