use std::{collections::HashMap, hash::Hash};


use super::constant_pool::{self, TypedEntry};

struct HashableFloat(f64);

impl PartialEq for HashableFloat {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for HashableFloat {}

impl Hash for HashableFloat {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&self.0.to_le_bytes());
    }
}
#[derive(PartialEq, Eq, Hash)]
enum InternableEntry {
    String(String),
    Float(HashableFloat),
    Int(isize),
}

impl From<constant_pool::TypedEntry> for InternableEntry {
    fn from(value: constant_pool::TypedEntry) -> Self {
        match value {
            TypedEntry::String(s) => Self::String(s),
            TypedEntry::Int(i) => Self::Int(i),
            TypedEntry::Float(f) => Self::Float(HashableFloat(f)),
        }
    }
}

pub struct Interner {
    interned: HashMap<InternableEntry, usize>,
}


impl Interner {
    pub fn new() -> Self {
        Interner { interned: HashMap::new() }
    }

    pub fn check(&self, entry: &constant_pool::TypedEntry) -> Option<usize> {
        self.interned.get(&entry.clone().into()).map(|x| *x)
    }

    pub fn add(&mut self, entry: &constant_pool::TypedEntry, val: usize) {
        assert!(self.check(entry).is_none(), "already interned");
        self.interned.insert(entry.clone().into(), val);
    }
}

