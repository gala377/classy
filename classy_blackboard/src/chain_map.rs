use std::{collections::HashMap, hash::Hash};

pub struct ChainMap<K, V> {
    chain: Vec<HashMap<K, V>>,
}

impl<K: Hash + Eq, V> Default for ChainMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Hash + Eq, V> ChainMap<K, V> {
    pub fn new() -> Self {
        ChainMap {
            chain: vec![HashMap::new()],
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.chain.last_mut().unwrap().insert(key, value);
    }

    pub fn pop(&mut self) {
        self.chain.pop();
    }

    pub fn push(&mut self) {
        self.chain.push(HashMap::new());
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for map in self.chain.iter().rev() {
            if let Some(value) = map.get(key) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        for map in self.chain.iter_mut().rev() {
            if let Some(value) = map.get_mut(key) {
                return Some(value);
            }
        }
        None
    }

    pub fn contains_key(&self, key: &K) -> bool {
        for map in self.chain.iter().rev() {
            if map.contains_key(key) {
                return true;
            }
        }
        false
    }

    pub fn scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.push();
        let res = f(self);
        self.pop();
        res
    }
}
