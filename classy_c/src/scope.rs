use std::{collections::HashMap, hash::Hash};

pub struct Scope<K, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> Scope<K, V>
where
    K: Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    pub fn new_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.stack.pop();
    }

    pub fn add(&mut self, key: K, value: V) {
        self.stack.last_mut().unwrap().insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }
        None
    }

    pub fn position(&self, key: &K) -> Option<usize> {
        for (i, scope) in self.stack.iter().enumerate().rev() {
            if scope.contains_key(key) {
                return Some(i);
            }
        }
        None
    }

    pub fn with_scope<F, R>(&mut self, mut f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.new_scope();
        let r = f(self);
        self.pop_scope();
        r
    }

    pub fn is_empty(&self) -> bool {
        self.stack.iter().all(|s| s.is_empty())
    }

    pub fn curr_scope_len(&self) -> usize {
        self.stack.last().unwrap().len()
    }
}
