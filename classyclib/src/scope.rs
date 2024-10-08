use std::{borrow::Borrow, collections::HashMap, hash::Hash};

pub type FlatScope<T> = Vec<Vec<T>>;

pub trait FlatScopeExt<T> {
    fn map_all<R>(&self, f: impl FnMut(&T) -> R) -> FlatScope<R>;

    fn new_scope(&mut self);

    fn add(&mut self, value: T);

    fn last_scope_mut(&mut self) -> Option<&mut Vec<T>>;
}

impl<T> FlatScopeExt<T> for FlatScope<T> {
    fn map_all<R>(&self, mut f: impl FnMut(&T) -> R) -> FlatScope<R> {
        self.iter()
            .map(|scope| scope.iter().map(|t| f(t)).collect())
            .collect()
    }

    fn new_scope(&mut self) {
        self.push(Vec::new());
    }

    fn add(&mut self, value: T) {
        self.last_mut().unwrap().push(value);
    }

    fn last_scope_mut(&mut self) -> Option<&mut Vec<T>> {
        self.last_mut()
    }
}

pub struct Scope<K, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> Default for Scope<K, V>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        Self::with_empty_scope()
    }
}

impl<K, V> Scope<K, V>
where
    K: Eq + Hash,
{
    pub fn with_empty_scope() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    pub fn without_scope() -> Self {
        Self { stack: Vec::new() }
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

    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_with_position(&self, key: &K) -> Option<(usize, &V)> {
        for (i, scope) in self.stack.iter().rev().enumerate() {
            if let Some(value) = scope.get(key) {
                return Some((i, value));
            }
        }
        None
    }

    pub fn position(&self, key: &K) -> Option<usize> {
        for (i, scope) in self.stack.iter().rev().enumerate() {
            if scope.contains_key(key) {
                return Some(i);
            }
        }
        None
    }

    pub fn with_scope<F, R>(&mut self, f: F) -> R
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

    pub fn contains(&self, key: &K) -> bool {
        self.stack.iter().any(|s| s.contains_key(key))
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.stack.iter().rev().flat_map(|s| s.iter())
    }

    /// Iterate scopes from the innermost to the outermost
    pub fn iter_scopes_in_to_out(&self) -> impl Iterator<Item = &HashMap<K, V>> {
        self.stack.iter().rev()
    }

    /// Iterate scopes from the outermost to the innermost
    pub fn iter_scopes_out_to_in(&self) -> impl Iterator<Item = &HashMap<K, V>> {
        self.stack.iter()
    }
}
