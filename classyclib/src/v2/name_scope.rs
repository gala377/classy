use crate::scope::Scope;

use super::ty::Type;

pub struct NameScope {
    /// Variables that are defined in this scope
    variables: Scope<String, Type>,
    /// Types that are defined in this scope
    types: Scope<String, Type>,
}

impl NameScope {
    pub fn new() -> Self {
        Self {
            variables: Scope::with_empty_scope(),
            types: Scope::with_empty_scope(),
        }
    }

    pub fn new_scope(&mut self) {
        self.variables.new_scope();
        self.types.new_scope();
    }

    pub fn pop_scope(&mut self) {
        self.variables.pop_scope();
        self.types.pop_scope();
    }

    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.add(name, ty);
    }

    pub fn add_type(&mut self, name: String, ty: Type) {
        self.types.add(name, ty);
    }

    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }
}
