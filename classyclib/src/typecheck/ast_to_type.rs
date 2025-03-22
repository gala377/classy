use crate::scope::Scope;

pub type PrefexScope = Scope<String, usize>;

impl PrefexScope {
    pub fn add_type_var(&mut self, name: impl Into<String>) {
        self.add(name.into(), self.curr_scope_len());
    }

    pub fn add_type_vars(&mut self, vars: &[String]) {
        for var in vars {
            self.add_type_var(var);
        }
    }
}
