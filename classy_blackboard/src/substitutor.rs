use std::collections::HashMap;

use crate::{fold::Folder, ty::Ty};

pub struct VariableSubstitutor<'a> {
    pub substitutions: &'a HashMap<usize, Ty>,
}

impl Folder for VariableSubstitutor<'_> {
    fn fold_ty_variable(&mut self, idx: usize) -> Ty {
        if let Some(ty) = self.substitutions.get(&idx) {
            ty.clone()
        } else {
            Ty::Variable(idx)
        }
    }
}
