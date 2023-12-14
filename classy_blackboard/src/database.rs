use std::collections::HashMap;

use crate::{
    clauses::{Instance, TypeImpl},
    goal::{ExClause, Goal},
    slg::Substitution,
    ty::{Constraint, Ty, TyRef},
};

pub struct Database {
    type_impls: Vec<TypeImpl>,
    instances: Vec<Instance>,
    names_to_ty: HashMap<String, usize>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            type_impls: Vec::new(),
            instances: Vec::new(),
            names_to_ty: HashMap::new(),
        }
    }

    pub fn add_type_impl(&mut self, type_impl: TypeImpl) -> TyRef {
        self.names_to_ty
            .insert(type_impl.name.clone(), self.type_impls.len());
        self.type_impls.push(type_impl);
        TyRef(self.type_impls.len() - 1)
    }

    pub fn add_struct(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
        members: Vec<(String, Ty)>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: false,
            members,
        };
        self.add_type_impl(type_impl)
    }

    pub fn add_type_class(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
        members: Vec<(String, Ty)>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: true,
            members,
        };
        self.add_type_impl(type_impl)
    }

    pub fn add_instance_for(
        &mut self,
        type_class: TyRef,
        type_params: Vec<String>,
        constraints: Vec<Constraint>,
        args: Vec<Ty>,
    ) {
        let instance = Instance {
            type_class,

            type_params,
            args,
            constraints,
        };
        self.add_instance(instance);
    }

    pub fn add_instance(&mut self, instance: Instance) {
        self.instances.push(instance);
    }

    pub fn typeref_from_name(&self, name: &str) -> Option<TyRef> {
        self.names_to_ty.get(name).map(|i| TyRef(*i))
    }

    pub fn find_matching_type(&self, _goal: &Goal) -> Vec<(ExClause, Substitution)> {
        unimplemented!()
    }

    pub fn find_matching_instance(&self, _goal: &Goal) -> Vec<(ExClause, Substitution)> {
        unimplemented!()
    }

    /// Union given goal with all clauses and return
    /// resulting exclauses alongside a substitution that has been made in order
    /// to make the union possible.
    pub fn find_matching(&self, _goal: &Goal) -> Vec<(ExClause, Substitution)> {
        unimplemented!()
    }
}
