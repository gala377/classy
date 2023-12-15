use std::collections::HashMap;

/// Reference to the database
/// that cointains type information
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Ty {
    /// Types refering to the type database
    Ref(TyRef),
    Array(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    App(TyRef, Vec<Ty>),
    /// An Existential variable used in queries. Will be substituted by the
    /// possible matches.
    Variable(usize),
    /// A Universal variable, will not be substituted. Is only well formed if
    /// there exists an instantiation for all possible types matching
    /// additional constraints.
    SynthesizedConstant(usize),
    /// Placeholder for generic types in clauses
    Generic(String),
}

/// A reference to a type into a
/// type table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstanceRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodBlockRef(pub usize);
/// A constraint on the given type
/// that has to be met
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Constraint {
    /// Type a has to be equal to type b
    Eq(Ty, Ty),
    /// Instance of a class needs to exists and be well formed
    Class(ClassRef, Vec<Ty>),
}

impl Ty {
    pub fn substitute_generics(&self, substitution: &HashMap<String, Ty>) -> Ty {
        match self {
            Ty::Ref(_) => self.clone(),
            Ty::Array(ty) => Ty::Array(Box::new(ty.substitute_generics(substitution))),
            Ty::Tuple(tys) => Ty::Tuple(
                tys.iter()
                    .map(|ty| ty.substitute_generics(substitution))
                    .collect(),
            ),
            Ty::Fn(args, ret) => Ty::Fn(
                args.iter()
                    .map(|ty| ty.substitute_generics(substitution))
                    .collect(),
                Box::new(ret.substitute_generics(substitution)),
            ),
            Ty::App(ty_ref, args) => Ty::App(
                *ty_ref,
                args.iter()
                    .map(|ty| ty.substitute_generics(substitution))
                    .collect(),
            ),
            Ty::Variable(_) => self.clone(),
            Ty::SynthesizedConstant(_) => self.clone(),
            Ty::Generic(name) => substitution.get(name).unwrap_or(self).clone(),
        }
    }
}
