use crate::{
    goal::Goal,
    ty::{Constraint, Ty, TyRef},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Clause {
    /// Foo(Int)
    Fact(Ty),
    /// Foo(Int) :- Bar(Int)
    Implies(Box<Clause>, Vec<Goal>),
    /// forall<T> { Foo(T) :- Bar(T) }
    Forall(Vec<String>, Box<Clause>),
}

/// All type information necessary for inference
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeImpl {
    pub name: String,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
    pub members: Vec<(String, Ty)>,
    pub is_class: bool,
}

impl TypeImpl {
    pub fn as_goal(&self) -> Goal {
        todo!()
    }
}

/// there is an instance Show(Int)
/// For instance in form
/// instance for { Clone(a) } => Clone(Vec(a))
/// We would map it to
/// type_class: Clone
/// args: [ Vec(a) ]
/// type_params: [ a ]
/// constraints: [ Clone(a) ]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Instance {
    pub type_class: TyRef,
    pub args: Vec<Ty>,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
}
