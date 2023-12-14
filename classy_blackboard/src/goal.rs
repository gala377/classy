use crate::{clauses::Clause, ty::Ty};

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Goal {
    /// forall<T> { if (Show(T)) { Show(Vec(T)) } }
    Implies(Vec<Clause>, Box<Goal>),
    /// exists<T> { Foo(T) :- Show(T) }
    Exists(Vec<String>, Box<Goal>),
    /// forall<T> { Foo(T) }
    Forall(Vec<String>, Box<Goal>),
    /// Can express any other goal after it has been canonilized
    Atom(Ty),
}

#[derive(Debug, Clone)]
pub struct ExClause {
    // main goal with the applied substitutions
    pub head: Goal,
    // subgoals that need to be satisfied in order to satisfy the main goal
    pub subgoals: Vec<Goal>,
}
