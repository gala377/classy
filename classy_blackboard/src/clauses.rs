use crate::goal::{DomainGoal, Goal};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Clause {
    /// Foo(Int)
    Fact(DomainGoal),
    /// Foo(Int) :- Bar(Int)
    Implies(Box<Clause>, Vec<Goal>),
    /// forall<T> { Foo(T) :- Bar(T) }
    /// The first usize is the number of variables introduced
    Forall(usize, Box<Clause>),
}
