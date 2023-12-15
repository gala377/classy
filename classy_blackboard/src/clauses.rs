use std::collections::HashMap;

use crate::{
    goal::{DomainGoal, Goal},
    ty::Ty,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Clause {
    /// Foo(Int)
    Fact(DomainGoal),
    /// Foo(Int) :- Bar(Int)
    Implies(Box<Clause>, Vec<Goal>),
    /// forall<T> { Foo(T) :- Bar(T) }
    Forall(Vec<String>, Box<Clause>),
}

impl Clause {
    pub fn substitute_generics(&self, substitution: &HashMap<String, Ty>) -> Clause {
        match self {
            Clause::Fact(fact) => Clause::Fact(fact.substitute_generics(substitution)),
            Clause::Implies(head, subgoals) => Clause::Implies(
                Box::new(head.substitute_generics(substitution)),
                subgoals
                    .iter()
                    .map(|goal| goal.substitute_generics(substitution))
                    .collect(),
            ),
            Clause::Forall(generics, clause) => {
                let mut substitution = substitution.clone();
                for generic in generics {
                    substitution.remove(generic);
                }
                Clause::Forall(
                    generics.clone(),
                    Box::new(clause.substitute_generics(&substitution)),
                )
            }
        }
    }
}
