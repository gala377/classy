use std::collections::HashMap;

use crate::{
    clauses::Clause,
    database::UniverseIndex,
    ty::{ClassRef, Ty, TyRef},
};

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Goal {
    /// forall<T> { if (Show(T)) { Show(Vec(T)) } }
    Implies(Vec<Clause>, Box<Goal>),
    /// exists<T> { Foo(T) :- Show(T) }
    Exists(Vec<String>, Box<Goal>),
    /// forall<T> { Foo(T) }
    Forall(Vec<String>, Box<Goal>),
    /// Leaf node for goals, implemented by the user
    Domain(DomainGoal),
}

pub trait LabelingFunction {
    fn check_variable(&self, variable: usize) -> Option<UniverseIndex>;
    fn check_constant(&self, constant: usize) -> Option<UniverseIndex>;
    fn add_variable(&mut self, variable: usize, universe: UniverseIndex);
    fn add_constant(&mut self, constant: usize, universe: UniverseIndex);
}

impl Goal {
    pub fn max_universe(&self, labeling_function: &dyn LabelingFunction) -> UniverseIndex {
        match self {
            Goal::Implies(_, goal) => goal.max_universe(labeling_function),
            Goal::Domain(domain_goal) => domain_goal.max_universe(labeling_function),
            _ => UniverseIndex::ROOT,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum DomainGoal {
    // Does given instance exist and is well formed
    InstanceExistsAndWellFormed { head: ClassRef, args: Vec<Ty> },
    // Is there methods block for given type
    MethodBlockExists { on_type: Ty },
    // Is given class well formed
    ClassExists(ClassRef),
    // Type implementation exists
    TypeImplExists(TyRef),
    // Type well formed
    TypeWellFormed { ty: Ty },
    // Class well formed
    ClassWellFormed { head: ClassRef, args: Vec<Ty> },
}

impl DomainGoal {
    pub fn max_universe(&self, labeling_function: &dyn LabelingFunction) -> UniverseIndex {
        match self {
            DomainGoal::InstanceExistsAndWellFormed { args, .. } => args
                .iter()
                .map(|ty| ty.max_universe(labeling_function))
                .max()
                .unwrap(),
            DomainGoal::MethodBlockExists { on_type } => on_type.max_universe(labeling_function),
            DomainGoal::ClassExists(..) => UniverseIndex::ROOT,
            DomainGoal::TypeImplExists(..) => UniverseIndex::ROOT,
            DomainGoal::TypeWellFormed { ty } => ty.max_universe(labeling_function),
            DomainGoal::ClassWellFormed { args, .. } => args
                .iter()
                .map(|ty| ty.max_universe(labeling_function))
                .max()
                .unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExClause {
    // main goal with the applied substitutions
    pub head: Goal,
    // subgoals that need to be satisfied in order to satisfy the main goal
    pub subgoals: Vec<Goal>,
}

impl Goal {
    pub fn substitute_generics(&self, substitution: &HashMap<String, Ty>) -> Goal {
        match self {
            Goal::Implies(clauses, goal) => Goal::Implies(
                clauses
                    .iter()
                    .map(|clause| clause.substitute_generics(substitution))
                    .collect(),
                Box::new(goal.substitute_generics(substitution)),
            ),
            Goal::Exists(generics, goal) => {
                let mut substitution = substitution.clone();
                for generic in generics {
                    substitution.remove(generic);
                }
                Goal::Exists(
                    generics.clone(),
                    Box::new(goal.substitute_generics(&substitution)),
                )
            }
            Goal::Forall(generics, goal) => {
                let mut substitution = substitution.clone();
                for generic in generics {
                    substitution.remove(generic);
                }
                Goal::Forall(
                    generics.clone(),
                    Box::new(goal.substitute_generics(&substitution)),
                )
            }
            Goal::Domain(domain_goal) => {
                Goal::Domain(domain_goal.substitute_generics(substitution))
            }
        }
    }
}

impl DomainGoal {
    pub fn substitute_generics(&self, substitution: &HashMap<String, Ty>) -> DomainGoal {
        match self {
            DomainGoal::InstanceExistsAndWellFormed { head, args } => {
                DomainGoal::InstanceExistsAndWellFormed {
                    head: head.clone(),
                    args: args
                        .iter()
                        .map(|ty| ty.substitute_generics(substitution))
                        .collect(),
                }
            }
            DomainGoal::MethodBlockExists { on_type } => DomainGoal::MethodBlockExists {
                on_type: on_type.substitute_generics(substitution),
            },
            DomainGoal::ClassExists(class_ref) => DomainGoal::ClassExists(class_ref.clone()),
            DomainGoal::TypeImplExists(ty_ref) => DomainGoal::TypeImplExists(*ty_ref),
            DomainGoal::TypeWellFormed { ty } => DomainGoal::TypeWellFormed {
                ty: ty.substitute_generics(substitution),
            },
            DomainGoal::ClassWellFormed { head, args } => DomainGoal::ClassWellFormed {
                head: head.clone(),
                args: args
                    .iter()
                    .map(|ty| ty.substitute_generics(substitution))
                    .collect(),
            },
        }
    }
}
