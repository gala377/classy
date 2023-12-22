use crate::{
    clauses::Clause,
    database::UniverseIndex,
    ty::{ClassRef, Ty},
};

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Goal {
    /// forall<T> { if (Show(T)) { Show(Vec(T)) } }
    Implies(Vec<Clause>, Box<Goal>),
    /// exists<T> { Foo(T) :- Show(T) }
    /// first usize is number of variables introduced
    Exists(usize, Box<Goal>),
    /// forall<T> { Foo(T) }
    /// first usize is number of variables introduced
    Forall(usize, Box<Goal>),
    /// Leaf node for goals, implemented by the user
    Domain(DomainGoal),
}

pub trait LabelingFunction {
    fn check_variable(&self, variable: usize) -> Option<UniverseIndex>;
    fn check_constant(&self, constant: usize) -> Option<UniverseIndex>;
    fn add_variable(&mut self, variable: usize, universe: UniverseIndex);
    fn add_constant(&mut self, constant: usize, universe: UniverseIndex);
    fn adjust_universe(&mut self, var: usize, universe: UniverseIndex);
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
