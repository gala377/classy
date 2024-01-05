use std::collections::{BTreeSet, HashMap};

use crate::{
    clauses::Clause,
    database::UniverseIndex,
    fold::Folder,
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
    // Find method blocks and instances that contain given method
    FindMethod { name: String, on_type: Ty },
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
            DomainGoal::FindMethod { on_type, .. } => on_type.max_universe(labeling_function),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExClause {
    // main goal with the applied substitutions
    pub head: Goal,
    // subgoals that need to be satisfied in order to satisfy the main goal
    pub subgoals: Vec<Goal>,
    // mapping from variables to generics so that they can be unmapped
    // unmap[i] means that top scope generic with index i has been replaced
    // by the given type so unmap[i] is equal to Generic { 0, i }
    pub unmap: Vec<Ty>,
}

/// A cononical version of a Goal.
/// This is a normalized goal with all variables and all constants renumbered
/// from 0. Also universes are also renumbered starting from 0.
///
/// To retrieve the original goal, one needs to apply the inverse.
/// So every variable and constant needs to be replaced with the original one
/// TODO: How to obtain one?
/// And all universes need to increamented by the number of collapsed universes.
#[derive(Debug, Clone)]
pub struct CanonicalGoal {
    pub goal: Goal,
    pub collapsed_universes: usize,
    pub labeling_function: Vec<UniverseIndex>,
}

/// Used to unmap variables and constants from canonical form to their original
/// form to unamp one needs lookup the variable index from the canonical goal in
/// this map and will get the original variable index
pub type UnCanonMap = Vec<usize>;

impl CanonicalGoal {
    pub fn max_universe(&self) -> UniverseIndex {
        let mut max = UniverseIndex::ROOT;
        for index in &self.labeling_function {
            if *index > max {
                max = *index;
            }
        }
        max
    }

    pub fn new(goal: Goal, labeling: &dyn LabelingFunction) -> (Self, Vec<usize>) {
        let mut canonicalizer = Canonicalize::new(labeling);
        let res = canonicalizer.mk_canonical(&goal);
        (res, canonicalizer.reverse_variables_mapping)
    }
}

pub struct Canonicalize<'ctx> {
    universes: BTreeSet<UniverseIndex>,
    // mapping from old variables to new variables
    variables: HashMap<usize, usize>,
    // mapping from new variable indexes to old ones
    reverse_variables_mapping: Vec<usize>,
    next_variable: usize,
    labeling_function: &'ctx dyn LabelingFunction,
}

impl<'ctx> Canonicalize<'ctx> {
    pub fn new(labeling: &'ctx dyn LabelingFunction) -> Canonicalize {
        Canonicalize {
            universes: BTreeSet::new(),
            variables: HashMap::new(),
            reverse_variables_mapping: Vec::new(),
            next_variable: 0,
            labeling_function: labeling,
        }
    }

    pub fn mk_canonical(&mut self, goal: &Goal) -> CanonicalGoal {
        let canonical_goal = self.fold_goal(goal.clone());
        let min_universe = self
            .universes
            .iter()
            .min()
            .cloned()
            .unwrap_or(UniverseIndex::ROOT);
        let mut labeling = Vec::new();
        for var in &self.reverse_variables_mapping {
            let universe = self.labeling_function.check_variable(*var).unwrap();
            labeling.push(universe.collapse(min_universe));
        }
        CanonicalGoal {
            goal: canonical_goal,
            collapsed_universes: min_universe.as_usize(),
            labeling_function: labeling,
        }
    }
}

impl Folder for Canonicalize<'_> {
    fn fold_ty_variable(&mut self, idx: usize) -> Ty {
        if let Some(universe) = self.labeling_function.check_variable(idx) {
            self.universes.insert(universe);
        }
        if let Some(&var) = self.variables.get(&idx) {
            return Ty::Variable(var);
        }
        let var = self.next_variable;
        self.next_variable += 1;
        self.variables.insert(idx, var);
        self.reverse_variables_mapping.push(idx);
        Ty::Variable(var)
    }

    fn fold_ty_synthesized_constant(&mut self, idx: usize) -> Ty {
        if let Some(universe) = self.labeling_function.check_constant(idx) {
            self.universes.insert(universe);
        }
        if let Some(&var) = self.variables.get(&idx) {
            return Ty::SynthesizedConstant(var);
        }
        let var = self.next_variable;
        self.next_variable += 1;
        self.variables.insert(idx, var);
        self.reverse_variables_mapping.push(idx);
        Ty::SynthesizedConstant(idx)
    }
}
