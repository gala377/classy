use std::collections::HashMap;

use crate::clauses::Clause;
use crate::database::{UniverseIndex, VariableContext};
use crate::fold::{walk_clause, walk_goal, Folder};
use crate::goal::Goal;
use crate::ty::Ty;

/// Normalizes a clause into a canonical form.
///
/// What it means is that it strips any existental an universal quantifiers
/// and replaces them with fresh variables and constants. Meaning no generics
/// are left.
pub struct ClauseNormalizer<'ctx> {
    variable_context: &'ctx mut dyn VariableContext,
    substitutions: Vec<Vec<Ty>>,
    universe: UniverseIndex,
    pub initial_generics_subst: Option<Vec<Ty>>,
}

impl<'ctx> ClauseNormalizer<'ctx> {
    pub fn new(
        variable_context: &'ctx mut dyn VariableContext,
        universe: UniverseIndex,
    ) -> ClauseNormalizer<'ctx> {
        ClauseNormalizer {
            variable_context,
            substitutions: vec![],
            universe,
            initial_generics_subst: None,
        }
    }
}

impl Folder for ClauseNormalizer<'_> {
    fn fold_clause_forall(&mut self, generics: usize, clause: Clause) -> Clause {
        self.substitutions.push(
            (0..generics)
                .map(|_| self.variable_context.next_variable(self.universe))
                .collect(),
        );
        if self.initial_generics_subst.is_none() {
            self.initial_generics_subst = Some(self.substitutions.last().unwrap().clone());
        }
        let res = walk_clause(self, clause);
        self.substitutions.pop();
        res
    }

    fn fold_goal_exists(&mut self, vars: usize, goal: Goal) -> Goal {
        self.substitutions.push(
            (0..vars)
                .map(|_| self.variable_context.next_variable(self.universe))
                .collect(),
        );
        let res = walk_goal(self, goal);
        self.substitutions.pop();
        res
    }

    fn fold_goal_forall(&mut self, vars: usize, goal: Goal) -> Goal {
        self.substitutions.push(
            (0..vars)
                .map(|_| self.variable_context.next_constant(self.universe))
                .collect(),
        );
        let res = walk_goal(self, goal);
        self.substitutions.pop();
        res
    }

    fn fold_ty_generic(&mut self, scopes: usize, index: usize) -> Ty {
        let scope_index = self.substitutions.len() - 1 - scopes;
        self.substitutions[scope_index][index].clone()
    }
}

pub struct GoalNormalizer<'ctx> {
    variable_context: &'ctx mut dyn VariableContext,
    substitutions: Vec<Vec<Ty>>,
    universe: UniverseIndex,
}

impl<'ctx> GoalNormalizer<'ctx> {
    pub fn new(
        variable_context: &'ctx mut dyn VariableContext,
        universe: UniverseIndex,
    ) -> GoalNormalizer<'ctx> {
        GoalNormalizer {
            variable_context,
            substitutions: vec![],
            universe,
        }
    }
}

impl Folder for GoalNormalizer<'_> {
    fn fold_goal_forall(&mut self, generics: usize, goal: Goal) -> Goal {
        let last_universe = self.universe;
        self.substitutions.push(
            (0..generics)
                .map(|_| {
                    self.universe = self.universe.next();
                    self.variable_context.next_constant(self.universe)
                })
                .collect(),
        );
        let res = walk_goal(self, goal);
        self.substitutions.pop();
        self.universe = last_universe;
        res
    }
    fn fold_goal_exists(&mut self, vars: usize, goal: Goal) -> Goal {
        self.substitutions.push(
            (0..vars)
                .map(|_| self.variable_context.next_variable(self.universe))
                .collect(),
        );
        let res = walk_goal(self, goal);
        self.substitutions.pop();
        res
    }

    fn fold_goal_implies(&mut self, clauses: Vec<Clause>, goal: Goal) -> Goal {
        let clauses = clauses
            .into_iter()
            .map(|c| {
                let mut normalizer = AssumptionClauseNormalizer {
                    substitutions: &self.substitutions,
                    shift: 0,
                };
                normalizer.fold_clause(c)
            })
            .collect();
        let res = walk_goal(self, goal);
        Goal::Implies(clauses, Box::new(res))
    }

    fn fold_ty_generic(&mut self, scopes: usize, index: usize) -> Ty {
        let scope_index = self.substitutions.len() - 1 - scopes;
        self.substitutions[scope_index][index].clone()
    }
}

/// Only substitutes generics that have not been introduced within assumptions.
struct AssumptionClauseNormalizer<'subst> {
    pub substitutions: &'subst Vec<Vec<Ty>>,
    pub shift: usize,
}

impl Folder for AssumptionClauseNormalizer<'_> {
    fn fold_clause_forall(&mut self, generics: usize, clause: Clause) -> Clause {
        self.shift += 1;
        let res = walk_clause(self, clause);
        self.shift -= 1;
        Clause::Forall(generics, Box::new(res))
    }

    fn fold_goal_exists(&mut self, vars: usize, goal: Goal) -> Goal {
        self.shift += 1;
        let res = walk_goal(self, goal);
        self.shift -= 1;
        Goal::Exists(vars, Box::new(res))
    }

    fn fold_goal_forall(&mut self, vars: usize, goal: Goal) -> Goal {
        self.shift += 1;
        let res = walk_goal(self, goal);
        self.shift -= 1;
        Goal::Forall(vars, Box::new(res))
    }

    fn fold_ty_generic(&mut self, scopes: usize, index: usize) -> Ty {
        if scopes < self.shift {
            // Generic introduced within assumption
            return Ty::Generic { scopes, index };
        }
        let scopes = scopes - self.shift;
        let scope_index = self.substitutions.len() - 1 - scopes;
        self.substitutions[scope_index][index].clone()
    }
}

#[cfg(test)]
mod tests {

    use super::ClauseNormalizer;
    use crate::clauses::Clause;
    use crate::database::{UniverseIndex, VariableContext};
    use crate::fold::Folder;
    use crate::goal::{DomainGoal, Goal, LabelingFunction};
    use crate::normalizer::GoalNormalizer;
    use crate::ty::{Ty, TyRef};

    pub struct MockLabelingFunction;

    impl LabelingFunction for MockLabelingFunction {
        fn check_variable(&self, _variable: usize) -> Option<UniverseIndex> {
            None
        }

        fn check_constant(&self, _constant: usize) -> Option<UniverseIndex> {
            None
        }
        fn add_variable(&mut self, _variable: usize, _universe: UniverseIndex) {}
        fn add_constant(&mut self, _constant: usize, _universe: UniverseIndex) {}
        fn adjust_universe(&mut self, _var: usize, _universe: UniverseIndex) {}
    }

    pub struct TestVarContext {
        pub next: usize,
        pub lfunc: MockLabelingFunction,
    }

    impl TestVarContext {
        pub fn new() -> TestVarContext {
            TestVarContext {
                next: 0,
                lfunc: MockLabelingFunction,
            }
        }
    }

    impl VariableContext for TestVarContext {
        fn next_variable(&mut self, _in_universe: UniverseIndex) -> Ty {
            let res = Ty::Variable(self.next);
            self.next += 1;
            res
        }

        fn next_constant(&mut self, _in_universe: UniverseIndex) -> Ty {
            let res = Ty::SynthesizedConstant(self.next);
            self.next += 1;
            res
        }

        fn labeling_function(&mut self) -> &mut dyn crate::goal::LabelingFunction {
            &mut self.lfunc
        }
    }

    #[test]
    fn clause_fact() {
        let mut variable_context = TestVarContext::new();
        let mut clause_normalizer =
            ClauseNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let clause = Clause::Fact(DomainGoal::TypeWellFormed {
            ty: Ty::Ref(TyRef(0)),
        });

        let normalized = clause_normalizer.fold_clause(clause.clone());
        assert_eq!(normalized, clause);
    }

    #[test]
    fn clause_forall_fact() {
        let mut variable_context = TestVarContext::new();
        let mut clause_normalizer =
            ClauseNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let clause = Clause::Forall(
            1,
            Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                ty: Ty::Generic {
                    scopes: 0,
                    index: 0,
                },
            })),
        );

        let normalized = clause_normalizer.fold_clause(clause);
        assert_eq!(
            normalized,
            Clause::Fact(DomainGoal::TypeWellFormed {
                ty: Ty::Variable(0)
            })
        );
    }

    #[test]
    fn clause_forall_implies() {
        let mut variable_context = TestVarContext::new();
        let mut clause_normalizer =
            ClauseNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let clause = Clause::Forall(
            1,
            Box::new(Clause::Implies(
                Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::Generic {
                        scopes: 0,
                        index: 0,
                    },
                })),
                vec![Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::Generic {
                        scopes: 0,
                        index: 0,
                    },
                })],
            )),
        );

        let normalized = clause_normalizer.fold_clause(clause);
        assert_eq!(
            normalized,
            Clause::Implies(
                Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::Variable(0)
                })),
                vec![Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::Variable(0)
                })]
            )
        );
    }

    #[test]
    fn clause_nested_forall() {
        let mut variable_context = TestVarContext::new();
        let mut clause_normalizer =
            ClauseNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let clause = Clause::Forall(
            1,
            Box::new(Clause::Implies(
                Box::new(Clause::Forall(
                    1,
                    Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                        ty: Ty::Generic {
                            scopes: 0,
                            index: 0,
                        },
                    })),
                )),
                vec![Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::Generic {
                        scopes: 0,
                        index: 0,
                    },
                })],
            )),
        );

        let normalized = clause_normalizer.fold_clause(clause);
        assert_eq!(
            normalized,
            Clause::Implies(
                Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::Variable(1)
                })),
                vec![Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::Variable(0)
                })]
            )
        );
    }

    #[test]
    fn clause_forall_exists_in_goal() {
        let mut variable_context = TestVarContext::new();
        let mut clause_normalizer =
            ClauseNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let clause = Clause::Forall(
            1,
            Box::new(Clause::Implies(
                Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::Generic {
                        scopes: 0,
                        index: 0,
                    },
                })),
                vec![
                    Goal::Exists(
                        1,
                        Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                            ty: Ty::Generic {
                                scopes: 0,
                                index: 0,
                            },
                        })),
                    ),
                    Goal::Forall(
                        1,
                        Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                            ty: Ty::Generic {
                                scopes: 0,
                                index: 0,
                            },
                        })),
                    ),
                ],
            )),
        );

        let normalized = clause_normalizer.fold_clause(clause);
        assert_eq!(
            normalized,
            Clause::Implies(
                Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::Variable(0)
                })),
                vec![
                    Goal::Domain(DomainGoal::TypeWellFormed {
                        ty: Ty::Variable(1)
                    }),
                    Goal::Domain(DomainGoal::TypeWellFormed {
                        ty: Ty::SynthesizedConstant(2)
                    })
                ]
            )
        );
    }

    #[test]
    fn goal_exists() {
        let mut variable_context = TestVarContext::new();
        let mut goal_normalizer = GoalNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let goal = Goal::Exists(
            1,
            Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                ty: Ty::Generic {
                    scopes: 0,
                    index: 0,
                },
            })),
        );

        let normalized = goal_normalizer.fold_goal(goal);
        assert_eq!(
            normalized,
            Goal::Domain(DomainGoal::TypeWellFormed {
                ty: Ty::Variable(0)
            })
        );
    }

    #[test]
    fn goal_forall() {
        let mut variable_context = TestVarContext::new();
        let mut goal_normalizer = GoalNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let goal = Goal::Forall(
            1,
            Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                ty: Ty::Generic {
                    scopes: 0,
                    index: 0,
                },
            })),
        );

        let normalized = goal_normalizer.fold_goal(goal);
        assert_eq!(
            normalized,
            Goal::Domain(DomainGoal::TypeWellFormed {
                ty: Ty::SynthesizedConstant(0)
            })
        );
    }

    #[test]
    fn goal_implies() {
        let mut variable_context = TestVarContext::new();
        let mut goal_normalizer = GoalNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let goal = Goal::Forall(
            1,
            Box::new(Goal::Implies(
                vec![Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::Generic {
                        scopes: 0,
                        index: 0,
                    },
                })],
                Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::Generic {
                        scopes: 0,
                        index: 0,
                    },
                })),
            )),
        );

        let normalized = goal_normalizer.fold_goal(goal);
        assert_eq!(
            normalized,
            Goal::Implies(
                vec![Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::SynthesizedConstant(0)
                })],
                Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::SynthesizedConstant(0)
                }))
            )
        );
    }

    #[test]
    fn goal_implies_clause_forall() {
        let mut variable_context = TestVarContext::new();
        let mut goal_normalizer = GoalNormalizer::new(&mut variable_context, UniverseIndex::ROOT);
        let goal = Goal::Exists(
            1,
            Box::new(Goal::Implies(
                vec![
                    Clause::Forall(
                        1,
                        Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                            ty: Ty::Generic {
                                scopes: 0,
                                index: 0,
                            },
                        })),
                    ),
                    Clause::Forall(
                        1,
                        Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                            ty: Ty::Generic {
                                scopes: 1,
                                index: 0,
                            },
                        })),
                    ),
                ],
                Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::Generic {
                        scopes: 0,
                        index: 0,
                    },
                })),
            )),
        );

        let normalized = goal_normalizer.fold_goal(goal);
        assert_eq!(
            normalized,
            Goal::Implies(
                vec![
                    Clause::Forall(
                        1,
                        Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                            ty: Ty::Generic {
                                scopes: 0,
                                index: 0
                            }
                        }))
                    ),
                    Clause::Forall(
                        1,
                        Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                            ty: Ty::Variable(0)
                        }))
                    )
                ],
                Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: Ty::Variable(0)
                }))
            )
        );
    }
}
