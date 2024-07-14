use std::collections::{HashMap, VecDeque};

use tracing::{debug, info, warn};

use crate::{
    database::{AnswerOrigin, Database, GenericRef, MatchResult, UniverseIndex, VariableContext},
    fold::Folder,
    goal::{CanonicalGoal, ExClause, Goal, LabelingFunction, UnCanonMap},
    normalizer::GoalNormalizer,
    substitutor::VariableSubstitutor,
    ty::Ty,
};

pub struct Forest {
    /// All of the instantiated tables
    /// This is a mapping where tables[table.table_index] == table
    tables: Vec<Table>,
    /// A mapping from a goal to a table index of a table instantiated for that
    /// goal.
    ///
    /// So given a goal we this holds:
    /// tables[table_goals[goal]].goal == goal
    table_goals: HashMap<Goal, usize>,
}

impl Forest {
    pub fn new() -> Self {
        Self {
            tables: vec![],
            table_goals: HashMap::new(),
        }
    }

    pub fn new_table(&mut self, goal: Goal) -> usize {
        let table = Table::new(goal.clone());
        let index = self.tables.len();
        self.tables.push(table);
        self.table_goals.insert(goal, index);
        index
    }
}

/// A substitution from unboud variables to types.
#[derive(Clone, Debug)]
pub struct Substitution {
    pub mapping: HashMap<usize, Ty>,
    pub origins: HashMap<usize, AnswerOrigin>,
}

impl Substitution {
    pub fn add(&mut self, index: usize, ty: Ty, origin: AnswerOrigin) {
        self.mapping.insert(index, ty);
        self.origins.insert(index, origin);
    }

    pub fn new() -> Self {
        Self {
            mapping: HashMap::new(),
            origins: HashMap::new(),
        }
    }

    pub fn apply(&self, g: &Goal) -> Goal {
        let mut folder = VariableSubstitutor {
            substitutions: &self.mapping,
        };
        folder.fold_goal(g.clone())
    }
}

struct Stack {
    entries: Vec<StackEntry>,
}

#[derive(Clone)]
struct StackEntry {
    table_index: usize,
    active_strand_index: Option<usize>,
}

struct Strand {
    /// Substitution applied to a tables goal
    /// to get this strands head
    subst: Substitution,
    exclause: ExClause,
    selected_subgoal: Option<SelectedSubgoal>,
    origin: AnswerOrigin,
    labeling: Vec<UniverseIndex>,
    evidence: Vec<Answer>,
}

#[derive(Clone)]
struct SelectedSubgoal {
    subgoal_index: usize,
    /// Next answer to request
    next_answer: usize,
    /// Index of the table in the forest correspoding to this subgoal.
    /// It should be called to retrieve an answer
    table_index: usize,
    /// Mapping that allows to unmap canonical goal in form T(?0, ?1)
    /// to a uncanonical version containing generics.
    /// This is used to later map the answers back to the original
    /// So for example
    ///
    ///  Clone(Vec(a)) |- Clone(a) => { ?0 => a }
    ///
    ///  Would uncanonilize to Clone(Vec(?0)) |- Clone(?0)
    ///  When getting an answer ?0 => Int
    ///  We can then remap it using this map by replacing generic
    ///  corresponding to the ?0 with Int, getting Clone(Vec(Int))
    uncanonilize_mapping: UnCanonMap,
}

struct Table {
    #[allow(dead_code)]
    goal: Goal,
    strands: VecDeque<Strand>,
    answers: Vec<Answer>,
}

impl Table {
    fn new(goal: Goal) -> Self {
        Self {
            goal,
            strands: VecDeque::new(),
            answers: vec![],
        }
    }
}

impl LabelingFunction for HashMap<usize, UniverseIndex> {
    fn check_variable(&self, variable: usize) -> Option<UniverseIndex> {
        self.get(&variable).cloned()
    }

    fn check_constant(&self, constant: usize) -> Option<UniverseIndex> {
        self.get(&constant).cloned()
    }

    fn add_variable(&mut self, variable: usize, universe: UniverseIndex) {
        assert!(self.insert(variable, universe).is_none())
    }

    fn add_constant(&mut self, constant: usize, universe: UniverseIndex) {
        assert!(self.insert(constant, universe).is_none())
    }

    fn adjust_universe(&mut self, var: usize, universe: UniverseIndex) {
        self.iter_mut().for_each(|(v, u)| {
            if *v == var {
                *u = universe;
            }
        })
    }
}

struct VariableGeneratorImpl<'l, 'c> {
    labeling_function: &'l mut dyn LabelingFunction,
    next_variable: &'c mut usize,
}

impl VariableContext for VariableGeneratorImpl<'_, '_> {
    fn next_variable(&mut self, in_universe: UniverseIndex) -> Ty {
        let variable = *self.next_variable;
        *self.next_variable += 1;
        self.labeling_function.add_variable(variable, in_universe);
        Ty::Variable(variable)
    }

    fn next_constant(&mut self, in_universe: UniverseIndex) -> Ty {
        let constant = *self.next_variable;
        *self.next_variable += 1;
        self.labeling_function.add_constant(constant, in_universe);
        Ty::SynthesizedConstant(constant)
    }

    fn labeling_function(&mut self) -> &mut dyn LabelingFunction {
        self.labeling_function
    }
}

impl LabelingFunction for Vec<UniverseIndex> {
    fn check_variable(&self, variable: usize) -> Option<UniverseIndex> {
        self.get(variable).cloned()
    }

    fn check_constant(&self, constant: usize) -> Option<UniverseIndex> {
        self.get(constant).cloned()
    }

    fn add_variable(&mut self, variable: usize, universe: UniverseIndex) {
        assert!(self.len() == variable);
        self.push(universe)
    }

    fn add_constant(&mut self, constant: usize, universe: UniverseIndex) {
        assert!(self.len() == constant);
        self.push(universe)
    }

    fn adjust_universe(&mut self, var: usize, universe: UniverseIndex) {
        assert!(self.len() > var);
        self[var] = universe
    }
}

pub struct SlgSolver<'db, 'forest> {
    forest: &'forest mut Forest,
    stack: Stack,
    database: &'db Database,
    next_answer: usize,
    goal: Goal,
}

impl<'db, 'forest> SlgSolver<'db, 'forest> {
    pub fn new(database: &'db Database, forest: &'forest mut Forest, goal: Goal) -> Self {
        Self {
            forest,
            stack: Stack { entries: vec![] },
            database,
            next_answer: 0,
            goal,
        }
    }

    pub fn solve(&mut self) -> Option<Answer> {
        let goal = self.goal.clone();
        let mut labeling = Vec::new();
        let mut next_variable = 0;
        let mut var_ctx = VariableGeneratorImpl {
            labeling_function: &mut labeling,
            next_variable: &mut next_variable,
        };
        let mut normalizer = GoalNormalizer::new(&mut var_ctx, UniverseIndex::ROOT);
        let goal = normalizer.fold_goal(goal);
        let (canonical_goal, unmap) = CanonicalGoal::new(goal, &labeling);
        let max_var = unmap.len();
        let res = self.ensure_answer(self.next_answer, canonical_goal);
        self.next_answer += 1;
        let res = res.map(
            |Answer {
                 subst,
                 origin,
                 evidence,
             }| {
                let new_mapping = subst
                    .mapping
                    .iter()
                    .map(|(var, ty)| (var.clone(), ty.clone()))
                    .filter(|(var, val)| {
                        if var >= &max_var {
                            return false;
                        }
                        if let Ty::SynthesizedConstant(_) = val {
                            return false;
                        }
                        return true;
                    })
                    .collect::<HashMap<_, _>>();
                Answer {
                    subst: Substitution {
                        mapping: new_mapping,
                        origins: subst.origins,
                    },
                    origin,
                    evidence,
                }
            },
        );
        res
    }

    #[tracing::instrument(skip(self))]
    pub fn ensure_answer(&mut self, answer: usize, goal: CanonicalGoal) -> Option<Answer> {
        info!("Looking for answer to {:?}", goal.goal);
        let table_index = self.get_or_create_table_for_goal(goal);
        match self.ensure_answer_from_table(answer, table_index) {
            AnswerRes::Answer(subst) => Some(subst),
            AnswerRes::SolveUsingStackTop => self.solve_using_stack_top(),
            AnswerRes::NoMoreAnswers => None,
        }
    }

    fn ensure_answer_from_table(&mut self, answer: usize, table_idx: usize) -> AnswerRes {
        let table = &mut self.forest.tables[table_idx];
        let answer_opt = table.answers.get(answer);
        match answer_opt {
            // no answer has been found
            None => {
                // no more answers can be computed
                if self.forest.tables[table_idx].strands.is_empty() {
                    return AnswerRes::NoMoreAnswers;
                }
                // new answer has to be computed
                self.stack.entries.push(StackEntry {
                    table_index: table_idx,
                    active_strand_index: None,
                });
                AnswerRes::SolveUsingStackTop
            }
            Some(substitutions) => AnswerRes::Answer(substitutions.clone()),
        }
    }

    #[tracing::instrument(skip(self))]
    fn get_or_create_table_for_goal(&mut self, mut goal: CanonicalGoal) -> usize {
        if self.forest.table_goals.contains_key(&goal.goal) {
            info!("Table for {:?} already exists", goal.goal);
            return self.forest.table_goals[&goal.goal];
        }
        info!("Creating table for {:?}", goal.goal);
        let table_index = self.forest.new_table(goal.goal.clone());
        let table = &mut self.forest.tables[table_index];
        // Goal is in canonical form so it is already normalized and has universes
        // starting from 0
        let current_universe = goal.max_universe();
        let mut next_variable = goal.labeling_function.len();
        let matching = self.database.find_matching(
            &goal.goal,
            current_universe,
            &mut VariableGeneratorImpl {
                labeling_function: &mut goal.labeling_function,
                next_variable: &mut next_variable,
            },
        );
        if matching.is_empty() {
            info!("No match found for {:?}", goal.goal);
            return table_index;
        }
        // matching process created new variables and constants
        for MatchResult {
            exclause,
            substitution: subst,
            origin,
        } in matching
        {
            info!("Found match {:?} with substitution {:?}", exclause, subst);
            table.strands.push_back(Strand {
                subst,
                exclause,
                selected_subgoal: None,
                origin,
                labeling: goal.labeling_function.clone(),
                evidence: vec![],
            });
        }
        table_index
    }

    fn solve_using_stack_top(&mut self) -> Option<Answer> {
        loop {
            // get the current stack frame, this stack frame needs to be solved
            // in this iteration
            let mut stack_entry = self.stack.entries.last()?.clone();
            // No strand is active for the given stack frame
            // we need to activate it to have something to solve
            if stack_entry.active_strand_index.is_none() {
                // activate first strands
                let Some(strand_index) = self.select_strand(stack_entry.table_index) else {
                    // Strand could not be selected so we just pop, no more answers can be computed
                    self.stack.entries.pop();
                    continue;
                };
                // Save the active strand index in the stack entry
                // and continue the loop
                self.stack.entries.last_mut().unwrap().active_strand_index = Some(strand_index);
                stack_entry.active_strand_index = Some(strand_index);
            }
            // Now we know that for sure some strand is active so we can take it
            let active_strand_index = stack_entry.active_strand_index.unwrap();
            let no_subgoal_selected = self.forest.tables[stack_entry.table_index].strands
                [active_strand_index]
                .selected_subgoal
                .is_none();
            // We check if the active strand has a subgoal selected if not
            // we need to select one to solve in this iteration
            if no_subgoal_selected {
                match self.select_subgoal(&stack_entry, active_strand_index) {
                    // We selected the subgoal that already has an answer computed
                    // (it has been cashed by previous iterations)
                    SubgoalSelection::Answer(subst) => {
                        // we got an answer pop the stack
                        self.stack.entries.pop();
                        if self.stack.entries.is_empty() {
                            // the stack is empty so we are done with the whole query
                            return Some(subst);
                        }
                        // in the next iteration we will ask this table again
                        // and this will be the next answer it will get
                        continue;
                    }
                    SubgoalSelection::NoMoreSubgoals => {
                        unreachable!("This should not be returned by select_subgoal")
                    }
                    // subgoal has been selected, we can continue with the loop
                    SubgoalSelection::Subgoal(_) => {}
                };
            }
            // We are now sure there exists a subgoal to solve
            let selected_subgoal = self.forest.tables[stack_entry.table_index].strands
                [active_strand_index]
                .selected_subgoal
                .clone()
                .unwrap();
            // Get the answer to the goal. The answer are substitutions. For example:
            // 1 -> Int, meaning that replacing var 1 for Int is the answer.
            let answer = match self.ensure_answer_from_table(
                selected_subgoal.next_answer,
                selected_subgoal.table_index,
            ) {
                // We just got an answer to this subgoal, and the answer is this mapping
                AnswerRes::Answer(answer) => answer,
                // Skip to the next iteration, new table has been pushed to the stack
                AnswerRes::SolveUsingStackTop => continue,
                AnswerRes::NoMoreAnswers => {
                    // no more answers can be derived from this table
                    // we need to pop the strand from the table and select the next one
                    self.forest.tables[stack_entry.table_index]
                        .strands
                        .remove(active_strand_index)
                        .unwrap();
                    // we just remove the active strand so the next iteration of the loop
                    // will select the next one and return if there are no more strands
                    self.stack.entries.last_mut().unwrap().active_strand_index = None;
                    continue;
                }
            };
            // This maps variables from the subgoal to the generics in the original goal
            let uncanonilize_mapping = selected_subgoal.uncanonilize_mapping.clone();
            // We got an answer from the subgoal, but there might be more. So we
            // need to set the subgoal to gives is the next answer when we query it the
            // next time.
            self.forest.tables[stack_entry.table_index].strands[active_strand_index]
                .selected_subgoal
                .replace(SelectedSubgoal {
                    next_answer: selected_subgoal.next_answer + 1,
                    ..selected_subgoal
                });
            // Remap the answer to the original goal
            let mut generic_mapping = HashMap::new();
            for (binder_index, ty) in &answer.subst.mapping {
                uncanonilize_mapping.get(*binder_index).map(|index| {
                    generic_mapping.insert(*index, ty.clone());
                });
            }
            // Create a new exclause for the table to prove
            // This is achieved by removing the subgoal we just proven from it
            // And applyign the substitution we got to the head and rest of the subgoals
            let new_ex_clause = prepare_exclause_from_an_answer(
                &self.forest.tables[stack_entry.table_index].strands[active_strand_index].exclause,
                &generic_mapping,
                selected_subgoal.subgoal_index,
            );
            // Merge substitutions we already got within this strand with the new
            // substitutions we got from the subgoal.
            let new_subst = merge_subtitutions_with_generics_substitution(
                &self.forest.tables[stack_entry.table_index].strands[active_strand_index].subst,
                &generic_mapping,
            );
            // Create a new strand that has new exclause to prove as well
            // as the new merged substitutions.
            let new_strand = Strand {
                subst: new_subst,
                exclause: new_ex_clause,
                selected_subgoal: None,
                // TODO: Is this correct? We should probably forward origin from the answer
                origin: self.forest.tables[stack_entry.table_index].strands[active_strand_index]
                    .origin
                    .clone(),
                labeling: self.forest.tables[stack_entry.table_index].strands[active_strand_index]
                    .labeling
                    .clone(),
                evidence: {
                    let mut evidence = self.forest.tables[stack_entry.table_index].strands
                        [active_strand_index]
                        .evidence
                        .clone();
                    evidence.push(answer.clone());
                    warn!("Making new strand with evidence: {:?}", evidence);
                    evidence
                },
            };
            self.forest.tables[stack_entry.table_index]
                .strands
                .push_back(new_strand);
            let new_strand_index = self.forest.tables[stack_entry.table_index].strands.len() - 1;
            self.stack.entries.pop();
            // To prove the previous strand we need to prove the new one
            // so we push it on top fo the stack to solve
            self.stack.entries.push(StackEntry {
                table_index: stack_entry.table_index,
                active_strand_index: Some(new_strand_index),
            });
        }
    }

    fn select_subgoal(
        &mut self,
        stack_entry: &StackEntry,
        strand_index: usize,
    ) -> SubgoalSelection {
        let next_subgoal = self.select_next_subgoal(stack_entry.table_index, strand_index);
        if let SubgoalSelection::NoMoreSubgoals = next_subgoal {
            info!("No more subgoals for table {}", stack_entry.table_index);
            // this node is an answer as it does not have any subgoals to select
            let subst = self.forest.tables[stack_entry.table_index].strands[strand_index]
                .subst
                .clone();

            // unmap answer to have generics instead of variables
            let unmap = self.forest.tables[stack_entry.table_index].strands[strand_index]
                .exclause
                .unmap
                .iter()
                .cloned()
                .enumerate()
                .filter_map(|(index, ty)| match ty {
                    Ty::Variable(var) => Some((var, Ty::Generic { scopes: 0, index })),
                    Ty::SynthesizedConstant(var) => Some((var, Ty::Generic { scopes: 0, index })),
                    _ => None,
                })
                .collect::<HashMap<_, _>>();
            let origin = self.forest.tables[stack_entry.table_index].strands[strand_index]
                .origin
                .clone();
            let evidence = self.forest.tables[stack_entry.table_index].strands[strand_index]
                .evidence
                .clone();
            let mut substitutor = VariableSubstitutor {
                substitutions: &unmap,
            };
            let mut mapping = HashMap::new();
            for (index, ty) in subst.mapping.clone() {
                mapping.insert(index, substitutor.fold_ty(ty));
            }
            let subst = Substitution {
                mapping,
                origins: subst.origins,
            };
            // remove the strand from the table as there is nothing more to do with it
            self.forest.tables[stack_entry.table_index]
                .strands
                .remove(strand_index)
                .unwrap();
            // push the substitutions as the answer
            let answer = Answer {
                subst,
                origin,
                evidence,
            };
            self.forest.tables[stack_entry.table_index]
                .answers
                .push(answer.clone());
            return SubgoalSelection::Answer(answer);
        }
        info!(
            "Selected subgoal {:?} for table {}",
            next_subgoal, stack_entry.table_index
        );
        // otherwise return the next subgoal
        next_subgoal
    }

    fn select_strand(&self, table: usize) -> Option<usize> {
        if self.forest.tables[table].strands.is_empty() {
            return None;
        }
        Some(0)
    }

    #[allow(unreachable_code, unused_variables)]
    fn select_next_subgoal(&mut self, table_idx: usize, strand_idx: usize) -> SubgoalSelection {
        info!("Selecting next subgoal for table {}", table_idx);
        let table = &mut self.forest.tables[table_idx];
        let strand = &mut table.strands[strand_idx];
        // we select the last subgoal so its easy to pop it later
        if strand.exclause.subgoals.is_empty() {
            return SubgoalSelection::NoMoreSubgoals;
        }
        let subgoal_index = strand.exclause.subgoals.len() - 1;
        let subgoal = strand.exclause.subgoals[subgoal_index].clone();
        // we apply the substitution and get a goal that has all the bound
        // variables replaced with the types from the substitution
        let goal = strand.subst.apply(&subgoal);
        // We renumber constants and variables to start from 0
        let (canonical_goal, unmap_variables) = CanonicalGoal::new(goal, &strand.labeling);
        let table_index = self.get_or_create_table_for_goal(canonical_goal);
        self.forest.tables[table_idx].strands[strand_idx].selected_subgoal =
            Some(SelectedSubgoal {
                subgoal_index,
                next_answer: 0,
                table_index,
                uncanonilize_mapping: unmap_variables,
            });
        SubgoalSelection::Subgoal(subgoal_index)
    }
}

enum AnswerRes {
    Answer(Answer),
    SolveUsingStackTop,
    NoMoreAnswers,
}

#[derive(Debug)]
enum SubgoalSelection {
    Answer(Answer),
    Subgoal(usize),
    NoMoreSubgoals,
}

/// Replaces all the generics in the type with the types from the mapping
/// Also remove selected subgoal from the exclause.
///
/// This is to create new goals for the table to prove
fn prepare_exclause_from_an_answer(
    exclause: &ExClause,
    generic_mapping: &HashMap<usize, Ty>,
    selected_subgoal: usize,
) -> ExClause {
    let mut substitutor = VariableSubstitutor {
        substitutions: generic_mapping,
    };
    let mut new_exclause = exclause.clone();
    new_exclause.subgoals.remove(selected_subgoal);

    for constraint in new_exclause.subgoals.iter_mut() {
        *constraint = substitutor.fold_goal(constraint.clone());
    }
    new_exclause.head = substitutor.fold_goal(new_exclause.head);
    new_exclause.unmap = new_exclause
        .unmap
        .iter()
        .map(|ty| substitutor.fold_ty(ty.clone()))
        .collect();
    new_exclause
}

/// Update old substitution with the information from the new one.
/// This basically means replacing variables in the types of the old subsitution
/// with the types from the new one
fn merge_subtitutions_with_generics_substitution(
    subst: &Substitution,
    generic_subst: &HashMap<usize, Ty>,
) -> Substitution {
    let mut substitutor = VariableSubstitutor {
        substitutions: generic_subst,
    };
    let mut mapping = HashMap::new();
    for (index, ty) in subst.mapping.clone() {
        mapping.insert(index, substitutor.fold_ty(ty));
    }
    Substitution {
        mapping,
        origins: subst.origins.clone(),
    }
}

impl<'db, 'forest> Iterator for SlgSolver<'db, 'forest> {
    type Item = Answer;

    fn next(&mut self) -> Option<Self::Item> {
        self.solve()
    }
}

#[derive(Debug, Clone)]
pub struct Answer {
    pub subst: Substitution,
    pub origin: AnswerOrigin,
    /// Maps constraints to their respective answers in the reverse order of
    /// their appearence at the definition
    pub evidence: Vec<Answer>,
}
