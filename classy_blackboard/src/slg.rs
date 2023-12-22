use std::collections::{HashMap, VecDeque};

use crate::{
    database::{Database, GenericRef, MatchResult, UniverseIndex, VariableContext},
    fold::Folder,
    goal::{ExClause, Goal, LabelingFunction},
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

type UnboundIndex = usize;
type GenericIndex = usize;

/// A substitution from unboud variables to types.
#[derive(Clone, Debug)]
pub struct Substitution {
    pub mapping: HashMap<usize, Ty>,
}

impl From<HashMap<usize, Ty>> for Substitution {
    fn from(mapping: HashMap<usize, Ty>) -> Self {
        Self { mapping }
    }
}

impl Substitution {
    pub fn add(&mut self, index: usize, ty: Ty) {
        self.mapping.insert(index, ty);
    }

    pub fn new() -> Self {
        Self {
            mapping: HashMap::new(),
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
    origin: Option<GenericRef>,
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
    uncanonilize_mapping: HashMap<Ty, String>,
}

struct Table {
    #[allow(dead_code)]
    goal: Goal,
    strands: VecDeque<Strand>,
    answers: Vec<Substitution>,
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

pub struct SlgSolver<'db> {
    forest: Forest,
    stack: Stack,
    database: &'db Database,
    next_answer: usize,
    labeling_function: HashMap<usize, UniverseIndex>,

    next_variable: usize,
}

impl<'db> SlgSolver<'db> {
    pub fn new(database: &'db Database, forest: Forest) -> Self {
        Self {
            forest,
            stack: Stack { entries: vec![] },
            database,
            next_answer: 0,
            labeling_function: HashMap::new(),
            next_variable: 0,
        }
    }

    pub fn solve(&mut self, goal: Goal) -> Option<Substitution> {
        // TODO: Actually all the generics should be numbers as it was before
        // TODO: Actually they should be expressed as Debruijn indices
        // TODO: This is so the caching works as expected.
        // TODO: Basically if we normalize goals the each goal will get its own table
        // TODO: So every goal should start in form with no variables just generics and
        // TODO: somehow descriptors for constants?
        // TODO: Where constants reference forall scope and generics reference exists
        let res = self.ensure_answer(self.next_answer, goal);
        self.next_answer += 1;
        res
    }

    pub fn ensure_answer(&mut self, answer: usize, goal: Goal) -> Option<Substitution> {
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

    fn get_or_create_table_for_goal(&mut self, goal: Goal) -> usize {
        if self.forest.table_goals.contains_key(&goal) {
            return self.forest.table_goals[&goal];
        }
        let table_index = self.forest.new_table(goal.clone());
        let table = &mut self.forest.tables[table_index];
        let current_universe = goal.max_universe(&self.labeling_function);
        let matching = self.database.find_matching(
            &goal,
            current_universe,
            &mut VariableGeneratorImpl {
                labeling_function: &mut self.labeling_function,
                next_variable: &mut self.next_variable,
            },
        );
        for MatchResult {
            exclause,
            substitution: subst,
            origin,
        } in matching
        {
            table.strands.push_back(Strand {
                subst,
                exclause,
                selected_subgoal: None,
                origin,
            });
        }
        table_index
    }

    fn solve_using_stack_top(&mut self) -> Option<Substitution> {
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
            let subst = match self.ensure_answer_from_table(
                selected_subgoal.next_answer,
                selected_subgoal.table_index,
            ) {
                // We just got an answer to this subgoal, and the answer is this mapping
                AnswerRes::Answer(subst) => subst,
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
            // This maps vars to generics, for example ?0 => a.
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
            // Remap substitution to the generics.
            // Basically or substitution only applies to the form with vars but the
            // original goal had generics in it. So we need to map the vars back to
            // the generics.
            let mut generic_mapping = HashMap::new();
            for (binder_index, ty) in subst.mapping {
                let generic_index = uncanonilize_mapping[&Ty::Variable(binder_index)];
                generic_mapping.insert(generic_index, ty);
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
            // Create a new strand that has new exclause to prove an well
            // as the new merged substitutions.
            let new_strand = Strand {
                subst: new_subst,
                exclause: new_ex_clause,
                selected_subgoal: None,
                // copy the origin of the substitutions we got
                // TODO: Is this correct? We should probably forward origin from the answer
                origin: self.forest.tables[stack_entry.table_index].strands[active_strand_index]
                    .origin
                    .clone(),
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
            // this node is an answer as it does not have any subgoals to select
            let subst = self.forest.tables[stack_entry.table_index].strands[strand_index]
                .subst
                .clone();
            // remove the strand from the table as there is nothing more to do with it
            self.forest.tables[stack_entry.table_index]
                .strands
                .remove(strand_index)
                .unwrap();
            // push the substitutions as the answer
            self.forest.tables[stack_entry.table_index]
                .answers
                .push(subst.clone());
            return SubgoalSelection::Answer(subst);
        }
        // otherwise return the next subgoal
        next_subgoal
    }

    fn select_strand(&self, table: usize) -> Option<usize> {
        if self.forest.tables[table].strands.is_empty() {
            return None;
        }
        Some(0)
    }

    // TODO: Remove once the internal todo is solved
    #[allow(unreachable_code, unused_variables)]
    fn select_next_subgoal(&mut self, table_idx: usize, strand_idx: usize) -> SubgoalSelection {
        let table = &mut self.forest.tables[table_idx];
        let strand = &mut table.strands[strand_idx];
        // we select the last subgoal so its easy to pop it later
        if strand.exclause.subgoals.is_empty() {
            return SubgoalSelection::NoMoreSubgoals;
        }
        let subgoal_index = strand.exclause.subgoals.len() - 1;
        let subgoal = strand.exclause.subgoals[subgoal_index].clone();
        let substitution = &strand.subst;
        let goal = substitution.apply(&subgoal);
        let max_universe = goal.max_universe(&self.labeling_function);
        // TODO: This is wrong, we actually cannot noramlize the goal here
        // TODO: We need to have goal using generics in the "pure form" where
        // TODO: the ordering of generics starts from 0
        let goal = todo!();
        let mapping = todo!();
        let table_index = self.get_or_create_table_for_goal(goal);
        self.forest.tables[table_idx].strands[strand_idx].selected_subgoal =
            Some(SelectedSubgoal {
                subgoal_index,
                next_answer: 0,
                table_index,
                uncanonilize_mapping: mapping,
            });
        SubgoalSelection::Subgoal(subgoal_index)
    }
}

enum AnswerRes {
    Answer(Substitution),
    SolveUsingStackTop,
    NoMoreAnswers,
}

enum SubgoalSelection {
    Answer(Substitution),
    Subgoal(usize),
    NoMoreSubgoals,
}

// TODO: All of the below

/// Replaces all the generics in the type with the types from the mapping
/// Also remove selected subgoal from the exclause.
///
/// This is to create new goals for the table to prove
fn prepare_exclause_from_an_answer(
    _exclause: &ExClause,
    _generic_mapping: &HashMap<usize, Ty>,
    _selected_subgoal: usize,
) -> ExClause {
    // let mut new_exclause = exclause.clone();
    // new_exclause.subgoals.remove(selected_subgoal);
    // for constraint in new_exclause.subgoals.iter_mut() {
    //     *constraint = subsitute_generics(&generic_mapping, constraint.clone());
    // }
    // new_exclause.head = subsitute_generics(&generic_mapping, new_exclause.head);
    // new_exclause
    todo!()
}

/// Honestly I have no idea what this function does
///
/// @param subst - Substitutions made to the clause to get the table goal
/// @param generic_subst - Substitutions made to the clause to get the original
/// goal
fn merge_subtitutions_with_generics_substitution(
    _subst: &Substitution,
    _generic_subst: &HashMap<usize, Ty>,
) -> Substitution {
    // let mut mapping = HashMap::new();
    // for (index, ty) in subst.mapping.clone() {
    //     mapping.insert(index, subsitute_generics(generic_subst, ty));
    // }
    // Substitution { mapping }
    todo!()
}
