use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
};

use crate::{
    clauses::Ty,
    database::{Database, ExClause},
};

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct CanonilizedGoal(Ty);

impl CanonilizedGoal {
    pub fn mk_canonical(typ: Ty) -> (Self, HashMap<GenericIndex, UnboundIndex>) {
        let mut mk = MkCanonical::new();
        let res = mk.canonilize(typ);
        (res, mk.mapping)
    }

    pub fn wrap_ty(typ: Ty) -> Self {
        Self(typ)
    }
}

struct MkCanonical {
    next_id: UnboundIndex,
    mapping: HashMap<GenericIndex, UnboundIndex>,
}

impl MkCanonical {
    fn new() -> Self {
        Self {
            next_id: 0,
            mapping: HashMap::new(),
        }
    }

    fn canonilize(&mut self, ty: Ty) -> CanonilizedGoal {
        CanonilizedGoal(self.fold_ty(ty))
    }

    fn fold_ty(&mut self, ty: Ty) -> Ty {
        match ty {
            Ty::Generic(index) if self.mapping.contains_key(&index) => {
                Ty::UnBound(*self.mapping.get(&index).unwrap())
            }
            Ty::Generic(index) => {
                let new_index = self.next_id;
                self.next_id += 1;
                self.mapping.insert(index, new_index);
                Ty::UnBound(new_index)
            }
            Ty::App(id, args) => Ty::App(id, args.into_iter().map(|t| self.fold_ty(t)).collect()),
            Ty::Ref(_) => ty,
            Ty::Array(t) => Ty::Array(Box::new(self.fold_ty(*t))),
            Ty::Tuple(ts) => Ty::Tuple(ts.into_iter().map(|t| self.fold_ty(t)).collect()),
            Ty::Fn(args, ret) => Ty::Fn(
                args.into_iter().map(|t| self.fold_ty(t)).collect(),
                Box::new(self.fold_ty(*ret)),
            ),
            Ty::UnBound(_) => panic!("Unexpected unbound type variable"),
        }
    }
}

pub struct Forest {
    tables: Vec<Table>,
    table_goals: HashMap<CanonilizedGoal, usize>,
}

impl Forest {
    pub fn new() -> Self {
        Self {
            tables: vec![],
            table_goals: HashMap::new(),
        }
    }

    pub fn new_table(&mut self, goal: CanonilizedGoal) -> usize {
        let table = Table::new();
        let index = self.tables.len();
        self.tables.push(table);
        self.table_goals.insert(goal, index);
        index
    }
}

type UnboundIndex = usize;
type GenericIndex = usize;

#[derive(Clone, Debug)]
pub struct Substitution {
    pub mapping: HashMap<usize, Ty>,
}

impl From<HashMap<usize, Ty>> for Substitution {
    fn from(mapping: HashMap<usize, Ty>) -> Self {
        Self { mapping }
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
    uncanonilize_mapping: HashMap<UnboundIndex, GenericIndex>,
}

struct Table {
    strands: VecDeque<Strand>,
    answers: Vec<Substitution>,
}

impl Table {
    fn new() -> Self {
        Self {
            strands: VecDeque::new(),
            answers: vec![],
        }
    }
}

pub struct SlgSolver<'db> {
    forest: Forest,
    stack: Stack,
    database: &'db Database,
    next_answer: usize,
}

impl<'db> SlgSolver<'db> {
    pub fn new(database: &'db Database, forest: Forest) -> Self {
        Self {
            forest,
            stack: Stack { entries: vec![] },
            database,
            next_answer: 0,
        }
    }

    pub fn solve(&mut self, goal: CanonilizedGoal) -> Option<Substitution> {
        let res = self.ensure_answer(self.next_answer, goal);
        self.next_answer += 1;
        res
    }

    pub fn ensure_answer(&mut self, answer: usize, goal: CanonilizedGoal) -> Option<Substitution> {
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

    fn get_or_create_table_for_goal(&mut self, goal: CanonilizedGoal) -> usize {
        if self.forest.table_goals.contains_key(&goal) {
            return self.forest.table_goals[&goal];
        }
        let table_index = self.forest.new_table(goal.clone());
        let table = &mut self.forest.tables[table_index];
        let matching = self.database.find_matching(&goal.0);
        for (exclause, subst) in matching {
            table.strands.push_back(Strand {
                subst,
                exclause,
                selected_subgoal: None,
            });
        }
        table_index
    }

    fn solve_using_stack_top(&mut self) -> Option<Substitution> {
        loop {
            let mut stack_entry = self.stack.entries.last()?.clone();
            if stack_entry.active_strand_index.is_none() {
                // activate first strands
                let Some(strand_index) = self.select_strand(stack_entry.table_index) else {
                    // Strand could not be selected so we just pop, no more answers can be computed
                    self.stack.entries.pop();
                    continue;
                };
                self.stack.entries.last_mut().unwrap().active_strand_index = Some(strand_index);
                stack_entry.active_strand_index = Some(strand_index);
            }
            let strand_index = stack_entry.active_strand_index.unwrap();
            let no_subgoal_selected = self.forest.tables[stack_entry.table_index].strands
                [strand_index]
                .selected_subgoal
                .is_none();
            if no_subgoal_selected {
                match self.select_subgoal(&stack_entry, strand_index) {
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
                    // subgoal has been selected nothing to do then
                    SubgoalSelection::Subgoal(_) => {}
                };
            }
            let selected_subgoal = self.forest.tables[stack_entry.table_index].strands
                [strand_index]
                .selected_subgoal
                .clone()
                .unwrap();
            let subst = match self.ensure_answer_from_table(
                selected_subgoal.next_answer,
                selected_subgoal.table_index,
            ) {
                AnswerRes::Answer(subst) => subst,
                // skip to the next iteration, new table has been pushed to the stack
                AnswerRes::SolveUsingStackTop => continue,
                AnswerRes::NoMoreAnswers => {
                    // no more answers can be derived from this table
                    // we need to pop the strand from the table and select the next one
                    self.forest.tables[stack_entry.table_index]
                        .strands
                        .remove(strand_index)
                        .unwrap();
                    // we just remove the active strand as the next iteration of the loop
                    // will select the next one and return in case there are no more strands
                    self.stack.entries.last_mut().unwrap().active_strand_index = None;
                    continue;
                }
            };
            let uncanonilize_mapping = selected_subgoal.uncanonilize_mapping.clone();
            // move the subgoal to the next answer
            self.forest.tables[stack_entry.table_index].strands[strand_index]
                .selected_subgoal
                .replace(SelectedSubgoal {
                    next_answer: selected_subgoal.next_answer + 1,
                    ..selected_subgoal
                });
            // remap substitution to the generics
            let mut generic_mapping = HashMap::new();
            for (binder_index, ty) in subst.mapping {
                let generic_index = uncanonilize_mapping[&binder_index];
                generic_mapping.insert(generic_index, ty);
            }
            let new_ex_clause = prepare_exclause_from_an_answer(
                &self.forest.tables[stack_entry.table_index].strands[strand_index].exclause,
                &generic_mapping,
                selected_subgoal.subgoal_index,
            );
            let new_subst = merge_subtitutions_with_generics_substitution(
                &self.forest.tables[stack_entry.table_index].strands[strand_index].subst,
                &generic_mapping,
            );
            let new_strand = Strand {
                subst: new_subst,
                exclause: new_ex_clause,
                selected_subgoal: None,
            };
            self.forest.tables[stack_entry.table_index]
                .strands
                .push_back(new_strand);
            let new_strand_index = self.forest.tables[stack_entry.table_index].strands.len() - 1;
            self.stack.entries.pop();
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

    fn select_next_subgoal(&mut self, table_idx: usize, strand_idx: usize) -> SubgoalSelection {
        let table = &mut self.forest.tables[table_idx];
        let strand = &mut table.strands[strand_idx];
        // we select the last subgoal so its easy to pop it later
        if strand.exclause.constraints.is_empty() {
            return SubgoalSelection::NoMoreSubgoals;
        }
        let subgoal_index = strand.exclause.constraints.len() - 1;
        let subgoal = strand.exclause.constraints[subgoal_index].clone();
        let (goal, mapping) = CanonilizedGoal::mk_canonical(subgoal);
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

fn subsitute_generics(map: &HashMap<usize, Ty>, ty: Ty) -> Ty {
    match ty {
        Ty::Array(inner) => Ty::Array(Box::new(subsitute_generics(map, *inner))),
        Ty::Tuple(inner) => Ty::Tuple(
            inner
                .into_iter()
                .map(|t| subsitute_generics(map, t))
                .collect(),
        ),
        Ty::Fn(args, ret) => Ty::Fn(
            args.into_iter()
                .map(|t| subsitute_generics(map, t))
                .collect(),
            Box::new(subsitute_generics(map, *ret)),
        ),
        Ty::Generic(index) if map.contains_key(&index) => map[&index].clone(),
        Ty::App(head, args) => Ty::App(
            head,
            args.into_iter()
                .map(|t| subsitute_generics(map, t))
                .collect(),
        ),
        t => t,
    }
}

fn prepare_exclause_from_an_answer(
    exclause: &ExClause,
    generic_mapping: &HashMap<usize, Ty>,
    selected_subgoal: usize,
) -> ExClause {
    let mut new_exclause = exclause.clone();
    new_exclause.constraints.remove(selected_subgoal);
    for constraint in new_exclause.constraints.iter_mut() {
        *constraint = subsitute_generics(&generic_mapping, constraint.clone());
    }
    new_exclause.head = subsitute_generics(&generic_mapping, new_exclause.head);
    new_exclause
}

fn merge_subtitutions_with_generics_substitution(
    subst: &Substitution,
    generic_subst: &HashMap<usize, Ty>,
) -> Substitution {
    let mut mapping = HashMap::new();
    for (index, ty) in subst.mapping.clone() {
        mapping.insert(index, subsitute_generics(generic_subst, ty));
    }
    Substitution { mapping }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{
        clauses::{Constraint, Ty, TyRef},
        database::Database,
        query,
    };

    fn basic_db_prepare() -> (Database, HashMap<String, TyRef>) {
        let mut db = Database::new();
        // class Show(a)
        let show = db.add_type_class(
            "Show".to_string(),
            vec!["a".to_string()],
            vec![],
            HashMap::new(),
        );
        // class Debug(a)
        let debug = db.add_type_class(
            "Debug".into(),
            vec!["a".to_string()],
            vec![],
            HashMap::new(),
        );
        // type Int
        let int = db.add_struct("Int".to_string(), vec![], vec![], HashMap::new());
        // type String
        let string = db.add_struct("String".into(), vec![], vec![], HashMap::new());
        // type Foo(a)
        let foo = db.add_struct(
            "Foo".to_owned(),
            vec!["a".to_owned()],
            vec![Constraint::Class(show, vec![Ty::Generic(0)])],
            HashMap::new(),
        );
        // instance for Show(Int)
        db.add_instance_for(show, vec![], vec![], vec![Ty::Ref(int)]);
        // instance for { Show(a) } => Debug(a)
        db.add_instance_for(
            debug,
            vec!["a".to_string()],
            vec![Constraint::Class(show, vec![Ty::Generic(0)])],
            vec![Ty::Generic(0)],
        );
        // instance for { Debug(a) } => Debug(Foo(a))
        db.add_instance_for(
            debug,
            vec!["a".to_string()],
            vec![Constraint::Class(debug, vec![Ty::Generic(0)])],
            vec![Ty::App(foo, vec![Ty::Generic(0)])],
        );
        let mut names = HashMap::new();
        names.insert("string".into(), string);
        names.insert("show".into(), show);
        names.insert("debug".into(), debug);
        names.insert("int".into(), int);
        names.insert("foo".into(), foo);
        (db, names)
    }
}
