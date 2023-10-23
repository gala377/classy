use std::collections::HashMap;
use std::fmt::Write;
use std::ops::ControlFlow;

use crate::clauses::{Constraint, Instance, Ty, TyRef, TypeImpl};
use crate::query;
use crate::slg::Substitution;

pub struct Database {
    type_impls: Vec<TypeImpl>,
    instances: Vec<Instance>,
    names_to_ty: HashMap<String, usize>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            type_impls: Vec::new(),
            instances: Vec::new(),
            names_to_ty: HashMap::new(),
        }
    }

    pub fn make_readable(&self, ty: Ty, res: &mut impl Write) {
        match ty {
            Ty::Ref(index) => {
                let t_impl = &self.type_impls[index.0];
                write!(res, "{}", t_impl.name).unwrap();
                if t_impl.type_params.len() > 0 {
                    write!(res, "<").unwrap();
                    for (i, param) in t_impl.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(res, ", ").unwrap();
                        }
                        write!(res, "{}", param).unwrap();
                    }
                    write!(res, ">").unwrap();
                }
            }
            Ty::Array(inner) => {
                let mut inner_str = String::new();
                self.make_readable(*inner, &mut inner_str);
                write!(res, "[{}]", inner_str).unwrap();
            }
            Ty::Tuple(inner) => {
                write!(res, "(").unwrap();
                for (i, inner) in inner.iter().enumerate() {
                    if i > 0 {
                        write!(res, ", ").unwrap();
                    }
                    self.make_readable(inner.clone(), res);
                }
                write!(res, ")").unwrap();
            }
            Ty::Fn(args, ret) => {
                write!(res, "(").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(res, ", ").unwrap();
                    }
                    self.make_readable(arg.clone(), res);
                }
                write!(res, ") -> ").unwrap();
                self.make_readable(*ret, res);
            }
            Ty::Generic(index) => {
                write!(res, "g{}", index).unwrap();
            }
            Ty::App(head, args) => {
                let t_impl = &self.type_impls[head.0];
                write!(res, "{}", t_impl.name).unwrap();
                write!(res, "<").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(res, ", ").unwrap();
                    }
                    self.make_readable(arg.clone(), res);
                }
                write!(res, ">").unwrap();
            }
            Ty::UnBound(index) => {
                write!(res, "∃{}", index).unwrap();
            }
            Ty::Forall(index) => {
                write!(res, "∀{}", index).unwrap();
            }
        }
    }

    pub fn add_type_impl(&mut self, type_impl: TypeImpl) -> TyRef {
        self.names_to_ty
            .insert(type_impl.name.clone(), self.type_impls.len());
        self.type_impls.push(type_impl);
        TyRef(self.type_impls.len() - 1)
    }

    pub fn add_struct(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
        members: Vec<(String, Ty)>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: false,
            members,
        };
        self.add_type_impl(type_impl)
    }

    pub fn add_type_class(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
        members: Vec<(String, Ty)>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: true,
            members,
        };
        self.add_type_impl(type_impl)
    }

    pub fn add_instance_for(
        &mut self,
        type_class: TyRef,
        type_params: Vec<String>,
        constraints: Vec<Constraint>,
        args: Vec<Ty>,
    ) {
        let instance = Instance {
            type_class,

            type_params,
            args,
            constraints,
        };
        self.add_instance(instance);
    }

    pub fn add_instance(&mut self, instance: Instance) {
        self.instances.push(instance);
    }

    pub fn typeref_from_name(&self, name: &str) -> Option<TyRef> {
        self.names_to_ty.get(name).map(|i| TyRef(*i))
    }

    pub fn run(&mut self, query::TypeHolds(goal): &query::TypeHolds) -> bool {
        let mut solver = Solver::new(self);
        let mut goals = goal.clone().into();
        solver.goals.append(&mut goals);
        solver.solve()
    }

    /// Union given type with all instances and type defnitions and returns
    /// resulting exclauses alongside a substitution that has been made in order
    /// to make the union possible.
    pub fn find_matching(&self, ty: &Ty) -> Vec<(ExClause, Substitution)> {
        let mut result = Vec::new();
        // match instances
        for instance in &self.instances {
            let union_with = Ty::App(instance.type_class.clone(), instance.args.clone());
            let mut subst = HashMap::new();
            if union(ty.clone(), union_with, &mut subst).is_some() {
                let constraints = instance
                    .constraints
                    .clone()
                    .into_iter()
                    .map(|c| match c {
                        Constraint::Eq(_, _) => unimplemented!(),
                        Constraint::Class(head, args) => Ty::App(head, args),
                    })
                    .collect();
                let ex_clause = ExClause {
                    head: ty.clone(),
                    constraints,
                };
                result.push((ex_clause, subst.into()));
            }
        }
        // match types and classes
        for type_impl in &self.type_impls {
            // skip type class definitions without constraint
            if type_impl.is_class && type_impl.constraints.is_empty() {
                continue;
            }
            let type_ref = self.typeref_from_name(&type_impl.name).unwrap();
            let union_with = if type_impl.type_params.len() == 0 {
                Ty::Ref(type_ref)
            } else {
                let mut args = Vec::new();
                for i in 0..type_impl.type_params.len() {
                    args.push(Ty::Generic(i));
                }
                Ty::App(type_ref, args)
            };
            let mut subst = HashMap::new();
            if union(ty.clone(), union_with, &mut subst).is_some() {
                let constraints = type_impl
                    .constraints
                    .clone()
                    .into_iter()
                    .map(|c| match c {
                        Constraint::Eq(_, _) => unimplemented!(),
                        Constraint::Class(head, args) => Ty::App(head, args),
                    })
                    .collect();
                let ex_clause = ExClause {
                    head: ty.clone(),
                    constraints,
                };
                result.push((ex_clause, subst.into()));
            }
        }
        result
    }
}

/// Union 2 types together and record all substitutions made along the way in
/// passed map.
///
/// Note that substitution rules might be unclear. For example, substitution is.
/// only recorder when an unboud EXISTS variable (?0) is matched agains other
/// type. What it means that generic variables do not generate substitutions.
fn union(t1: Ty, t2: Ty, substituted: &mut HashMap<usize, Ty>) -> Option<()> {
    match (t1, t2) {
        (Ty::UnBound(index), t2) => match substituted.get(&index) {
            Some(old) if *old != t2 => None,
            Some(_) => Some(()),
            None => {
                substituted.insert(index, t2);
                Some(())
            }
        },
        // TODO: we need to check that generics are substituted consistantly
        (Ty::Forall(_), Ty::Generic(_)) => Some(()),
        (Ty::App(h1, as1), Ty::App(h2, as2)) if h1 == h2 => {
            if as1.len() != as2.len() {
                return None;
            }
            for (a1, a2) in as1.into_iter().zip(as2.into_iter()) {
                union(a1, a2, substituted)?;
            }
            Some(())
        }
        (Ty::Array(t1), Ty::Array(t2)) => union(*t1, *t2, substituted),
        (Ty::Tuple(ts1), Ty::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                return None;
            }
            for (t1, t2) in ts1.into_iter().zip(ts2.into_iter()) {
                union(t1, t2, substituted)?;
            }
            Some(())
        }
        (Ty::Ref(t1), Ty::Ref(t2)) if t1 == t2 => Some(()),
        (Ty::Fn(args1, ret1), Ty::Fn(args2, ret2)) => {
            if args1.len() != args2.len() {
                return None;
            }
            for (t1, t2) in args1.into_iter().zip(args2.into_iter()) {
                union(t1, t2, substituted)?;
            }
            union(*ret1, *ret2, substituted)
        }
        (Ty::Generic(_), _) => Some(()),
        (_, Ty::Generic(_)) => Some(()),
        (_, Ty::UnBound(_)) => panic!("Unexpected unbound variable"),
        (_, _) => None,
    }
}

#[derive(Debug, Clone)]
pub struct ExClause {
    pub head: Ty,
    pub constraints: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct Goal {
    pub head: TyRef,
    pub args: Vec<Ty>,
}

struct Solver<'db> {
    pub goals: Vec<Goal>,
    database: &'db Database,
}

struct Choice {
    goals: Vec<Goal>,
}

impl Choice {
    fn new() -> Self {
        Choice { goals: Vec::new() }
    }
    fn push_goal(&mut self, goal: Goal) {
        self.goals.push(goal);
    }
}

use ControlFlow::*;

impl<'db> Solver<'db> {
    pub fn new(database: &'db Database) -> Self {
        Solver {
            goals: Vec::new(),
            database,
        }
    }

    pub fn solve(&mut self) -> bool {
        println!("Running solver...");
        println!("Goals: {:#?}", self.goals);
        while let Some(goal) = self.goals.pop() {
            match self.solve_goal(goal) {
                Break(b) => return b,
                Continue(()) => (),
            }
        }
        true
    }

    pub fn solve_goal(&mut self, goal: Goal) -> ControlFlow<bool> {
        println!("Solving goal: {:#?}", goal);
        let ty = &self.database.type_impls[goal.head.0];
        println!("Got head Type: {:#?}", ty);
        if ty.type_params.len() != goal.args.len() {
            println!("Type params len does not match");
            return Break(false);
        }
        if ty.is_class {
            return self.find_instance(goal.head, goal.args);
        }
        self.solve_struct(ty, goal)
    }

    fn solve_struct(&mut self, ty: &TypeImpl, goal: Goal) -> ControlFlow<bool> {
        let mut constraints = ty.constraints.clone();
        for (generic_index, arg) in goal.args.iter().enumerate() {
            constraints = constraints
                .iter()
                .map(|c| c.substitute(generic_index, arg))
                .collect();
        }
        for c in constraints {
            match c {
                Constraint::Eq(_, _) => unimplemented!(),
                Constraint::Class(head, args) => {
                    self.goals.push(Goal { head, args });
                }
            }
        }
        Continue(())
    }

    pub fn find_instance(&mut self, head: TyRef, args: Vec<Ty>) -> ControlFlow<bool> {
        println!("Type is a class");
        let instances = self
            .database
            .instances
            .iter()
            .filter(|inst| inst.type_class == head)
            .cloned()
            .collect::<Vec<_>>();
        println!("Instances: {:#?}", instances);
        let mut choices = Vec::new();
        for Instance {
            args: instance_args,
            constraints,
            ..
        } in instances
        {
            assert_eq!(args.len(), instance_args.len());
            self.unify_instance(&args, instance_args, constraints, &mut choices);
        }
        // see if any of the instantiations unify
        // given their respective constraints
        let current_goals = self.goals.clone();
        for choice in choices {
            let mut new_goals = current_goals.clone();
            new_goals.extend(choice.goals);
            self.goals = new_goals;
            if self.solve() {
                self.goals = current_goals;
                return Continue(());
            }
        }
        // if none of the choices worked we have to return false
        Break(false)
    }

    fn unify_instance(
        &mut self,
        args: &Vec<Ty>,
        instance_args: Vec<Ty>,
        constraints: Vec<Constraint>,
        choices: &mut Vec<Choice>,
    ) {
        let mut subst = HashMap::new();
        for (instance_arg, arg) in instance_args.iter().zip(args.iter()) {
            if !self.unify(instance_arg.clone(), arg.clone(), &mut subst) {
                return;
            }
        }
        let mut substituted = constraints.clone();
        for (generic_index, ty) in &subst {
            substituted = substituted
                .iter()
                .map(|c| c.substitute(*generic_index, ty))
                .collect();
        }
        let mut choice = Choice::new();
        for c in substituted {
            match c {
                Constraint::Eq(_, _) => unimplemented!(),
                Constraint::Class(head, args) => {
                    choice.push_goal(Goal { head, args });
                }
            }
        }
        choices.push(choice);
    }

    // unify needs to return substitutions for us
    pub fn unify(&mut self, t1: Ty, t2: Ty, subst: &mut HashMap<usize, Ty>) -> bool {
        match (t1, t2) {
            (Ty::UnBound(_), Ty::UnBound(_)) => false,
            (Ty::UnBound(_), _) => false,
            (_, Ty::UnBound(_)) => false,
            (t1, Ty::Generic(index)) => {
                if let Some(previous) = subst.insert(index, t1.clone()) {
                    if previous != t1 {
                        println!(
                            "Generic {:?} already has a value {:?} cannot substitute with {:?}",
                            index, previous, t1
                        );
                        return false;
                    }
                }
                true
            }
            (Ty::Generic(index), t2) => {
                if let Some(previous) = subst.insert(index, t2.clone()) {
                    if previous != t2 {
                        println!(
                            "Generic {:?} already has a value {:?} cannot substitute with {:?}",
                            index, previous, t2
                        );
                        return false;
                    }
                }
                true
            }
            (Ty::Ref(t1), Ty::Ref(t2)) if t1 == t2 => true,
            (Ty::Array(t1), Ty::Array(t2)) => self.unify(*t1, *t2, subst),
            (Ty::Tuple(ts1), Ty::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return false;
                }
                for (t1, t2) in ts1.into_iter().zip(ts2.into_iter()) {
                    if !self.unify(t1, t2, subst) {
                        return false;
                    }
                }
                true
            }
            (Ty::Fn(args1, ret1), Ty::Fn(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return false;
                }
                for (t1, t2) in args1.into_iter().zip(args2.into_iter()) {
                    if !self.unify(t1, t2, subst) {
                        return false;
                    }
                }
                self.unify(*ret1, *ret2, subst)
            }
            (Ty::App(head1, args1), Ty::App(head2, args2)) if head1 == head2 => {
                if args1.len() != args2.len() {
                    return false;
                }
                for (t1, t2) in args1.into_iter().zip(args2.into_iter()) {
                    if !self.unify(t1, t2, subst) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}
