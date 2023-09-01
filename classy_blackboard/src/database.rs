use std::collections::HashMap;
use std::ops::ControlFlow;

use crate::clauses::{Constraint, Instance, Ty, TyRef, TypeImpl};
use crate::query;

pub struct Database {
    type_impls: Vec<TypeImpl>,
    instances: Vec<Instance>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            type_impls: Vec::new(),
            instances: Vec::new(),
        }
    }

    pub fn add_type_impl(&mut self, type_impl: TypeImpl) -> TyRef {
        self.type_impls.push(type_impl);
        TyRef(self.type_impls.len() - 1)
    }

    pub fn add_struct(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: false,
        };
        self.add_type_impl(type_impl)
    }

    pub fn add_type_class(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: true,
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

    pub fn run(&mut self, query::TypeHolds(goal): &query::TypeHolds) -> bool {
        let mut solver = Solver::new(self);
        let mut goals = goal.clone().into();
        solver.goals.append(&mut goals);
        solver.solve()
    }
}

#[derive(Debug)]
pub struct Goal {
    pub head: TyRef,
    pub args: Vec<Ty>,
}

struct Solver<'db> {
    pub goals: Vec<Goal>,
    database: &'db Database,
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
        println!("Runbning solver...");
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
        let mut instance_found = false;
        let instances = self
            .database
            .instances
            .iter()
            .filter(|inst| inst.type_class == head)
            .cloned()
            .collect::<Vec<_>>();
        println!("Instances: {:#?}", instances);
        'outer: for (
            index,
            Instance {
                args: instance_args,
                constraints,
                ..
            },
        ) in instances.into_iter().enumerate()
        {
            assert_eq!(args.len(), instance_args.len());
            let mut subst = HashMap::new();
            for (instance_arg, arg) in instance_args.iter().zip(args.iter()) {
                if !self.unify(instance_arg.clone(), arg.clone(), &mut subst) {
                    continue 'outer;
                }
            }
            let mut substituted = constraints.clone();
            for (generic_index, ty) in &subst {
                substituted = substituted
                    .iter()
                    .map(|c| c.substitute(*generic_index, ty))
                    .collect();
            }
            println!("Instance {index} unified");
            instance_found = true;
            for c in substituted {
                match c {
                    Constraint::Eq(_, _) => unimplemented!(),
                    Constraint::Class(head, args) => {
                        self.goals.push(Goal { head, args });
                    }
                }
            }
        }
        if !instance_found {
            return Break(false);
        }
        Continue(())
    }

    // unify needs to return substitutions for us
    pub fn unify(&mut self, t1: Ty, t2: Ty, subst: &mut HashMap<usize, Ty>) -> bool {
        match (t1, t2) {
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
