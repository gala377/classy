use crate::{
    clauses::Clause,
    goal::{DomainGoal, Goal},
    ty::{ClassRef, Ty, TyRef},
};

pub trait Folder: Sized {
    fn fold_clause(&mut self, clause: Clause) -> Clause {
        walk_clause(self, clause)
    }

    fn fold_goal(&mut self, goal: Goal) -> Goal {
        walk_goal(self, goal)
    }

    fn fold_domain_goal(&mut self, domain_goal: DomainGoal) -> DomainGoal {
        walk_domain_goal(self, domain_goal)
    }

    fn fold_ty(&mut self, ty: Ty) -> Ty {
        walk_ty(self, ty)
    }

    fn fold_fact(&mut self, domain_goal: DomainGoal) -> Clause {
        walk_fact(self, domain_goal)
    }

    fn fold_clause_implies(&mut self, head: Clause, subgoals: Vec<Goal>) -> Clause {
        walk_clause_implies(self, head, subgoals)
    }

    fn fold_clause_forall(&mut self, generics: usize, clause: Clause) -> Clause {
        walk_clause_forall(self, generics, clause)
    }

    fn fold_goal_implies(&mut self, clauses: Vec<Clause>, goal: Goal) -> Goal {
        walk_goal_implies(self, clauses, goal)
    }

    fn fold_goal_exists(&mut self, introduced: usize, goal: Goal) -> Goal {
        walk_goal_exists(self, introduced, goal)
    }

    fn fold_goal_forall(&mut self, vars: usize, goal: Goal) -> Goal {
        walk_goal_forall(self, vars, goal)
    }

    fn fold_goal_domain(&mut self, domain: DomainGoal) -> Goal {
        walk_goal_domain(self, domain)
    }

    fn fold_domain_instance_exists_and_well_formed(
        &mut self,
        head: ClassRef,
        args: Vec<Ty>,
    ) -> DomainGoal {
        walk_domain_instance_exists_and_well_formed(self, head, args)
    }

    fn fold_domain_method_block_exists(&mut self, on_type: Ty) -> DomainGoal {
        walk_domain_method_block_exists(self, on_type)
    }

    fn fold_domain_type_well_formed(&mut self, ty: Ty) -> DomainGoal {
        walk_domain_type_well_formed(self, ty)
    }

    fn fold_domain_class_well_formed(&mut self, head: ClassRef, args: Vec<Ty>) -> DomainGoal {
        walk_domain_class_well_formed(self, head, args)
    }

    fn fold_ty_ref(&mut self, r: TyRef) -> Ty {
        walk_ty_ref(self, r)
    }

    fn fold_ty_array(&mut self, ty: Ty) -> Ty {
        walk_ty_array(self, ty)
    }

    fn fold_ty_tuple(&mut self, tys: Vec<Ty>) -> Ty {
        walk_ty_tuple(self, tys)
    }

    fn fold_ty_fn(&mut self, args: Vec<Ty>, ret: Ty) -> Ty {
        walk_ty_fn(self, args, ret)
    }

    fn fold_ty_app(&mut self, head: TyRef, args: Vec<Ty>) -> Ty {
        walk_ty_app(self, head, args)
    }

    fn fold_ty_variable(&mut self, idx: usize) -> Ty {
        walk_ty_variable(self, idx)
    }

    fn fold_ty_synthesized_constant(&mut self, idx: usize) -> Ty {
        walk_ty_synthesized_constant(self, idx)
    }

    fn fold_ty_generic(&mut self, scopes: usize, index: usize) -> Ty {
        walk_ty_generic(self, scopes, index)
    }
}

pub fn walk_clause(folder: &mut impl Folder, clause: Clause) -> Clause {
    match clause {
        Clause::Fact(domain_goal) => folder.fold_fact(domain_goal),
        Clause::Implies(head, subgoals) => folder.fold_clause_implies(*head, subgoals),
        Clause::Forall(generics, clause) => folder.fold_clause_forall(generics, *clause),
    }
}

pub fn walk_fact(folder: &mut impl Folder, domain_goal: DomainGoal) -> Clause {
    Clause::Fact(folder.fold_domain_goal(domain_goal))
}

pub fn walk_clause_implies(folder: &mut impl Folder, head: Clause, subgoals: Vec<Goal>) -> Clause {
    Clause::Implies(
        Box::new(folder.fold_clause(head)),
        subgoals.into_iter().map(|g| folder.fold_goal(g)).collect(),
    )
}

pub fn walk_clause_forall(folder: &mut impl Folder, generics: usize, clause: Clause) -> Clause {
    Clause::Forall(generics, Box::new(folder.fold_clause(clause)))
}

pub fn walk_goal(folder: &mut impl Folder, goal: Goal) -> Goal {
    match goal {
        Goal::Implies(clauses, goal) => folder.fold_goal_implies(clauses, *goal),
        Goal::Exists(vars, goal) => folder.fold_goal_exists(vars, *goal),
        Goal::Forall(vars, goal) => folder.fold_goal_forall(vars, *goal),
        Goal::Domain(domain) => folder.fold_goal_domain(domain),
    }
}

pub fn walk_goal_implies(folder: &mut impl Folder, clauses: Vec<Clause>, goal: Goal) -> Goal {
    Goal::Implies(
        clauses.into_iter().map(|c| folder.fold_clause(c)).collect(),
        Box::new(folder.fold_goal(goal)),
    )
}

pub fn walk_goal_exists(folder: &mut impl Folder, vars: usize, goal: Goal) -> Goal {
    Goal::Exists(vars, Box::new(folder.fold_goal(goal)))
}

pub fn walk_goal_forall(folder: &mut impl Folder, vars: usize, goal: Goal) -> Goal {
    Goal::Forall(vars, Box::new(folder.fold_goal(goal)))
}

pub fn walk_goal_domain(folder: &mut impl Folder, domain: DomainGoal) -> Goal {
    Goal::Domain(folder.fold_domain_goal(domain))
}

pub fn walk_domain_goal(folder: &mut impl Folder, goal: DomainGoal) -> DomainGoal {
    match goal {
        DomainGoal::InstanceExistsAndWellFormed { head, args } => {
            folder.fold_domain_instance_exists_and_well_formed(head, args)
        }
        DomainGoal::MethodBlockExists { on_type } => {
            folder.fold_domain_method_block_exists(on_type)
        }
        DomainGoal::TypeWellFormed { ty } => folder.fold_domain_type_well_formed(ty),
        DomainGoal::ClassWellFormed { head, args } => {
            folder.fold_domain_class_well_formed(head, args)
        }
    }
}

pub fn walk_domain_instance_exists_and_well_formed(
    folder: &mut impl Folder,
    head: ClassRef,
    args: Vec<Ty>,
) -> DomainGoal {
    DomainGoal::InstanceExistsAndWellFormed {
        head,
        args: args.into_iter().map(|ty| folder.fold_ty(ty)).collect(),
    }
}

pub fn walk_domain_method_block_exists(folder: &mut impl Folder, on_type: Ty) -> DomainGoal {
    DomainGoal::MethodBlockExists {
        on_type: folder.fold_ty(on_type),
    }
}

pub fn walk_domain_type_well_formed(folder: &mut impl Folder, ty: Ty) -> DomainGoal {
    DomainGoal::TypeWellFormed {
        ty: folder.fold_ty(ty),
    }
}

pub fn walk_domain_class_well_formed(
    folder: &mut impl Folder,
    head: ClassRef,
    args: Vec<Ty>,
) -> DomainGoal {
    DomainGoal::ClassWellFormed {
        head,
        args: args.into_iter().map(|ty| folder.fold_ty(ty)).collect(),
    }
}

pub fn walk_ty(folder: &mut impl Folder, ty: Ty) -> Ty {
    match ty {
        Ty::Ref(r) => folder.fold_ty_ref(r),
        Ty::Array(ty) => folder.fold_ty_array(*ty),
        Ty::Tuple(tys) => folder.fold_ty_tuple(tys),
        Ty::Fn(args, ret) => folder.fold_ty_fn(args, *ret),
        Ty::App(head, args) => folder.fold_ty_app(head, args),
        Ty::Variable(idx) => folder.fold_ty_variable(idx),
        Ty::SynthesizedConstant(idx) => folder.fold_ty_synthesized_constant(idx),
        Ty::Generic { scopes, index } => folder.fold_ty_generic(scopes, index),
    }
}

pub fn walk_ty_ref(_folder: &mut impl Folder, r: TyRef) -> Ty {
    Ty::Ref(r)
}

pub fn walk_ty_array(folder: &mut impl Folder, ty: Ty) -> Ty {
    Ty::Array(Box::new(folder.fold_ty(ty)))
}

pub fn walk_ty_tuple(folder: &mut impl Folder, tys: Vec<Ty>) -> Ty {
    Ty::Tuple(tys.into_iter().map(|ty| folder.fold_ty(ty)).collect())
}

pub fn walk_ty_fn(folder: &mut impl Folder, args: Vec<Ty>, ret: Ty) -> Ty {
    Ty::Fn(
        args.into_iter().map(|ty| folder.fold_ty(ty)).collect(),
        Box::new(folder.fold_ty(ret)),
    )
}

pub fn walk_ty_app(folder: &mut impl Folder, head: TyRef, args: Vec<Ty>) -> Ty {
    Ty::App(
        head,
        args.into_iter().map(|ty| folder.fold_ty(ty)).collect(),
    )
}

pub fn walk_ty_variable(_folder: &mut impl Folder, idx: usize) -> Ty {
    Ty::Variable(idx)
}

pub fn walk_ty_synthesized_constant(_folder: &mut impl Folder, idx: usize) -> Ty {
    Ty::SynthesizedConstant(idx)
}

pub fn walk_ty_generic(_folder: &mut impl Folder, scopes: usize, index: usize) -> Ty {
    Ty::Generic { scopes, index }
}
