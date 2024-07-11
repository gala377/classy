use std::collections::HashMap;

use tracing::info;

use crate::{
    clauses::Clause,
    fold::Folder,
    goal::{DomainGoal, ExClause, Goal, LabelingFunction},
    normalizer::ClauseNormalizer,
    slg::Substitution,
    ty::{ClassRef, Constraint, InstanceRef, MethodBlockRef, Ty, TyRef},
};

#[derive(Copy, Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct UniverseIndex(usize);

impl UniverseIndex {
    pub const ROOT: UniverseIndex = UniverseIndex(0);
    pub fn next(&self) -> UniverseIndex {
        UniverseIndex(self.0 + 1)
    }

    pub fn collapse(&self, by: UniverseIndex) -> UniverseIndex {
        assert!(self.0 >= by.0);
        UniverseIndex(self.0 - by.0)
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DefId(pub usize);

/// All type information necessary for inference
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct TypeImpl {
    pub name: String,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
    pub fields: Vec<(String, Ty)>,
}

pub struct MethodsBlock {
    pub name: Option<String>,
    pub methods: Vec<Definition>,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
    pub on_type: Ty,
}

#[derive(Default)]
pub struct TypeClass {
    pub name: String,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
    pub members: Vec<(String, DefId, Ty)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct Instance {
    pub type_class: ClassRef,
    pub args: Vec<Ty>,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
}

pub struct Definition {
    pub type_params: Vec<String>,
    pub name: String,
    pub ty: Ty,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GenericRef {
    Type(TyRef),
    Class(ClassRef),
    Instance(InstanceRef),
    MethodBlock(MethodBlockRef),
}

pub trait VariableContext {
    fn next_variable(&mut self, in_universe: UniverseIndex) -> Ty;
    fn next_constant(&mut self, in_universe: UniverseIndex) -> Ty;
    fn labeling_function(&mut self) -> &mut dyn LabelingFunction;
}

struct AnnotatedClause {
    /// A clause translated from some source like type impl or an instance
    pub clause: Clause,

    /// A reference to the origin of this clause in the database
    ///
    /// ! This is important because we want the answer to also return the
    /// ! origins of all the definitions that have been used to prove the
    /// ! goal so that they can be used by the compiler to generate code
    pub origin: GenericRef,
}

#[derive(Default)]
pub struct Database {
    type_impls: Vec<TypeImpl>,
    type_classes: Vec<TypeClass>,
    instances: Vec<Instance>,
    method_blocks: Vec<MethodsBlock>,
    names_to_ty: HashMap<String, usize>,

    /// Those clauses are in their raw form and need to be normalized
    /// to form without forall. This means that the generic types need to be
    /// replaced by variables, and those variables need to be denoted in the
    /// labeling function.
    clauses: Vec<AnnotatedClause>,
}

impl Database {
    pub fn new() -> Self {
        Database::default()
    }

    pub fn reserve_type_impl(&mut self, name: &str) -> TyRef {
        let mut type_impl = TypeImpl::default();
        type_impl.name = name.to_string();
        self.add_type_impl(type_impl)
    }

    pub fn add_type_impl(&mut self, type_impl: TypeImpl) -> TyRef {
        self.names_to_ty
            .insert(type_impl.name.clone(), self.type_impls.len());
        self.type_impls.push(type_impl);
        TyRef(self.type_impls.len() - 1)
    }

    pub fn replace_type_impl(&mut self, ty: TyRef, type_impl: TypeImpl) {
        self.type_impls[ty.0] = type_impl;
    }

    pub fn reserve_class(&mut self, name: &str) -> ClassRef {
        let mut type_class = TypeClass::default();
        type_class.name = name.to_string();
        self.add_class(type_class)
    }

    pub fn replace_class(&mut self, class: ClassRef, type_class: TypeClass) {
        self.type_classes[class.0] = type_class;
    }

    pub fn add_class(&mut self, type_class: TypeClass) -> ClassRef {
        self.names_to_ty
            .insert(type_class.name.clone(), self.type_classes.len());
        self.type_classes.push(type_class);
        ClassRef(self.type_classes.len() - 1)
    }

    pub fn reserve_instance(&mut self) -> InstanceRef {
        self.add_instance(Instance::default())
    }

    pub fn add_instance(&mut self, instance: Instance) -> InstanceRef {
        self.instances.push(instance);
        InstanceRef(self.instances.len() - 1)
    }

    pub fn replace_instance(&mut self, instance: InstanceRef, instance_def: Instance) {
        self.instances[instance.0] = instance_def;
    }

    pub fn add_method_block(&mut self, method_block: MethodsBlock) -> MethodBlockRef {
        self.method_blocks.push(method_block);
        MethodBlockRef(self.method_blocks.len() - 1)
    }

    pub fn get_method_block(&self, method_block: GenericRef) -> &MethodsBlock {
        match method_block {
            GenericRef::MethodBlock(method_block) => &self.method_blocks[method_block.0],
            _ => panic!("Expected method block"),
        }
    }

    pub fn get_instance(&self, instance: GenericRef) -> &Instance {
        match instance {
            GenericRef::Instance(reference) => &self.instances[reference.0],
            _ => panic!("Expected instance"),
        }
    }

    pub fn typeref_from_name(&self, name: &str) -> Option<TyRef> {
        self.names_to_ty.get(name).map(|i| TyRef(*i))
    }

    pub fn class_from_name(&self, name: &str) -> Option<ClassRef> {
        self.names_to_ty.get(name).map(|i| ClassRef(*i))
    }

    pub fn get_class(&self, class: ClassRef) -> &TypeClass {
        &self.type_classes[class.0]
    }

    pub fn get_type_impl(&self, ty: TyRef) -> &TypeImpl {
        &self.type_impls[ty.0]
    }

    pub fn get_class_mut(&mut self, class: ClassRef) -> &mut TypeClass {
        &mut self.type_classes[class.0]
    }

    pub fn get_type_impl_mut(&mut self, ty: TyRef) -> &mut TypeImpl {
        &mut self.type_impls[ty.0]
    }

    /// This method needs to be called after database has been filled with
    /// all the declarations. It will lower all the declarations into clauses
    /// that they can be used by the solver.
    pub fn lower_to_clauses(&mut self) {
        for (type_ref, type_impl) in self.type_impls.iter().enumerate() {
            let type_ref = TyRef(type_ref);
            let clause = self.lower_type_impl(type_ref.clone(), type_impl);
            self.clauses.push(AnnotatedClause {
                clause,
                origin: GenericRef::Type(type_ref.clone()),
            });
        }
        for (class_ref, class) in self.type_classes.iter().enumerate() {
            let class_ref = ClassRef(class_ref);
            let clause = self.lower_class(class_ref.clone(), class);
            self.clauses.push(AnnotatedClause {
                clause,
                origin: GenericRef::Class(class_ref.clone()),
            });
        }
        for (instance_ref, instance) in self.instances.iter().enumerate() {
            let instance_ref = InstanceRef(instance_ref);
            let clause = self.lower_instance(instance);
            self.clauses.push(AnnotatedClause {
                clause,
                origin: GenericRef::Instance(instance_ref.clone()),
            });
        }
        for (method_block_ref, method_block) in self.method_blocks.iter().enumerate() {
            let method_block_ref = MethodBlockRef(method_block_ref);
            let clause = self.lower_methods_block(method_block);
            self.clauses.push(AnnotatedClause {
                clause,
                origin: GenericRef::MethodBlock(method_block_ref.clone()),
            });
        }
    }

    fn lower_methods_block(
        &self,
        MethodsBlock {
            type_params,
            constraints,
            on_type,
            ..
        }: &MethodsBlock,
    ) -> Clause {
        let constraints_as_clauses = constraints
            .iter()
            .map(|c| self.lower_constraint_into_clause(c))
            .collect::<Vec<_>>();
        let clauses = constraints
            .iter()
            .map(|c| self.lower_constraint(c))
            .chain(std::iter::once(Goal::Implies(
                constraints_as_clauses,
                Box::new(Goal::Domain(DomainGoal::TypeWellFormed {
                    ty: on_type.clone(),
                })),
            )))
            .collect::<Vec<_>>();
        Clause::Forall(
            // ! This is fine.
            // ! Because there can be multiple binders we will increase the universe one per binder
            // ! if there is no paramerter we will not increase the universe
            type_params.len(),
            Box::new(Clause::Implies(
                Box::new(Clause::Fact(DomainGoal::MethodBlockExists {
                    on_type: on_type.clone(),
                })),
                clauses,
            )),
        )
    }

    fn lower_class(
        &self,
        class_ref: ClassRef,
        TypeClass {
            type_params,
            constraints,
            ..
        }: &TypeClass,
    ) -> Clause {
        if type_params.is_empty() {
            assert!(
                constraints.is_empty(),
                "if there are no type params there are no types to constraint"
            );
            return Clause::Fact(DomainGoal::ClassWellFormed {
                head: class_ref.clone(),
                args: Vec::new(),
            });
        }
        // we know we have at some type arguments
        let clauses = constraints
            .iter()
            .map(|c| self.lower_constraint(c))
            .collect::<Vec<_>>();
        if clauses.is_empty() {
            Clause::Forall(
                type_params.len(),
                Box::new(Clause::Fact(DomainGoal::ClassWellFormed {
                    head: class_ref,
                    args: type_params
                        .iter()
                        .enumerate()
                        .map(|(index, _)| Ty::Generic { scopes: 0, index })
                        .collect(),
                })),
            )
        } else {
            Clause::Forall(
                type_params.len(),
                Box::new(Clause::Implies(
                    Box::new(Clause::Fact(DomainGoal::ClassWellFormed {
                        head: class_ref,
                        args: type_params
                            .iter()
                            .enumerate()
                            .map(|(index, _)| Ty::Generic { scopes: 0, index })
                            .collect(),
                    })),
                    clauses,
                )),
            )
        }
    }

    fn lower_instance(
        &self,
        Instance {
            type_class,
            args,
            type_params,
            constraints,
        }: &Instance,
    ) -> Clause {
        let constraints_as_clauses = constraints
            .iter()
            .map(|c| self.lower_constraint_into_clause(c))
            .collect::<Vec<_>>();
        let clauses = constraints
            .iter()
            .map(|c| self.lower_constraint(c))
            .chain(args.iter().map(|arg| {
                // we generate well formed queries. Basically we need to ask if
                // the type is even well formed to consider it for the instance
                // TODO: If we can prove in an earlier pass that every instance is
                // TODO: well formed then we can remove this query
                Goal::Implies(
                    constraints_as_clauses.clone(),
                    Box::new(Goal::Domain(DomainGoal::TypeWellFormed { ty: arg.clone() })),
                )
            }))
            .collect::<Vec<_>>();
        Clause::Forall(
            // ! This is fine.
            // ! Because there can be multiple binders we will increase the universe one per binder
            // ! if there is no paramerter we will not increase the universe
            type_params.len(),
            Box::new(Clause::Implies(
                Box::new(Clause::Fact(DomainGoal::InstanceExistsAndWellFormed {
                    head: type_class.clone(),
                    args: args.clone(),
                })),
                clauses,
            )),
        )
    }

    fn lower_type_impl(
        &self,
        type_ref: TyRef,
        TypeImpl {
            type_params,
            constraints,
            ..
        }: &TypeImpl,
    ) -> Clause {
        if type_params.is_empty() {
            assert!(
                constraints.is_empty(),
                "if there are no type params there are no types to constraint"
            );
            return Clause::Fact(DomainGoal::TypeWellFormed {
                ty: Ty::Ref(type_ref),
            });
        }
        // we know we have at some type arguments
        let clauses = constraints
            .iter()
            .map(|c| self.lower_constraint(c))
            .collect::<Vec<_>>();
        if clauses.is_empty() {
            Clause::Forall(
                type_params.len(),
                Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                    ty: Ty::App(
                        Box::new(Ty::Ref(type_ref)),
                        type_params
                            .iter()
                            .enumerate()
                            .map(|(index, _)| Ty::Generic { scopes: 0, index })
                            .collect(),
                    ),
                })),
            )
        } else {
            Clause::Forall(
                type_params.len(),
                Box::new(Clause::Implies(
                    Box::new(Clause::Fact(DomainGoal::TypeWellFormed {
                        ty: Ty::App(
                            Box::new(Ty::Ref(type_ref)),
                            type_params
                                .iter()
                                .enumerate()
                                .map(|(index, _)| Ty::Generic { scopes: 0, index })
                                .collect(),
                        ),
                    })),
                    clauses,
                )),
            )
        }
    }

    fn lower_constraint_into_clause(&self, constraint: &Constraint) -> Clause {
        match constraint {
            Constraint::Eq(..) => unimplemented!(),
            Constraint::Class(class, args) => {
                Clause::Fact(DomainGoal::InstanceExistsAndWellFormed {
                    head: class.clone(),
                    args: args.clone(),
                })
            }
        }
    }

    fn lower_constraint(&self, constraint: &Constraint) -> Goal {
        match constraint {
            Constraint::Eq(..) => unimplemented!(),
            Constraint::Class(class, args) => {
                Goal::Domain(DomainGoal::InstanceExistsAndWellFormed {
                    head: class.clone(),
                    args: args.clone(),
                })
            }
        }
    }
}

pub struct MatchResult {
    /// Exclause derived from the matching clause
    pub exclause: ExClause,
    pub substitution: Substitution,
    pub origin: Option<GenericRef>,
}

impl Database {
    /// Union given goal with all clauses and return
    /// resulting exclauses alongside a substitution that has been made in order
    /// to make the union possible.
    ///
    /// goal: The goal to unify with
    /// variable_generator:
    ///     A generator that will be used to generate new variables when
    ///     normalizing the clauses, it should adjust the labeling function
    ///     itself.
    #[tracing::instrument(skip(self, variable_generator))]
    pub fn find_matching(
        &self,
        goal: &Goal,
        current_universe: UniverseIndex,
        variable_generator: &mut dyn VariableContext,
    ) -> Vec<MatchResult> {
        let mut goal = goal.clone();
        let mut assumptions = Vec::new();
        while let Goal::Implies(mut assumption, next_goal) = goal {
            assumptions.append(&mut assumption);
            goal = *next_goal;
        }
        let goal = match goal {
            Goal::Domain(domain_goal) => domain_goal,
            g => panic!("Goal is not normalized: {g:?}"),
        };
        let mut results = Vec::new();
        for clause in &assumptions {
            info!("Matching assumption: {:?}", clause);
            self.match_clause(clause, &goal, current_universe, variable_generator, None)
                .map(|match_result| results.push(match_result));
        }
        for annotated_clause in &self.clauses {
            info!("Matching clause: {:?}", annotated_clause.clause);
            self.match_clause(
                &annotated_clause.clause,
                &goal,
                current_universe,
                variable_generator,
                Some(annotated_clause.origin.clone()),
            )
            .map(|mut match_result| {
                info!("Clause matched");
                if !assumptions.is_empty() {
                    match_result.exclause.subgoals.iter_mut().for_each(|goal| {
                        *goal = Goal::Implies(assumptions.clone(), Box::new(goal.clone()))
                    });
                }
                results.push(match_result)
            });
        }
        results
    }

    fn match_clause(
        &self,
        clause: &Clause,
        goal: &DomainGoal,
        current_universe: UniverseIndex,
        variable_generator: &mut dyn VariableContext,
        origin: Option<GenericRef>,
    ) -> Option<MatchResult> {
        let (clause, unmap) = self.normalize_clause(&clause, current_universe, variable_generator);
        // extract the inner domain foal and the body of a clause
        let (raw_clause, body) = match clause {
            Clause::Implies(box Clause::Fact(fact), body) => (fact, body),
            Clause::Fact(fact) => (fact, Vec::new()),
            _ => panic!("Clause is not normalized: {clause:?}"),
        };
        self.unify(goal, &raw_clause, variable_generator, origin.clone())
            .map(|substitution| MatchResult {
                exclause: ExClause {
                    head: Goal::Domain(raw_clause),
                    subgoals: body,
                    unmap: unmap.clone().unwrap_or_default(),
                },
                substitution,
                origin: origin.clone(),
            })
    }

    /// Remove any universal quantifiers and replace their respective variable
    /// uses with a variable in the given universe.
    fn normalize_clause(
        &self,
        clause: &Clause,
        current_universe: UniverseIndex,
        variable_generator: &mut dyn VariableContext,
    ) -> (Clause, Option<Vec<Ty>>) {
        let mut normalizer = ClauseNormalizer::new(variable_generator, current_universe);
        (
            normalizer.fold_clause(clause.clone()),
            normalizer.initial_generics_subst,
        )
    }

    fn unify(
        &self,
        goal: &DomainGoal,
        clause: &DomainGoal,
        variable_generator: &mut dyn VariableContext,
        origin: Option<GenericRef>,
    ) -> Option<Substitution> {
        let mut stack = vec![(goal.clone(), clause.clone())];
        let mut substitution = Substitution::new();
        macro_rules! subst_stack {
            () => {
                let mut substitutor = $crate::substitutor::VariableSubstitutor {
                    substitutions: &substitution.mapping,
                };
                for (g1, g2) in &mut stack {
                    *g1 = substitutor.fold_domain_goal(g1.clone());
                    *g2 = substitutor.fold_domain_goal(g2.clone());
                }
            };
        }
        while let Some((g1, g2)) = stack.pop() {
            use DomainGoal::*;
            match (g1, g2) {
                (TypeWellFormed { ty: ty1 }, TypeWellFormed { ty: ty2 }) => {
                    self.unify_ty(
                        &ty1,
                        &ty2,
                        &mut substitution,
                        variable_generator,
                        origin.clone(),
                    )?;
                    subst_stack!();
                }
                (
                    ClassWellFormed {
                        head: head1,
                        args: args1,
                    },
                    ClassWellFormed {
                        head: head2,
                        args: args2,
                    },
                ) if head1 == head2 && args1.len() == args2.len() => {
                    for (a1, a2) in args1.iter().zip(args2.iter()) {
                        self.unify_ty(
                            a1,
                            a2,
                            &mut substitution,
                            variable_generator,
                            origin.clone(),
                        )?;
                        subst_stack!();
                    }
                }
                (
                    InstanceExistsAndWellFormed {
                        head: head1,
                        args: args1,
                    },
                    InstanceExistsAndWellFormed {
                        head: head2,
                        args: args2,
                    },
                ) if head1 == head2 && args1.len() == args2.len() => {
                    for (a1, a2) in args1.iter().zip(args2.iter()) {
                        self.unify_ty(
                            a1,
                            a2,
                            &mut substitution,
                            variable_generator,
                            origin.clone(),
                        )?;
                        subst_stack!();
                    }
                }
                (MethodBlockExists { on_type: ty1 }, MethodBlockExists { on_type: ty2 }) => {
                    self.unify_ty(
                        &ty1,
                        &ty2,
                        &mut substitution,
                        variable_generator,
                        origin.clone(),
                    )?;
                    subst_stack!();
                }
                (
                    FindMethod { name, on_type },
                    MethodBlockExists {
                        on_type: target_type,
                    },
                ) => {
                    let method_found = match origin.clone() {
                        Some(reference @ GenericRef::MethodBlock(_)) => self
                            .get_method_block(reference)
                            .methods
                            .iter()
                            .find(|def| def.name == *name)
                            .is_some(),
                        _ => false,
                    };
                    if !method_found {
                        return None;
                    }
                    self.unify_ty(
                        &on_type,
                        &target_type,
                        &mut substitution,
                        variable_generator,
                        origin.clone(),
                    )?;
                    subst_stack!();
                }
                _ => return None,
            }
        }
        Some(substitution)
    }

    fn unify_ty(
        &self,
        t1: &Ty,
        t2: &Ty,
        substitutions: &mut Substitution,
        variable_generator: &mut dyn VariableContext,
        origin: Option<GenericRef>,
    ) -> Option<()> {
        let mut stack = vec![(t1.clone(), t2.clone())];

        macro_rules! subst_stack {
            ($var:expr, $for_t:expr) => {
                for (t1, t2) in stack.iter_mut() {
                    *t1 = t1.substitute_variable($var, $for_t);
                    *t2 = t2.substitute_variable($var, $for_t);
                }
            };
        }

        macro_rules! subst_substitutions {
            ($var:expr, $for_t:expr) => {
                for t in substitutions.mapping.values_mut() {
                    *t = t.substitute_variable($var, $for_t);
                }
            };
        }
        while let Some((t1, t2)) = stack.pop() {
            use Ty::*;
            match (t1, t2) {
                (Ref(r1), Ref(r2)) if r1 == r2 => {}
                (Array(t1), Array(t2)) => stack.push((*t1, *t2)),
                (Tuple(t1), Tuple(t2)) if t1.len() == t2.len() => {
                    stack.extend(t1.into_iter().zip(t2.into_iter()));
                }
                (Fn(args1, ret1), Fn(args2, ret2)) if args1.len() == args2.len() => {
                    stack.extend(args1.into_iter().zip(args2.into_iter()));
                    stack.push((*ret1, *ret2));
                }
                (Variable(idx1), Variable(idx2)) if idx1 == idx2 => {}
                (SynthesizedConstant(idx1), SynthesizedConstant(idx2)) if idx1 == idx2 => {}
                (Generic { .. }, _) | (_, Generic { .. }) => {
                    panic!("Unexpected generic type in normalized type")
                }
                // app { scheme<X> { () -> X }, Int }
                (App(ty_1, args_1), App(ty_2, args_2)) if args_1.len() == args_2.len() => {
                    stack.push((ty_1.as_ref().clone(), ty_2.as_ref().clone()));
                    stack.extend(args_1.into_iter().zip(args_2.into_iter()));
                }
                (SynthesizedConstant(c), Variable(v)) | (Variable(v), SynthesizedConstant(c)) => {
                    let c_universe = variable_generator
                        .labeling_function()
                        .check_constant(c)
                        .unwrap();
                    let v_universe = variable_generator
                        .labeling_function()
                        .check_variable(v)
                        .unwrap();
                    if c_universe > v_universe {
                        println!("Type unification failed because universe check failed");
                        return None;
                    }
                    let for_t = SynthesizedConstant(c);
                    subst_stack!(v, &for_t);
                    subst_substitutions!(v, &for_t);
                    substitutions.add(v, for_t, origin.clone());
                }
                (Variable(idx), t) | (t, Variable(idx)) => {
                    if t.occurence_check(idx) {
                        println!("Type unification failed because of occurence check");
                        return None;
                    }
                    let constants = t.extract_constants();
                    let v_universe = variable_generator
                        .labeling_function()
                        .check_variable(idx)
                        .unwrap();
                    for c in constants {
                        let c_universe = variable_generator
                            .labeling_function()
                            .check_constant(c)
                            .unwrap();
                        if c_universe > v_universe {
                            println!("Type unification failed because universe check failed");
                            return None;
                        }
                    }
                    let variables = t.extract_variables();
                    let adjusted_universe = variables.iter().fold(v_universe, |acc, v| {
                        let v_universe = variable_generator
                            .labeling_function()
                            .check_variable(*v)
                            .unwrap();
                        acc.min(v_universe)
                    });
                    variable_generator
                        .labeling_function()
                        .adjust_universe(idx, adjusted_universe);
                    for v in variables {
                        variable_generator
                            .labeling_function()
                            .adjust_universe(v, adjusted_universe);
                    }

                    subst_stack!(idx, &t);
                    subst_substitutions!(idx, &t);
                    substitutions.add(idx, t, origin.clone());
                }
                _ => return None,
            }
        }
        Some(())
    }
}
