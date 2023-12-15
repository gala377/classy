use std::collections::HashMap;

use crate::{
    clauses::Clause,
    goal::{DomainGoal, ExClause, Goal},
    slg::Substitution,
    ty::{ClassRef, Constraint, InstanceRef, MethodBlockRef, Ty, TyRef},
};

#[derive(Copy, Debug, Clone, Hash, PartialEq, Eq)]
pub struct UniverseIndex(usize);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DefId(usize);

/// All type information necessary for inference
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

pub struct TypeClass {
    pub name: String,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
    pub members: Vec<(DefId, Ty)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

pub trait VariableGenerator {
    fn next_variable(&mut self, in_universe: UniverseIndex) -> Ty;
    fn next_constant(&mut self, in_universe: UniverseIndex) -> Ty;
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

    pub fn add_type_impl(&mut self, type_impl: TypeImpl) -> TyRef {
        self.names_to_ty
            .insert(type_impl.name.clone(), self.type_impls.len());
        self.type_impls.push(type_impl);
        TyRef(self.type_impls.len() - 1)
    }

    pub fn add_class(&mut self, type_class: TypeClass) -> ClassRef {
        self.names_to_ty
            .insert(type_class.name.clone(), self.type_classes.len());
        self.type_classes.push(type_class);
        ClassRef(self.type_classes.len() - 1)
    }

    pub fn add_instance(&mut self, instance: Instance) {
        self.instances.push(instance);
    }

    pub fn add_method_block(&mut self, method_block: MethodsBlock) {
        self.method_blocks.push(method_block);
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
}

impl Database {
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
            // ! of there is no paramerter we will not increase the universe
            type_params.clone(),
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
            return Clause::Fact(DomainGoal::ClassExists(class_ref));
        }
        // we know we have at some type arguments
        let clauses = constraints
            .iter()
            .map(|c| self.lower_constraint(c))
            .collect::<Vec<_>>();
        if clauses.is_empty() {
            Clause::Forall(
                type_params.clone(),
                Box::new(Clause::Fact(DomainGoal::ClassExists(class_ref))),
            )
        } else {
            Clause::Forall(
                type_params.clone(),
                Box::new(Clause::Implies(
                    Box::new(Clause::Fact(DomainGoal::ClassExists(class_ref))),
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
            // ! of there is no paramerter we will not increase the universe
            type_params.clone(),
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
            return Clause::Fact(DomainGoal::TypeImplExists(type_ref.clone()));
        }
        // we know we have at some type arguments
        let clauses = constraints
            .iter()
            .map(|c| self.lower_constraint(c))
            .collect::<Vec<_>>();
        if clauses.is_empty() {
            Clause::Forall(
                type_params.clone(),
                Box::new(Clause::Fact(DomainGoal::TypeImplExists(type_ref.clone()))),
            )
        } else {
            Clause::Forall(
                type_params.clone(),
                Box::new(Clause::Implies(
                    Box::new(Clause::Fact(DomainGoal::TypeImplExists(type_ref.clone()))),
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
    pub fn find_matching(
        &self,
        goal: &Goal,
        current_universe: UniverseIndex,
        variable_generator: &mut dyn VariableGenerator,
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
        for clause in assumptions {
            self.match_clause(&clause, &goal, current_universe, variable_generator, None)
                .map(|match_result| results.push(match_result));
        }
        for annotated_clause in &self.clauses {
            self.match_clause(
                &annotated_clause.clause,
                &goal,
                current_universe,
                variable_generator,
                Some(annotated_clause.origin.clone()),
            )
            .map(|match_result| results.push(match_result));
        }
        results
    }

    fn match_clause(
        &self,
        clause: &Clause,
        goal: &DomainGoal,
        current_universe: UniverseIndex,
        variable_generator: &mut dyn VariableGenerator,
        origin: Option<GenericRef>,
    ) -> Option<MatchResult> {
        let clause = self.normalize_clause(&clause, current_universe, variable_generator);
        // extract the inner domain foal and the body of a clause
        let (raw_clause, body) = match clause {
            Clause::Implies(box Clause::Fact(fact), body) => (fact, body),
            Clause::Fact(fact) => (fact, Vec::new()),
            _ => panic!("Clause is not normalized: {clause:?}"),
        };
        self.unify(goal, &raw_clause)
            .map(|substitution| MatchResult {
                exclause: ExClause {
                    head: Goal::Domain(raw_clause),
                    subgoals: body,
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
        variable_generator: &mut dyn VariableGenerator,
    ) -> Clause {
        let mut clause = clause.clone();
        while let Clause::Forall(generics, next_clause) = clause {
            let substitution = generics
                .iter()
                .cloned()
                .map(|g| (g, variable_generator.next_variable(current_universe)))
                .collect();
            clause = next_clause.substitute_generics(&substitution);
        }
        clause
    }

    fn unify(&self, _goal: &DomainGoal, _clause: &DomainGoal) -> Option<Substitution> {
        todo!()
    }

    fn unify_ty(&self, _goal: &Ty, _clause: &Ty) -> Option<Substitution> {
        todo!()
    }
}
