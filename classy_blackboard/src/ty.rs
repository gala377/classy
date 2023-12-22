use crate::{database::UniverseIndex, goal::LabelingFunction};

/// Reference to the database
/// that cointains type information
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Ty {
    /// Types refering to the type database
    Ref(TyRef),
    Array(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    App(TyRef, Vec<Ty>),
    /// An Existential variable used in queries. Will be substituted by the
    /// possible matches.
    Variable(usize),
    /// A Universal variable, will not be substituted. Is only well formed if
    /// there exists an instantiation for all possible types matching
    /// additional constraints.
    SynthesizedConstant(usize),
    /// Placeholder for generic types in clauses expressed as an De Bruijn index
    Generic {
        scopes: usize,
        index: usize,
    },
}

/// A reference to a type into a
/// type table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstanceRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodBlockRef(pub usize);
/// A constraint on the given type
/// that has to be met
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Constraint {
    /// Type a has to be equal to type b
    Eq(Ty, Ty),
    /// Instance of a class needs to exists and be well formed
    Class(ClassRef, Vec<Ty>),
}

impl Ty {
    pub fn max_universe(&self, labeling_function: &dyn LabelingFunction) -> UniverseIndex {
        match self {
            Ty::Ref(_) => UniverseIndex::ROOT,
            Ty::Array(ty) => ty.max_universe(labeling_function),
            Ty::Tuple(tys) => tys
                .iter()
                .map(|ty| ty.max_universe(labeling_function))
                .max()
                .unwrap(),
            Ty::Fn(args, ret) => args
                .iter()
                .map(|ty| ty.max_universe(labeling_function))
                .max()
                .unwrap()
                .max(ret.max_universe(labeling_function)),
            Ty::App(_, args) => args
                .iter()
                .map(|ty| ty.max_universe(labeling_function))
                .max()
                .unwrap(),
            Ty::Variable(idx) => labeling_function.check_variable(*idx).unwrap(),
            Ty::SynthesizedConstant(idx) => labeling_function.check_constant(*idx).unwrap(),
            Ty::Generic { .. } => panic!("Generic type in normalized type"),
        }
    }

    /// Checks if the given variable is contained in the type.
    /// true if is is, false otherwise.
    pub fn occurence_check(&self, variable: usize) -> bool {
        match self {
            Ty::Ref(_) => false,
            Ty::Array(ty) => ty.occurence_check(variable),
            Ty::Tuple(tys) => tys.iter().any(|ty| ty.occurence_check(variable)),
            Ty::Fn(args, ret) => {
                args.iter().any(|ty| ty.occurence_check(variable)) || ret.occurence_check(variable)
            }
            Ty::App(_, args) => args.iter().any(|ty| ty.occurence_check(variable)),
            Ty::Variable(idx) => *idx == variable,
            Ty::SynthesizedConstant(_) => false,
            Ty::Generic { .. } => false,
        }
    }

    pub fn extract_variables(&self) -> Vec<usize> {
        match self {
            Ty::Ref(_) => vec![],
            Ty::Array(ty) => ty.extract_variables(),
            Ty::Tuple(tys) => tys
                .iter()
                .flat_map(|ty| ty.extract_variables())
                .collect::<Vec<_>>(),
            Ty::Fn(args, ret) => args
                .iter()
                .flat_map(|ty| ty.extract_variables())
                .chain(ret.extract_variables())
                .collect::<Vec<_>>(),
            Ty::App(_, args) => args
                .iter()
                .flat_map(|ty| ty.extract_variables())
                .collect::<Vec<_>>(),
            Ty::Variable(idx) => vec![*idx],
            Ty::SynthesizedConstant(_) => vec![],
            Ty::Generic { .. } => vec![],
        }
    }

    pub fn extract_constants(&self) -> Vec<usize> {
        match self {
            Ty::Ref(_) => vec![],
            Ty::Array(ty) => ty.extract_constants(),
            Ty::Tuple(tys) => tys
                .iter()
                .flat_map(|ty| ty.extract_constants())
                .collect::<Vec<_>>(),
            Ty::Fn(args, ret) => args
                .iter()
                .flat_map(|ty| ty.extract_constants())
                .chain(ret.extract_constants())
                .collect::<Vec<_>>(),
            Ty::App(_, args) => args
                .iter()
                .flat_map(|ty| ty.extract_constants())
                .collect::<Vec<_>>(),
            Ty::Variable(_) => vec![],
            Ty::SynthesizedConstant(idx) => vec![*idx],
            Ty::Generic { .. } => vec![],
        }
    }

    pub fn substitute_variable(&self, variable: usize, ty: &Ty) -> Ty {
        match self {
            Ty::Ref(_) => self.clone(),
            Ty::Array(ty) => Ty::Array(Box::new(ty.substitute_variable(variable, ty))),
            Ty::Tuple(tys) => Ty::Tuple(
                tys.iter()
                    .map(|ty| ty.substitute_variable(variable, ty))
                    .collect(),
            ),
            Ty::Fn(args, ret) => Ty::Fn(
                args.iter()
                    .map(|ty| ty.substitute_variable(variable, ty))
                    .collect(),
                Box::new(ret.substitute_variable(variable, ty)),
            ),
            Ty::App(ty_ref, args) => Ty::App(
                *ty_ref,
                args.iter()
                    .map(|ty| ty.substitute_variable(variable, ty))
                    .collect(),
            ),
            Ty::Variable(idx) => {
                if *idx == variable {
                    ty.clone()
                } else {
                    self.clone()
                }
            }
            Ty::SynthesizedConstant(_) => self.clone(),
            Ty::Generic { .. } => self.clone(),
        }
    }
}
