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
    /// Placeholder for generic types in clauses
    Generic(String),
}

/// A reference to a type into a
/// type table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyRef(pub usize);

/// A constraint on the given type
/// that has to be met
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Constraint {
    Eq(Ty, Ty),
    Class(TyRef, Vec<Ty>),
}
