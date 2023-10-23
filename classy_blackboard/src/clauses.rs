/// All type information necessary for inference
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeImpl {
    pub name: String,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
    pub members: Vec<(String, Ty)>,
    pub is_class: bool,
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

impl Constraint {
    pub fn substitute(&self, generic_index: usize, with: &Ty) -> Constraint {
        match self {
            Self::Eq(t1, t2) => Self::Eq(
                t1.substitute(generic_index, with),
                t2.substitute(generic_index, with),
            ),
            Self::Class(cls, args) => Self::Class(
                cls.clone(),
                args.iter()
                    .map(|arg| arg.substitute(generic_index, with))
                    .collect(),
            ),
        }
    }
}

/// Reference to the database
/// that cointains type information
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Ty {
    // Types refering to the database
    Ref(TyRef),
    Array(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    // A generic variable inside the type
    // TODO: should support DeBruijn indexes
    Generic(usize),
    App(TyRef, Vec<Ty>),
    // An Existential variable used in queries. Will be substituted by the possible matches.
    UnBound(usize),
    // A Universal variable, will not be substituted. Is only well formed if there exists
    // an instantiation for all possible types matching additional constraints.
    Forall(usize),
}

impl Ty {
    /// Substitute a generic variable under the index `generic_index` with the
    /// type `with` and return the resulting type.
    pub fn substitute(&self, generic_index: usize, with: &Ty) -> Ty {
        match self {
            Ty::Ref(r) => Ty::Ref(*r),
            Ty::Array(t) => Ty::Array(Box::new(t.substitute(generic_index, with))),
            Ty::Tuple(ts) => Ty::Tuple(
                ts.iter()
                    .map(|t| t.substitute(generic_index, with))
                    .collect(),
            ),
            Ty::Fn(args, ret) => Ty::Fn(
                args.iter()
                    .map(|t| t.substitute(generic_index, with))
                    .collect(),
                Box::new(ret.substitute(generic_index, with)),
            ),
            Ty::Generic(index) if *index == generic_index => with.clone(),
            Ty::Generic(_) => self.clone(),
            Ty::App(head, args) => Ty::App(
                *head,
                args.iter()
                    .map(|t| t.substitute(generic_index, with))
                    .collect(),
            ),
            Ty::UnBound(_) => self.clone(),
            Ty::Forall(_) => self.clone(),
        }
    }

    /// Returns a list of all unbound variables in the type
    pub fn unbound(&self) -> Vec<usize> {
        let mut res = Vec::new();
        self.unbound_impl(&mut res);
        res
    }

    /// Walk the type recursively and collect all unbound variables into a.
    /// vector
    fn unbound_impl(&self, res: &mut Vec<usize>) {
        match self {
            Ty::Ref(_) => {}
            Ty::Array(inner) => {
                inner.unbound_impl(res);
            }
            Ty::Tuple(inner) => {
                inner.iter().for_each(|t| t.unbound_impl(res));
            }
            Ty::Fn(args, ret) => {
                args.iter().for_each(|t| t.unbound_impl(res));
                ret.unbound_impl(res);
            }
            Ty::Generic(_) => {}
            Ty::App(_, args) => args.iter().for_each(|t| t.unbound_impl(res)),
            Ty::UnBound(usize) => res.push(*usize),
            Ty::Forall(_) => {}
        }
    }
}

// there is an instance Show(Int)

/// For instance in form
/// instance for { Clone(a) } => Clone(Vec(a))
/// We would map it to
/// type_class: Clone
/// args: [ Vec(a) ]
/// type_params: [ a ]
/// constraints: [ Clone(a) ]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Instance {
    pub type_class: TyRef,
    pub args: Vec<Ty>,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
}

// does Foo(Int) hold?
