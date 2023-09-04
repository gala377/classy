use std::collections::HashMap;

use crate::database::Goal;

/// All type information necessary for inference
#[derive(Debug)]
pub struct TypeImpl {
    pub name: String,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
    pub members: HashMap<String, Ty>,
    pub is_class: bool,
}

/// A reference to a type into a
/// type table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyRef(pub usize);

/// A constraint on the given type
/// that has to be met
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty {
    Ref(TyRef),
    Array(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Generic(usize),
    App(TyRef, Vec<Ty>),
}

impl Ty {
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
        }
    }
}

impl Into<Vec<Goal>> for Ty {
    fn into(self) -> Vec<Goal> {
        match self {
            Ty::Ref(_) => vec![],
            Ty::Array(inner) => (*inner).into(),
            Ty::Tuple(inner) => inner
                .into_iter()
                .flat_map(|t| <Self as Into<Vec<Goal>>>::into(t))
                .collect(),
            Ty::Fn(args, ret) => {
                let mut goals: Vec<Goal> = (*ret).into();
                for arg in args {
                    goals.extend(<Self as Into<Vec<Goal>>>::into(arg));
                }
                goals
            }
            Ty::Generic(_) => vec![],
            Ty::App(head, tail) => {
                vec![Goal {
                    head,
                    args: tail.clone(),
                }]
            }
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
#[derive(Debug, Clone)]
pub struct Instance {
    pub type_class: TyRef,
    pub args: Vec<Ty>,
    pub type_params: Vec<String>,
    pub constraints: Vec<Constraint>,
}

// does Foo(Int) hold?
