use std::ops::{Add, AddAssign, Sub, SubAssign};

use crate::{
    knowledge::{Database, QueryError},
    typecheck::type_context::{Name, TypeId},
};

use super::type_context::TypCtx;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Type {
    Int,
    UInt,
    Bool,
    String,
    Float,
    Unit,
    Byte,
    Struct {
        def: TypeId,
        // maps fields to the TypeId of the type
        fields: Vec<(Name, Type)>,
    },
    ADT {
        def: TypeId,
        // maps constructors to the TypeId of the type
        // can be a tuple type
        constructors: Vec<(Name, Type)>,
    },
    Function {
        args: Vec<Type>,
        ret: Box<Type>,
    },
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Alias(TypeId),
    /// Used for expressions that diverge the execution flow, like "return".
    /// Equal to any type.
    Divergent,
    ToInfere,
    /// A type schema, generics combines with the type they apply to
    Scheme {
        prefex: Vec<Name>,
        typ: Box<Type>,
    },
    /// An apliication of types to a Scheme or some other type in which case
    /// the arguments should be empty
    App {
        typ: Box<Type>,
        args: Vec<Type>,
    },

    /// Reference to the generic type under index `usize` in the definition
    /// within the DeBrujin index.
    Generic(DeBruijn, usize),
    /// Temporary type used onlu for type inference
    Fresh(usize),
}

impl Type {
    pub fn is_ref(&self) -> Option<bool> {
        match self {
            Type::Int
            | Type::UInt
            | Type::Bool
            | Type::Float
            | Type::Divergent
            | Type::Unit
            | Type::Byte => Some(false),

            Type::String
            | Type::Struct { .. }
            | Type::ADT { .. }
            | Type::Function { .. }
            | Type::Tuple(_)
            | Type::Array(_)
            | Type::Scheme { .. }
            // almost certain we will not have aliases to primitive types
            | Type::Alias(_)
            | Type::Generic(_, _) => Some(true),
            Type::Fresh(_) | Type::ToInfere => None,
            Type::App { typ, .. } => typ.is_ref(),
        }
    }

    /// Checks if the type is builtin type. In case it cannot be determined
    /// (like with generics, fresh types, functions and so on and so forth)
    /// returns None.
    pub fn is_builtin(&self) -> Option<bool> {
        match self {
            Type::Int
            | Type::UInt
            | Type::Bool
            | Type::Float
            | Type::Divergent
            | Type::Unit
            | Type::Byte
            | Type::String => Some(true),

            Type::Struct { .. } | Type::ADT { .. } => Some(false),
            _ => None,
        }
    }
    pub fn align(&self) -> usize {
        if self.is_ref().unwrap() {
            return std::mem::align_of::<usize>();
        }
        match self {
            Type::Bool | Type::Byte => 1,
            Type::Int | Type::UInt | Type::Float | Type::Unit => std::mem::align_of::<usize>(),
            t => panic!("cannot get the alignment of the type {t:?}"),
        }
    }

    pub fn byte_size(&self) -> usize {
        if self.is_ref().unwrap() {
            return std::mem::size_of::<usize>();
        }
        match self {
            Type::Bool | Type::Byte => 1,
            Type::Int | Type::UInt | Type::Float | Type::Unit => std::mem::size_of::<usize>(),
            t => panic!("cannot get the size of the type {t:?}"),
        }
    }

    pub fn assert_resolved(self) -> Type {
        match self {
            Type::Alias(_) => panic!("type is not resolved"),
            t => t,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct DeBruijn(pub isize);

impl DeBruijn {
    pub fn zero() -> Self {
        Self(0)
    }
}

impl Add<isize> for DeBruijn {
    type Output = Self;

    fn add(self, rhs: isize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl Sub<isize> for DeBruijn {
    type Output = Self;

    fn sub(self, rhs: isize) -> Self::Output {
        Self(self.0 - rhs)
    }
}

impl AddAssign<isize> for DeBruijn {
    fn add_assign(&mut self, rhs: isize) {
        self.0 += rhs;
    }
}

impl SubAssign<isize> for DeBruijn {
    fn sub_assign(&mut self, rhs: isize) {
        self.0 -= rhs;
    }
}

pub trait TypeFolder: Sized {
    type Error: std::fmt::Debug;

    fn fold_type(&mut self, typ: Type) -> Result<Type, Self::Error> {
        fold_type(self, typ)
    }

    fn fold_int(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::Int)
    }

    fn fold_byte(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::Byte)
    }

    fn fold_uint(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::UInt)
    }

    fn fold_divergent(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::Divergent)
    }

    fn fold_alias(&mut self, for_type: usize) -> Result<Type, Self::Error> {
        Result::Ok(Type::Alias(for_type))
    }

    fn fold_bool(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::Bool)
    }
    fn fold_float(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::Float)
    }
    fn fold_unit(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::Unit)
    }
    fn fold_string(&mut self) -> Result<Type, Self::Error> {
        Result::Ok(Type::String)
    }

    fn fold_tuple(&mut self, types: Vec<Type>) -> Result<Type, Self::Error> {
        fold_tuple(self, types)
    }

    fn fold_to_infere(&mut self) -> Result<Type, Self::Error> {
        Ok(Type::ToInfere)
    }

    fn fold_fresh(&mut self, id: usize) -> Result<Type, Self::Error> {
        Ok(Type::Fresh(id))
    }

    fn fold_generic(&mut self, debruijn: DeBruijn, id: usize) -> Result<Type, Self::Error> {
        Ok(Type::Generic(debruijn, id))
    }

    fn fold_function(&mut self, args: Vec<Type>, ret: Type) -> Result<Type, Self::Error> {
        fold_function(self, args, ret)
    }

    fn fold_struct(
        &mut self,
        def: usize,
        fields: Vec<(String, Type)>,
    ) -> Result<Type, Self::Error> {
        fold_struct(self, def, fields)
    }

    fn fold_array(&mut self, arr_t: Type) -> Result<Type, Self::Error> {
        fold_array(self, arr_t)
    }

    fn fold_scheme(&mut self, prefex: Vec<Name>, typ: Type) -> Result<Type, Self::Error> {
        fold_scheme(self, prefex, typ)
    }

    fn fold_application(&mut self, typ: Type, args: Vec<Type>) -> Result<Type, Self::Error> {
        fold_application(self, typ, args)
    }
    fn fold_adt(
        &mut self,
        def: usize,
        constructors: Vec<(String, Type)>,
    ) -> Result<Type, Self::Error> {
        fold_adt(self, def, constructors)
    }
}

pub fn fold_scheme<T: TypeFolder>(
    folder: &mut T,
    prefex: Vec<Name>,
    typ: Type,
) -> Result<Type, T::Error> {
    Ok(Type::Scheme {
        prefex,
        typ: Box::new(folder.fold_type(typ)?),
    })
}

pub fn fold_type<T: TypeFolder>(folder: &mut T, typ: Type) -> Result<Type, T::Error> {
    match typ {
        Type::Int => folder.fold_int(),
        Type::UInt => folder.fold_uint(),
        Type::Bool => folder.fold_bool(),
        Type::String => folder.fold_string(),
        Type::Float => folder.fold_float(),
        Type::Unit => folder.fold_unit(),
        Type::Byte => folder.fold_byte(),
        Type::Struct { def, fields } => folder.fold_struct(def, fields),
        Type::ADT { def, constructors } => folder.fold_adt(def, constructors),
        Type::Function { args, ret } => folder.fold_function(args, *ret),
        Type::Tuple(types) => folder.fold_tuple(types),
        Type::Array(inner) => folder.fold_array(*inner),
        Type::Alias(for_type) => folder.fold_alias(for_type),
        Type::Divergent => folder.fold_divergent(),
        Type::ToInfere => folder.fold_to_infere(),
        Type::Scheme { prefex, typ } => folder.fold_scheme(prefex, *typ),
        Type::Generic(debruijn, id) => folder.fold_generic(debruijn, id),
        Type::Fresh(id) => folder.fold_fresh(id),
        Type::App { typ, args } => folder.fold_application(*typ, args),
    }
}

pub fn fold_tuple<T: TypeFolder>(folder: &mut T, types: Vec<Type>) -> Result<Type, T::Error> {
    let types = types
        .into_iter()
        .map(|t| folder.fold_type(t))
        .collect::<Result<_, _>>()?;
    Ok(Type::Tuple(types))
}

pub fn fold_function<T: TypeFolder>(
    folder: &mut T,
    args: Vec<Type>,
    ret: Type,
) -> Result<Type, T::Error> {
    let args = args
        .into_iter()
        .map(|t| folder.fold_type(t))
        .collect::<Result<_, _>>()?;
    let ret = folder.fold_type(ret)?;
    Ok(Type::Function {
        args,
        ret: Box::new(ret),
    })
}

pub fn fold_struct<T: TypeFolder>(
    folder: &mut T,
    def: usize,
    fields: Vec<(String, Type)>,
) -> Result<Type, T::Error> {
    let fields = fields
        .into_iter()
        .map(|(name, t)| folder.fold_type(t).map(|t| (name, t)))
        .collect::<Result<_, _>>()?;
    Ok(Type::Struct { def, fields })
}

pub fn fold_array<T: TypeFolder>(folder: &mut T, arr_t: Type) -> Result<Type, T::Error> {
    Ok(Type::Array(Box::new(folder.fold_type(arr_t)?)))
}

pub fn fold_application<T: TypeFolder>(
    folder: &mut T,
    typ: Type,
    args: Vec<Type>,
) -> Result<Type, T::Error> {
    let args = args
        .into_iter()
        .map(|t| folder.fold_type(t))
        .collect::<Result<_, _>>()?;
    Ok(Type::App {
        typ: Box::new(folder.fold_type(typ)?),
        args,
    })
}

fn fold_adt<T: TypeFolder>(
    folder: &mut T,
    def: usize,
    constructors: Vec<(String, Type)>,
) -> Result<Type, T::Error> {
    let constructors = constructors
        .into_iter()
        .map(|(name, t)| folder.fold_type(t).map(|t| (name, t)))
        .collect::<Result<_, _>>()?;
    Ok(Type::ADT { def, constructors })
}

/// Checks if types are structurally equal.
/// Does not perform any unification or instantiation.
pub fn types_eq(ctx: &TypCtx, t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::Alias(f1), Type::Alias(f2)) if f1 == f2 => {
            return true;
        }
        _ => {}
    }
    let t1 = if let Type::Alias(for_type) = t1 {
        match ctx.definitions.get(for_type) {
            Some(t) => t,
            None => return false,
        }
    } else {
        t1
    };
    let t2 = if let Type::Alias(for_type) = t2 {
        match ctx.definitions.get(for_type) {
            Some(t) => t,
            None => return false,
        }
    } else {
        t2
    };
    match (t1, t2) {
        (Type::Int, Type::Int) => true,
        (Type::UInt, Type::UInt) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::String, Type::String) => true,
        (Type::Float, Type::Float) => true,
        (Type::Unit, Type::Unit) => true,
        (Type::ToInfere, Type::ToInfere) => false,
        (Type::Divergent, _) => true,
        (_, Type::Divergent) => true,
        (Type::Struct { def: def1, .. }, Type::Struct { def: def2, .. }) => def1 == def2,
        (Type::Tuple(fields1), Type::Tuple(fields2)) => {
            if fields1.len() != fields2.len() {
                return false;
            }
            fields1
                .iter()
                .zip(fields2)
                .all(|(t1, t2)| types_eq(ctx, t1, t2))
        }
        (
            Type::Function {
                args: args1,
                ret: ret1,
            },
            Type::Function {
                args: args2,
                ret: ret2,
            },
        ) => {
            if args1.len() != args2.len() {
                return false;
            }
            types_eq(ctx, ret1, ret2)
                && args1
                    .iter()
                    .zip(args2)
                    .all(|(t1, t2)| types_eq(ctx, t1, t2))
        }
        (Type::Generic(d1, n1), Type::Generic(d2, n2)) => d1 == d2 && n1 == n2,
        (
            Type::Scheme {
                prefex: prefex_1,
                typ: typ_1,
            },
            Type::Scheme {
                prefex: prefex_2,
                typ: typ_2,
            },
        ) => {
            // This could be incorrect, to test for scheme equivalence we need to
            // replace generics in typ_2 using generics in typ_1 and then compare this
            prefex_1.len() == prefex_2.len() && types_eq(ctx, typ_1, typ_2)
        }
        (Type::App { typ: t1, args: a1 }, Type::App { typ: t2, args: a2 }) => {
            types_eq(ctx, t1, t2) && a1.iter().zip(a2).all(|(t1, t2)| types_eq(ctx, t1, t2))
        }
        _ => false,
    }
}

/// Same as types_eq, only here temporaraly to make refactoring easier.
pub fn types_strictly_eq(db: &Database, t1: &Type, t2: &Type) -> Result<bool, QueryError> {
    match (t1, t2) {
        (Type::Alias(f1), Type::Alias(f2)) if f1 == f2 => {
            return Ok(true);
        }
        _ => {}
    }
    let (t1, t2) = match (t1, t2) {
        (&Type::Alias(for_type), t2) => {
            let t1 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type))?;
            (t1, t2)
        }
        (t1, &Type::Alias(for_type)) => {
            let t2 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type))?;
            (t1, t2)
        }
        (&Type::Alias(for_type_1), &Type::Alias(for_type_2)) if for_type_1 == for_type_2 => {
            return Ok(true);
        }
        (&Type::Alias(for_type_1), &Type::Alias(for_type_2)) => {
            let t1 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type_1))?;
            let t2 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type_2))?;
            (t1, t2)
        }
        (t1, t2) => (t1, t2),
    };
    Ok(match (t1, t2) {
        (Type::Int, Type::Int) => true,
        (Type::UInt, Type::UInt) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::String, Type::String) => true,
        (Type::Float, Type::Float) => true,
        (Type::Unit, Type::Unit) => true,
        (Type::ToInfere, Type::ToInfere) => false,
        (Type::Divergent, _) => true,
        (_, Type::Divergent) => true,
        (Type::Struct { def: def1, .. }, Type::Struct { def: def2, .. }) => def1 == def2,
        (Type::Tuple(fields1), Type::Tuple(fields2)) => {
            if fields1.len() != fields2.len() {
                return Ok(false);
            }
            fields1
                .iter()
                .zip(fields2)
                .map(|(t1, t2)| types_strictly_eq(db, t1, t2))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .all(|eq| eq)
        }
        (
            Type::Function {
                args: args1,
                ret: ret1,
            },
            Type::Function {
                args: args2,
                ret: ret2,
            },
        ) => {
            if args1.len() != args2.len() {
                return Ok(false);
            }
            types_strictly_eq(db, ret1, ret2)?
                && args1
                    .iter()
                    .zip(args2)
                    .map(|(t1, t2)| types_strictly_eq(db, t1, t2))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .all(|eq| eq)
        }
        (Type::Generic(d1, n1), Type::Generic(d2, n2)) => d1 == d2 && n1 == n2,
        (
            Type::Scheme {
                prefex: prefex_1,
                typ: typ_1,
            },
            Type::Scheme {
                prefex: prefex_2,
                typ: typ_2,
            },
        ) => {
            // This could be incorrect, to test for scheme equivalence we need to
            // replace generics in typ_2 using generics in typ_1 and then compare this
            prefex_1.len() == prefex_2.len() && types_strictly_eq(db, typ_1, typ_2)?
        }
        (Type::App { typ: t1, args: a1 }, Type::App { typ: t2, args: a2 }) => {
            types_strictly_eq(db, t1, t2)?
                && a1
                    .iter()
                    .zip(a2)
                    .map(|(t1, t2)| types_strictly_eq(db, t1, t2))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .all(|eq| eq)
        }
        _ => false,
    })
}
