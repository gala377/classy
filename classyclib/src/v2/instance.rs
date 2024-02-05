use std::collections::HashMap;

use crate::{
    id_provider::UniqueId,
    util::discard_values::DiscardValues,
    v2::knowledge::{Database, QueryError, TypeId},
};
use thiserror::Error;

use crate::typecheck::types::{DeBruijn, Type, TypeFolder};

#[derive(Error, Debug)]
pub enum InstantiationError {
    #[error("Not all prefex arguments have been saturated prefex={prefex} != args={args}")]
    PrefexAndArgCountMismatch { prefex: usize, args: usize },
    #[error("Query error during instantiation: {0}")]
    QueryError(#[from] QueryError),
    #[error("Instantiation argument is not a reference type: {0:?}")]
    NonRefType(Type),
}

/// Instance scheme `scheme_t` with `args`. It is important to note
/// that `scheme_t` must be a scheme type or an alias to a scheme.
pub fn instance(
    db: &Database,
    scheme_t: Type,
    args: Vec<Type>,
) -> Result<Type, InstantiationError> {
    args.iter()
        .map(|t| {
            t.is_ref()
                .ok_or(InstantiationError::NonRefType(t.clone()))
                .map(|is_ref| {
                    if is_ref {
                        Ok(())
                    } else {
                        Err(InstantiationError::NonRefType(t.clone()))
                    }
                })
        })
        .collect::<Result<DiscardValues, _>>()?;
    match &scheme_t {
        // Special case when we just need to unpack a scheme
        Type::Scheme { prefex, typ } if args.is_empty() && prefex.is_empty() => {
            return Ok(ShiftDebruijn.fold_type(*typ.clone()).unwrap());
        }
        Type::Scheme { prefex, .. } => {
            assert_eq!(
                prefex.len(),
                args.len(),
                "cannot saturate a type application"
            );
        }
        Type::Alias(id) => {
            let resolved = db.resolve_tid(crate::v2::knowledge::TypeId(*id))?;
            return instance(db, resolved, args);
        }
        t => {
            assert!(args.is_empty(), "type is empty, cannot instantiate: {t:?}");
        }
    }
    let new_t = args
        .into_iter()
        .enumerate()
        .fold(Ok(scheme_t), |acc, (i, t)| {
            let mut replacer = Instatiator {
                for_gen: i,
                instatiated: t,
                db,
                deruijn: DeBruijn(-1),
            };
            replacer.fold_type(acc?)
        })?;
    Ok(ShiftDebruijn.fold_type(new_t).unwrap())
}

struct ShiftDebruijn;
impl TypeFolder for ShiftDebruijn {
    type Error = ();
    fn fold_generic(&mut self, index: DeBruijn, pos: usize) -> Result<Type, ()> {
        Ok(Type::Generic(index - 1, pos))
    }
}

struct Instatiator<'db> {
    for_gen: usize,
    instatiated: Type,
    deruijn: DeBruijn,
    db: &'db Database,
}

impl<'ctx> TypeFolder for Instatiator<'ctx> {
    type Error = InstantiationError;

    fn fold_scheme(
        &mut self,
        prefex: Vec<crate::typecheck::type_context::Name>,
        typ: Type,
    ) -> Result<Type, Self::Error> {
        println!("Instatiating scheme {prefex:?} {typ:?}");
        self.deruijn += 1;
        let typ = self.fold_type(typ)?;
        self.deruijn -= 1;
        if self.deruijn == DeBruijn(-1) {
            println!("Instatiated scheme to {typ:?}");
            return Ok(typ);
        }
        println!("Not a top level so returning scheme of {typ:?}");
        Ok(Type::Scheme {
            prefex,
            typ: Box::new(typ),
        })
    }

    fn fold_generic(&mut self, def: DeBruijn, id: usize) -> Result<Type, Self::Error> {
        println!(
            "Instatiating generic {def:?} {id} current ({:?}, {})",
            self.deruijn, self.for_gen
        );
        if def != self.deruijn {
            return Ok(Type::Generic(def, id));
        }
        if self.for_gen == id {
            match &self.instatiated {
                Type::Generic(d, i) => Ok(Type::Generic(self.deruijn.clone() + d.0, *i)),
                t => Ok(t.clone()),
            }
        } else {
            Ok(Type::Generic(def, id))
        }
    }

    fn fold_alias(&mut self, for_type: usize) -> Result<Type, Self::Error> {
        println!("Resolving alias {for_type}");
        let resolved = self
            .db
            .resolve_tid(crate::v2::knowledge::TypeId(for_type))?;
        println!("Resolved alias {resolved:?}");
        self.fold_type(resolved)
    }
}

/// Substitutions to be made for fresh variables in a type for it to match
/// the given union result.
pub type Substitutions = HashMap<UniqueId, Type>;

#[derive(Error, Debug)]
pub enum UnificationError {
    #[error("One of the types tried to be unified has not been replaced by a type metavariable")]
    UnexpectedToInfere,
    #[error("Could not unify types {0:?} and {1:?}")]
    CouldNotUnify(Type, Type),

    #[error("Query error during unification: {0}")]
    QueryError(#[from] QueryError),

    #[error("Instantiation error during unification: {0}")]
    InstantiationError(#[from] InstantiationError),
}

pub fn union(
    db: &Database,
    a: Type,
    b: Type,
    subs: &mut Substitutions,
) -> Result<Type, UnificationError> {
    match (a, b) {
        (Type::Alias(for_t), other) => union(db, db.resolve_tid(TypeId(for_t))?, other, subs),
        (other, Type::Alias(for_t)) => union(db, other, db.resolve_tid(TypeId(for_t))?, subs),
        (Type::Int, Type::Int) => Ok(Type::Int),
        (Type::UInt, Type::UInt) => Ok(Type::UInt),
        (Type::Float, Type::Float) => Ok(Type::Float),
        (Type::Byte, Type::Byte) => Ok(Type::Byte),
        (Type::String, Type::String) => Ok(Type::String),
        (Type::Bool, Type::Bool) => Ok(Type::Bool),
        (Type::Unit, Type::Unit) => Ok(Type::Unit),
        (Type::Tuple(a), Type::Tuple(b)) => Ok(Type::Tuple(union_slices(db, &a, &b, subs)?)),
        (Type::Fresh(id_1), other) => {
            subs.insert(id_1, other.clone());
            Ok(other)
        }
        (other, Type::Fresh(id_2)) => {
            subs.insert(id_2, other.clone());
            Ok(other)
        }
        (
            Type::Function {
                args: args_1,
                ret: ret_1,
            },
            Type::Function {
                args: args_2,
                ret: ret_2,
            },
        ) => {
            let args = union_slices(db, &args_1, &args_2, subs)?;
            let ret = union(db, *ret_1, *ret_2, subs)?;
            Ok(Type::Function {
                args,
                ret: Box::new(ret),
            })
        }
        (Type::Generic(scope_1, index_1), Type::Generic(scope_2, index_2))
            if scope_2 == scope_1 && index_1 == index_2 =>
        {
            Ok(Type::Generic(scope_1, index_1))
        }
        (Type::Struct { def: def_1, fields }, Type::Struct { def: def_2, .. })
            if def_1 == def_2 =>
        {
            Ok(Type::Struct { def: def_1, fields })
        }
        (
            Type::ADT {
                def: def_1,
                constructors,
            },
            Type::ADT { def: def_2, .. },
        ) if def_1 == def_2 => Ok(Type::ADT {
            def: def_1,
            constructors,
        }),
        (Type::ToInfere, _) => Err(UnificationError::UnexpectedToInfere),
        (_, Type::ToInfere) => Err(UnificationError::UnexpectedToInfere),
        (Type::App { typ, args }, other) => {
            let instanced = instance(db, *typ, args)?;
            union(db, instanced, other, subs)
        }
        (other, Type::App { typ, args }) => {
            let instanced = instance(db, *typ, args)?;
            union(db, instanced, other, subs)
        }
        (
            Type::Scheme {
                prefex: prf_1,
                typ: t_1,
            },
            Type::Scheme {
                prefex: prf_2,
                typ: t_2,
            },
        ) if prf_1.len() == prf_2.len() => union(db, *t_1, *t_2, subs),
        (Type::Array(inner_1), Type::Array(inner_2)) => {
            let inner = union(db, *inner_1, *inner_2, subs)?;
            Ok(Type::Array(Box::new(inner)))
        }
        (Type::Divergent, _) => Ok(Type::Divergent),
        (_, Type::Divergent) => Ok(Type::Divergent),
        (a, b) => Err(UnificationError::CouldNotUnify(a, b)),
    }
}

fn union_slices(
    db: &Database,
    a: &[Type],
    b: &[Type],
    subs: &mut Substitutions,
) -> Result<Vec<Type>, UnificationError> {
    if a.len() != b.len() {
        return Err(UnificationError::CouldNotUnify(
            Type::Tuple(a.to_vec()),
            Type::Tuple(b.to_vec()),
        ));
    }
    a.into_iter()
        .zip(b.into_iter())
        .map(|(a, b)| union(db, a.clone(), b.clone(), subs))
        .collect::<Result<Vec<_>, _>>()
}
