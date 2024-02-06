use crate::typecheck::{type_context::Name, types::DeBruijn};

use super::knowledge::{Id, TypeId};

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
        def: Id<TypeId>,
        // maps fields to the TypeId of the type
        fields: Vec<(Name, Type)>,
    },
    ADT {
        def: Id<TypeId>,
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
    Alias(Id<TypeId>),
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

    fn fold_alias(&mut self, for_type: Id<TypeId>) -> Result<Type, Self::Error> {
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
        def: Id<TypeId>,
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
        def: Id<TypeId>,
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
    def: Id<TypeId>,
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
    def: Id<TypeId>,
    constructors: Vec<(String, Type)>,
) -> Result<Type, T::Error> {
    let constructors = constructors
        .into_iter()
        .map(|(name, t)| folder.fold_type(t).map(|t| (name, t)))
        .collect::<Result<_, _>>()?;
    Ok(Type::ADT { def, constructors })
}
