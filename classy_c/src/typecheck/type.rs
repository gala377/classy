use crate::typecheck::type_context::{Name, TypeId};

use super::type_context::DefId;

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

    /// Reference to the generic type under index `isize` in the definition
    /// with def id (defid, index)
    Generic(DefId, usize),
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
            | Type::Generic(_, _) => Some(true),
            Type::Fresh(_) | Type::Alias(_) | Type::ToInfere => None,
            Type::App { typ, .. } => typ.is_ref(),
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
}

pub trait TypeFolder: Sized {
    fn fold_type(&mut self, typ: Type) -> Type {
        fold_type(self, typ)
    }

    fn fold_int(&mut self) -> Type {
        Type::Int
    }

    fn fold_byte(&mut self) -> Type {
        Type::Byte
    }

    fn fold_uint(&mut self) -> Type {
        Type::UInt
    }

    fn fold_divergent(&mut self) -> Type {
        Type::Divergent
    }

    fn fold_alias(&mut self, for_type: usize) -> Type {
        Type::Alias(for_type)
    }

    fn fold_bool(&mut self) -> Type {
        Type::Bool
    }
    fn fold_float(&mut self) -> Type {
        Type::Float
    }
    fn fold_unit(&mut self) -> Type {
        Type::Unit
    }
    fn fold_string(&mut self) -> Type {
        Type::String
    }

    fn fold_tuple(&mut self, types: Vec<Type>) -> Type {
        fold_tuple(self, types)
    }

    fn fold_to_infere(&mut self) -> Type {
        Type::ToInfere
    }

    fn fold_fresh(&mut self, id: usize) -> Type {
        Type::Fresh(id)
    }

    fn fold_generic(&mut self, def_id: usize, id: usize) -> Type {
        Type::Generic(def_id, id)
    }

    fn fold_function(&mut self, args: Vec<Type>, ret: Type) -> Type {
        fold_function(self, args, ret)
    }

    fn fold_struct(&mut self, def: usize, fields: Vec<(String, Type)>) -> Type {
        fold_struct(self, def, fields)
    }

    fn fold_array(&mut self, arr_t: Type) -> Type {
        fold_array(self, arr_t)
    }

    fn fold_scheme(&mut self, prefex: Vec<Name>, typ: Type) -> Type {
        fold_scheme(self, prefex, typ)
    }

    fn fold_application(&mut self, typ: Type, args: Vec<Type>) -> Type {
        fold_application(self, typ, args)
    }
}

pub fn fold_scheme(folder: &mut impl TypeFolder, prefex: Vec<Name>, typ: Type) -> Type {
    Type::Scheme {
        prefex,
        typ: Box::new(folder.fold_type(typ)),
    }
}

pub fn fold_type(folder: &mut impl TypeFolder, typ: Type) -> Type {
    match typ {
        Type::Int => folder.fold_int(),
        Type::UInt => folder.fold_uint(),
        Type::Bool => folder.fold_bool(),
        Type::String => folder.fold_string(),
        Type::Float => folder.fold_float(),
        Type::Unit => folder.fold_unit(),
        Type::Byte => folder.fold_byte(),
        Type::Struct { def, fields } => folder.fold_struct(def, fields),
        Type::ADT {
            def: _,
            constructors: _,
        } => todo!(),
        Type::Function { args, ret } => folder.fold_function(args, *ret),
        Type::Tuple(types) => folder.fold_tuple(types),
        Type::Array(inner) => folder.fold_array(*inner),
        Type::Alias(for_type) => folder.fold_alias(for_type),
        Type::Divergent => folder.fold_divergent(),
        Type::ToInfere => folder.fold_to_infere(),
        Type::Scheme { prefex, typ } => folder.fold_scheme(prefex, *typ),
        Type::Generic(def_id, id) => folder.fold_generic(def_id, id),
        Type::Fresh(id) => folder.fold_fresh(id),
        Type::App { typ, args } => folder.fold_application(*typ, args),
    }
}

pub fn fold_tuple(folder: &mut impl TypeFolder, types: Vec<Type>) -> Type {
    let types = types.into_iter().map(|t| folder.fold_type(t));
    Type::Tuple(types.collect())
}

pub fn fold_function(folder: &mut impl TypeFolder, args: Vec<Type>, ret: Type) -> Type {
    let args = args.into_iter().map(|t| folder.fold_type(t)).collect();
    let ret = folder.fold_type(ret);
    Type::Function {
        args,
        ret: Box::new(ret),
    }
}

pub fn fold_struct(folder: &mut impl TypeFolder, def: usize, fields: Vec<(String, Type)>) -> Type {
    let fields = fields
        .into_iter()
        .map(|(name, t)| (name, folder.fold_type(t)))
        .collect();
    Type::Struct { def, fields }
}

pub fn fold_array(folder: &mut impl TypeFolder, arr_t: Type) -> Type {
    Type::Array(Box::new(folder.fold_type(arr_t)))
}

pub fn fold_application(folder: &mut impl TypeFolder, typ: Type, args: Vec<Type>) -> Type {
    let args = args.into_iter().map(|t| folder.fold_type(t)).collect();
    Type::App {
        typ: Box::new(folder.fold_type(typ)),
        args,
    }
}
