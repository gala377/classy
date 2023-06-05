use crate::typecheck::type_context::{Name, TypeId};

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Type {
    Int,
    UInt,
    Bool,
    String,
    Float,
    Unit,
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
    /// Reference to the generic type under index `isize` in the closests enclosing schema.
    Generic(usize),
    /// Temporary type used onlu for type inference
    Fresh(usize),
}

pub trait TypeFolder: Sized {
    fn fold_type(&mut self, typ: Type) -> Type {
        fold_type(self, typ)
    }

    fn fold_int(&mut self) -> Type {
        Type::Int
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

    fn fold_generic(&mut self, id: usize) -> Type {
        Type::Generic(id)
    }

    fn fold_function(&mut self, args: Vec<Type>, ret: Type) -> Type {
        fold_function(self, args, ret)
    }

    fn fold_struct(&mut self, def: usize, fields: Vec<(String, Type)>) -> Type {
        fold_struct(self, def, fields)
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
        Type::Struct { def, fields } => folder.fold_struct(def, fields),
        Type::ADT { def: _, constructors: _ } => todo!(),
        Type::Function { args, ret } => folder.fold_function(args, *ret),
        Type::Tuple(types) => folder.fold_tuple(types),
        Type::Array(_) => todo!(),
        Type::Alias(for_type) => folder.fold_alias(for_type),
        Type::Divergent => folder.fold_divergent(),
        Type::ToInfere => folder.fold_to_infere(),
        Type::Scheme { prefex: _, typ: _ } => todo!(),
        Type::Generic(id) => folder.fold_generic(id),
        Type::Fresh(id) => folder.fold_fresh(id),
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
