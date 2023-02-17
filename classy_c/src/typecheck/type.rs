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
}
