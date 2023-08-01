use super::types::Type;

#[derive(Debug, Clone)]
pub enum Constraint {
    /// Defines that type t1 should be equal to t2
    Eq(Type, Type),
    /// Defines that a t.field should exist and be of type 'of_type'
    HasField {
        t: Type,
        field: String,
        of_type: Type,
    },
    /// t has to be an ADT with a case named `case` and of type `of_type`.
    /// A tuple struct is expected as a tuple type.
    /// A struct is expected as a struct type with dummy definition ID.
    HasCase {
        t: Type,
        case: String,
        of_type: Type,
    },
    HasMethod {
        receiver: Type,
        method: String,
        args: Vec<Type>,
        ret: Type,
    },
}
