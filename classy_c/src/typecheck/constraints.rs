use super::r#type::Type;

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
}
