use std::collections::HashMap;

use bitvec::macros::internal::funty::Floating;
use thiserror::Error;

use classy_syntax::ast;

use crate::id_provider::UniqueId;
use crate::typecheck::r#type::{DeBruijn, Type, TypeFolder};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionId(UniqueId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(pub UniqueId);

/// A complete representation of a program that allows for easy quering.
pub struct Database {
    /// Global variable definitions.
    variable_definitions: HashMap<DefinitionId, ast::ConstDefinition>,
    /// Function and method definitions.
    function_definitions: HashMap<DefinitionId, ast::FunctionDefinition>,
    /// Type definitions.
    type_definitions: HashMap<DefinitionId, ast::TypeDefinition>,

    /// A mapping assigning each definition to its respective type.
    definition_types: HashMap<DefinitionId, TypeId>,

    /// All the type values generated during typechecking.
    /// Maps their respective handle to the value of the type.
    type_aliases: HashMap<TypeId, Type>,

    /// Contains mapping between the base type -> specialisation -> methods
    method_blocks: MethodBlocksMappings,
}

impl Database {
    pub fn resolve_alias(&self, type_id: TypeId) -> Result<Type, QueryError> {
        self.type_aliases
            .get(&type_id)
            .cloned()
            .ok_or(QueryError::TypeNotFound(type_id))
    }
}

#[derive(Error, Debug)]
pub enum QueryError {
    #[error("Definition {0:?} not found")]
    DefinitionNotFound(DefinitionId),
    #[error("Type {0:?} not found")]
    TypeNotFound(TypeId),
}

pub struct ResolvedMethod {
    pub base_type: TypeId,
    pub receiver_type: TypeId,
    pub method_defniition: DefinitionId,
}

struct MethodHandle {
    name: String,
    definition: DefinitionId,
}
struct MethodBlocksMappings {
    methods_blocks: HashMap<TypeId, HashMap<TypeId, MethodHandle>>,
}

impl MethodBlocksMappings {
    /// Try to find a method with the given name that is applicable to the given
    /// receiver type. In case more than one spacialisation applies return
    /// all resolved methods. The caller is responsible for choosing the
    /// right implementation.
    pub fn find_method(&self, receiver_type: Type, name: &str) -> Vec<ResolvedMethod> {
        // get base type of the receiver
        // get base type
        // unify specialisation with the receiver type
        // find a name, if not, look for the next specialisation.
        todo!()
    }
}
