use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use thiserror::Error;

use classy_syntax::ast;

use crate::id_provider::UniqueId;
use crate::typecheck::instance::{union, UnificationError};
use crate::typecheck::types::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionId(pub UniqueId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(pub UniqueId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageId(pub UniqueId);

pub const CURRENT_PACKAGE_ID: PackageId = PackageId(0);

pub struct PackageInfo {
    pub name: String,
    pub globals: HashMap<String, DefinitionId>,
    pub definition_types: HashMap<DefinitionId, TypeId>,
    pub type_aliases: HashMap<TypeId, Type>,
}

impl PackageInfo {
    pub fn empty(name: &str) -> Self {
        Self {
            name: name.to_string(),
            globals: HashMap::new(),
            definition_types: HashMap::new(),
            type_aliases: HashMap::new(),
        }
    }
}

impl PackageInfo {
    pub fn get_definition(&self, name: &str) -> Option<DefinitionId> {
        self.globals.get(name).cloned()
    }
}

pub enum ImplVisibility {
    /// Imported alongside the type. As well as visible withing the same
    /// namespace as the type.
    ///
    /// This visibility is assigned to exported anonymous implementations
    /// defined within the same namespace as the type they refer to.
    /// In particular during unification it only applies to cases where the most
    /// outer type does not require any substitutions.
    /// Example:
    ///     export methods Option(a) // this is fine as Option does not require
    ///                              // firther substitutions, it is a matcher
    ///                             // already.     
    ///     export methods a { id = this } // this is not fine as `a` is a
    ///                                    // generic type and as such requires
    ///                                    // immediate unification.    
    ///     export methods Int // this is also fine but not possible as every
    ///                        // package is in a different namespace than Int.
    ///
    /// The same applies to instances.      
    ImplicitExport,
    /// By default visible within given namespace but can be imported by name.
    /// This is for unexported anonymous implementations as well as named
    /// implementations.
    InNamespace,
}

pub struct Visibility {
    pub package: PackageId,
    pub namespace: ast::Namespace,
    pub visibility: ImplVisibility,
}

/// A complete representation of a program that allows for easy quering.
pub struct Database {
    /// Mapping for package id to its package info.
    pub packages: Vec<PackageInfo>,
    /// Map of package name to its id.
    pub packages_map: HashMap<String, PackageId>,

    /// Global names, as global names in every package need to be unique
    /// we can map them to their definition id.§
    pub globals: HashMap<String, DefinitionId>,

    /// Global variable definitions.
    pub variable_definitions: HashMap<DefinitionId, ast::ConstDefinition>,
    /// Function and method definitions.
    pub function_definitions: HashMap<DefinitionId, ast::FunctionDefinition>,
    /// Type definitions.
    pub type_definitions: HashMap<DefinitionId, ast::TypeDefinition>,

    /// A mapping assigning each definition to its respective type.
    pub definition_types: HashMap<DefinitionId, TypeId>,

    /// All the type values generated during typechecking.
    /// Maps their respective handle to the value of the type.
    pub type_aliases: HashMap<TypeId, Type>,

    /// Maps a type to its type id.
    ///
    /// Note that different type ids can refer
    /// to types that are structurally equal so this mapping is not a perfect
    /// reverse. In this case this map will recognize such types as equal.
    /// However, as the types are checked using structural equality throughout
    /// typechecking, this collision should not effect it.
    pub reverse_type_aliases: TypeHashMap<TypeId>,

    /// Contains mapping between the base type -> specialisation -> methods
    pub method_blocks: MethodBlocksMappings,
}

#[derive(Error, Debug)]
pub enum QueryError {
    #[error("Definition {0:?} not found")]
    DefinitionNotFound(DefinitionId),
    #[error("Type {0:?} not found")]
    TypeNotFound(TypeId),

    #[error("Methods unnsopported for type {0:?}")]
    MethodsUnsupported(Type),

    #[error("Type {0:?} is not hashable")]
    InvalidHash(Type),

    #[error("No corresponding type id for type {0:?}")]
    NoTypeIdForType(Type),

    #[error("Unification error: {0}")]
    UnificationError(#[from] Box<UnificationError>),
}

pub struct ResolvedMethod {
    /// Unified type for the receiver.
    pub unified_type: Type,
    /// Definition id pointing to the node with the definition of the method.
    pub method_defnition: DefinitionId,
    /// Substitutions that need to be applied to the receiver on its
    /// meta type variables to be equal to the unified type.
    /// Those substitutions need to hold for this method to apply.
    pub substitutions: HashMap<UniqueId, Type>,
}

pub struct MethodHandle {
    name: String,
    definition: DefinitionId,
}
pub type MethodBlocksMappings = HashMap<TypeId, HashMap<TypeId, MethodHandle>>;

impl Database {
    const INITIAL_TYPE_MAP_CAPACITY: u64 = 1000;

    pub fn new() -> Self {
        Self {
            packages: Vec::new(),
            packages_map: HashMap::new(),
            variable_definitions: HashMap::new(),
            function_definitions: HashMap::new(),
            type_definitions: HashMap::new(),
            globals: HashMap::new(),
            definition_types: HashMap::new(),
            type_aliases: HashMap::new(),
            reverse_type_aliases: TypeHashMap::new(Self::INITIAL_TYPE_MAP_CAPACITY as u64),
            method_blocks: HashMap::new(),
        }
    }

    // Package queries
    pub fn add_package(&mut self, package: PackageInfo) {
        self.packages_map
            .insert(package.name.clone(), PackageId(self.packages.len() + 1));
        self.packages.push(package);
    }

    pub fn get_package(&self, id: PackageId) -> &PackageInfo {
        // this is important as the current package id is 0
        // so we need to subtract 1 to get the correct index
        // into the dependencies vector
        &self.packages[(id.0 - 1) as usize]
    }

    pub fn package_id(&self, of: &str) -> Option<PackageId> {
        self.packages_map.get(of).cloned()
    }

    pub fn package_info(&self, of: PackageId) -> Option<&PackageInfo> {
        self.packages.get(of.0 as usize)
    }
    // Definitions

    pub fn add_type_definition(&mut self, id: DefinitionId, definition: ast::TypeDefinition) {
        assert!(!self.type_definitions.contains_key(&id));
        assert!(self
            .globals
            .insert(definition.name.clone(), id.clone())
            .is_none());
        self.type_definitions.insert(id, definition);
    }

    pub fn add_function_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::FunctionDefinition,
    ) {
        assert!(!self.function_definitions.contains_key(&id));
        assert!(self
            .globals
            .insert(definition.name.clone(), id.clone())
            .is_none());
        self.function_definitions.insert(id, definition);
    }

    pub fn add_const_definition(&mut self, id: DefinitionId, definition: ast::ConstDefinition) {
        assert!(!self.variable_definitions.contains_key(&id));
        assert!(self
            .globals
            .insert(definition.name.clone(), id.clone())
            .is_none());
        self.variable_definitions.insert(id, definition);
    }

    pub fn get_global(&self, name: &str) -> Option<DefinitionId> {
        self.globals.get(name).cloned()
    }

    pub fn get_type(&self, package: PackageId, id: DefinitionId) -> Option<&Type> {
        if package == CURRENT_PACKAGE_ID {
            let tid = self.definition_types.get(&id)?;
            self.type_aliases.get(tid)
        } else {
            let package_info = self.get_package(package);
            let tid = package_info.definition_types.get(&id)?;
            package_info.type_aliases.get(tid)
        }
    }

    pub fn get_type_by_unresolved_name(
        &self,
        current_namespace: &[String],
        path: &[String],
        name: &str,
    ) -> Option<&Type> {
        if let Some(potential_package) = path.first() {
            if let Some(package_id) = self.package_id(potential_package) {
                let package_info = self.get_package(package_id.clone());
                let mut expanded_name = path[1..].to_vec();
                expanded_name.push(name.to_string());
                let expanded_name = expanded_name.join("::");
                if let Some(definition_id) = package_info.globals.get(&expanded_name) {
                    return self.get_type(package_id, definition_id.clone());
                }
                return None;
            }
        }
        let mut expanded_name = current_namespace.to_vec();
        expanded_name.extend(path.iter().cloned());
        expanded_name.push(name.to_string());
        let expand_name = expanded_name.join("::");
        let Some(definition_id) = self.globals.get(&expand_name) else {
            return None;
        };
        self.get_type(CURRENT_PACKAGE_ID, definition_id.clone())
    }

    // Type aliasing

    pub fn resolve_alias(&self, type_id: TypeId) -> Result<Type, QueryError> {
        self.resolve_alias_ref(type_id).cloned()
    }

    pub fn resolve_alias_ref(&self, type_id: TypeId) -> Result<&Type, QueryError> {
        self.type_aliases
            .get(&type_id)
            .ok_or(QueryError::TypeNotFound(type_id))
    }

    pub fn type_id_from_definition(
        &self,
        definition_id: DefinitionId,
    ) -> Result<TypeId, QueryError> {
        self.definition_types
            .get(&definition_id)
            .cloned()
            .ok_or(QueryError::DefinitionNotFound(definition_id))
    }

    pub fn resolve_method(
        &self,
        receiver: Type,
        name: &str,
    ) -> Result<Vec<ResolvedMethod>, QueryError> {
        // get base type
        // union with every specialisation
        // find methods that match the name
        let base_type = self.base_type(receiver.clone())?;
        let base_type_id = self
            .reverse_type_aliases
            .get(self, &base_type)?
            .ok_or(QueryError::NoTypeIdForType(base_type.clone()))?;
        let specialisations = self
            .method_blocks
            .get(base_type_id)
            .ok_or_else(|| QueryError::MethodsUnsupported(receiver.clone()))?;
        let mut methods: Vec<(Type, DefinitionId)> = Vec::new();
        for (type_id, method) in specialisations {
            if method.name == name {
                let resolved = self.resolve_alias(type_id.clone())?;
                methods.push((resolved, method.definition.clone()));
            }
        }
        // find matches for specialisations
        let mut resolved_methods = Vec::new();
        for (typ, meth_def) in methods {
            let mut subs = HashMap::new();
            let unified = union(self, receiver.clone(), typ, &mut subs).map_err(Box::new)?;
            resolved_methods.push(ResolvedMethod {
                unified_type: unified,
                method_defnition: meth_def,
                substitutions: subs,
            });
        }
        Ok(resolved_methods)
    }

    /// Returns the un-schemed, un-applied type.
    pub fn base_type(&self, t: Type) -> Result<Type, QueryError> {
        match t {
            Type::Alias(id) => self.base_type(self.resolve_alias(crate::knowledge::TypeId(id))?),
            Type::Scheme { typ, .. } => self.base_type(*typ),
            Type::App { typ, .. } => self.base_type(*typ),
            t => Ok(t),
        }
    }

    /// Check if 2 types are structuraly equal. This means no instantiation or
    /// union is happening. Aliases get resolved before comparison.
    pub fn types_strictly_eq(&self, t1: &Type, t2: &Type) -> Result<bool, QueryError> {
        match (t1, t2) {
            (Type::Alias(f1), Type::Alias(f2)) if f1 == f2 => {
                return Ok(true);
            }
            _ => {}
        }
        let (t1, t2) = match (t1, t2) {
            (&Type::Alias(for_type_1), &Type::Alias(for_type_2)) if for_type_1 == for_type_2 => {
                return Ok(true);
            }
            (&Type::Alias(for_type_1), &Type::Alias(for_type_2)) => {
                let t1 = self.resolve_alias_ref(crate::knowledge::TypeId(for_type_1))?;
                let t2 = self.resolve_alias_ref(crate::knowledge::TypeId(for_type_2))?;
                (t1, t2)
            }
            (&Type::Alias(for_type), t2) => {
                let t1 = self.resolve_alias_ref(crate::knowledge::TypeId(for_type))?;
                (t1, t2)
            }
            (t1, &Type::Alias(for_type)) => {
                let t2 = self.resolve_alias_ref(crate::knowledge::TypeId(for_type))?;
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
                    .map(|(t1, t2)| self.types_strictly_eq(t1, t2))
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
                self.types_strictly_eq(ret1, ret2)?
                    && args1
                        .iter()
                        .zip(args2)
                        .map(|(t1, t2)| self.types_strictly_eq(t1, t2))
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
                prefex_1.len() == prefex_2.len() && self.types_strictly_eq(typ_1, typ_2)?
            }
            (Type::App { typ: t1, args: a1 }, Type::App { typ: t2, args: a2 }) => {
                self.types_strictly_eq(t1, t2)?
                    && a1
                        .iter()
                        .zip(a2)
                        .map(|(t1, t2)| self.types_strictly_eq(t1, t2))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .all(|eq| eq)
            }
            _ => false,
        })
    }

    pub fn hash_type(&self, typ: &Type) -> Result<u64, QueryError> {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.hash_type_with_hasher(typ, &mut hasher)?;
        Ok(hasher.finish())
    }

    pub fn hash_type_with_hasher<H: Hasher>(
        &self,
        typ: &Type,
        state: &mut H,
    ) -> Result<(), QueryError> {
        std::mem::discriminant(typ).hash(state);
        match typ {
            Type::Struct { def, .. } => state.write_usize(*def),
            Type::ADT { def, .. } => state.write_usize(*def),
            Type::Function { args, ret } => {
                // write length so that tuples and functions with the same number of arguments
                // hash differently
                state.write_usize(args.len());
                for arg in args {
                    self.hash_type_with_hasher(arg, state)?;
                }
                self.hash_type_with_hasher(ret, state)?;
            }
            Type::Tuple(args) => {
                // write length so that tuples and functions with the same number of arguments
                // hash differently
                state.write_usize(args.len());
                for arg in args {
                    self.hash_type_with_hasher(arg, state)?;
                }
            }
            Type::Array(inner) => {
                self.hash_type_with_hasher(inner, state)?;
            }
            Type::Alias(for_type) => {
                let resolved = self.resolve_alias(TypeId(*for_type))?.assert_resolved();
                self.hash_type_with_hasher(&resolved, state)?;
            }
            Type::ToInfere => return Err(QueryError::InvalidHash(Type::ToInfere)),
            Type::Scheme { prefex, typ } => {
                state.write_usize(prefex.len());
                self.hash_type_with_hasher(typ, state)?;
            }
            Type::App { typ, args } => {
                self.hash_type_with_hasher(typ, state)?;
                state.write_usize(args.len());
                for arg in args {
                    self.hash_type_with_hasher(arg, state)?;
                }
            }
            Type::Generic(debruijn, index) => {
                state.write_isize(debruijn.0);
                state.write_usize(*index);
            }
            t @ Type::Fresh(_) => return Err(QueryError::InvalidHash(t.clone())),
            // types without arguments already wrote their discriminants
            _ => {}
        };
        Ok(())
    }
}

pub struct TypeHashMap<V> {
    inner: Vec<Vec<(Type, V)>>,
    capacity: u64,
}

impl<V: Clone> TypeHashMap<V> {
    pub fn new(capacity: u64) -> Self {
        Self {
            inner: vec![Vec::new(); capacity as usize],
            capacity,
        }
    }
    pub fn insert(&mut self, db: &Database, key: Type, value: V) -> Result<Option<V>, QueryError> {
        let hash = db.hash_type(&key)?;
        let index = hash % self.capacity;
        let bucket = &mut self.inner[index as usize];
        for (t, v) in bucket.iter_mut() {
            if db.types_strictly_eq(&key, t)? {
                return Ok(Some(std::mem::replace(v, value)));
            }
        }
        bucket.push((key, value));
        Ok(None)
    }

    pub fn get(&self, db: &Database, key: &Type) -> Result<Option<&V>, QueryError> {
        let hash = db.hash_type(key)?;
        let index = hash % self.capacity;
        let bucket = &self.inner[index as usize];
        for (t, v) in bucket.iter() {
            if db.types_strictly_eq(key, t)? {
                return Ok(Some(v));
            }
        }
        Ok(None)
    }
}
