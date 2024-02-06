use core::panic;
use std::cell::RefCell;
/// This is a part of typechecker V2.
/// This will replace the TypCtx.
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::rc::Rc;

use thiserror::Error;

use classy_syntax::ast;

use crate::id_provider::UniqueId;
use crate::session::Session;
use crate::typecheck::ast_to_type::PrefexScope;
use crate::typecheck::types::DeBruijn;
use crate::v2::instance::UnificationError;
use crate::v2::ty::Type;

pub struct GenericConstraint {
    /// Class used for the constraint
    pub class: Id<DefinitionId>,
    /// Arguments used for the constraint
    pub args: Vec<Type>,
}

/// Id of some item, can either be a local reference meaning current package
/// or a global one meaning it references a definition from another package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id<T> {
    /// Might reference a definition from another package or the current one
    Global { package: PackageId, id: T },
    /// References a definition within the current package
    Local(LocalId<T>),
}

/// Id referencing current package
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct LocalId<T>(pub T);

impl<T> LocalId<T> {
    pub fn new(id: T) -> Self {
        Self(id)
    }

    pub fn into_id(self) -> Id<T> {
        Id::Local(self)
    }

    pub fn as_global(self, package: PackageId) -> Id<T> {
        Id::Global {
            package,
            id: self.0,
        }
    }
}

impl<T> Into<Id<T>> for LocalId<T> {
    fn into(self) -> Id<T> {
        Id::Local(self)
    }
}

/// Id that identifies a definition within a package
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct DefinitionId(pub UniqueId);

/// Id that identifies a type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeId(pub UniqueId);

/// Id that identifies a package
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct PackageId(pub UniqueId);

pub const CURRENT_PACKAGE_ID: PackageId = PackageId(0);

/// Information about a method blocks defined within a class
pub struct ClassMethodBlock {
    /// Receiver type for the methods block
    pub receiver: Id<TypeId>,
    /// Collection of all the methods defined within the block with their types
    pub methods: Vec<(String, LocalId<TypeId>)>,
}

/// Information about a class definition
pub struct ClassInfo {
    pub name: String,
    // Collection of all the static methods defined within the class
    // and their types
    pub static_methods: Vec<(String, LocalId<TypeId>)>,
    /// Method blocks defined within the class
    pub method_blocks: Vec<ClassMethodBlock>,
}

/// Information about a method blocks within an instance
pub struct InstanceMethodBlock {
    /// Receiver type for the methods block
    pub receiver: Id<TypeId>,
    /// Collection of all the methods defined within the block with their
    /// definitions
    pub methods: Vec<(String, LocalId<DefinitionId>)>,
}

pub struct InstanceInfo {
    /// Optional name of the instance
    pub name: Option<String>,
    /// Type of the reeiver the instance is defined for
    pub receiver: Id<TypeId>,
    /// Static methods implementations defined within the instance
    pub static_methods: Vec<(String, LocalId<DefinitionId>)>,
    pub method_blocks: Vec<InstanceMethodBlock>,
    /// Should be autoimported if the given definition is autoimported
    /// if none it should not be autoimported
    pub autoimported: Option<LocalId<DefinitionId>>,
}

pub struct MethodBlockInfo {
    pub name: Option<String>,
    /// Receiver type for the methods block
    pub receiver: Id<TypeId>,
    /// Methods definitions
    pub methods: Vec<(String, LocalId<DefinitionId>)>,
}

pub struct MethodInfo {
    pub name: String,
    pub ty: LocalId<TypeId>,
}

pub struct PackageInfo {
    /// Name of the package
    pub name: String,
    /// All the globaly visible definitions within the package
    pub globals: HashMap<String, LocalId<DefinitionId>>,
    /// Used to lookup type of a given definition
    pub definition_types: HashMap<LocalId<DefinitionId>, LocalId<TypeId>>,
    /// Used to lookup type id into a type
    pub typeid_to_type: HashMap<LocalId<TypeId>, Type>,
    /// When importing item with definition id Key, all the following
    /// definitions should be imported as well. This comes into play when we
    /// import a type, we also want to import implicit instances and method
    /// blocks.
    pub implicit_imports: HashMap<LocalId<DefinitionId>, Vec<LocalId<DefinitionId>>>,
    /// Method blocks defined within the package
    pub method_blocks: HashMap<LocalId<DefinitionId>, MethodBlockInfo>,
    /// Classes defined within the package
    pub classes: HashMap<LocalId<DefinitionId>, ClassInfo>,
    /// Instances defined within the package
    pub instances: HashMap<LocalId<DefinitionId>, InstanceInfo>,
    /// All the methods from instances and method blocks defined within the
    /// package. The context of their definition should be known for the type to
    /// be properly substituted.
    pub methods: HashMap<LocalId<DefinitionId>, MethodInfo>,
    /// Type constraints for any definition within the package
    pub constraints: HashMap<LocalId<DefinitionId>, Vec<GenericConstraint>>,
}

impl PackageInfo {
    pub fn empty(name: &str) -> Self {
        Self {
            name: name.to_string(),
            globals: HashMap::new(),
            definition_types: HashMap::new(),
            typeid_to_type: HashMap::new(),
            implicit_imports: HashMap::new(),
            method_blocks: HashMap::new(),
            classes: HashMap::new(),
            instances: HashMap::new(),
            methods: HashMap::new(),
            constraints: HashMap::new(),
        }
    }
}

/// A complete representation of a program that allows for easy quering.
pub struct Database {
    /// Mapping for package id to its package info.
    pub packages: Vec<PackageInfo>,
    /// Map of package name to its id.
    pub packages_map: HashMap<String, PackageId>,

    /// Global names, as global names in every package need to be unique
    /// we can map them to their definition id.§
    pub globals: HashMap<String, LocalId<DefinitionId>>,

    /// Global variable definitions.
    pub variable_definitions: HashMap<LocalId<DefinitionId>, ast::ConstDefinition>,
    /// Function and method definitions.
    pub function_definitions: HashMap<LocalId<DefinitionId>, ast::FunctionDefinition>,
    /// Type definitions.
    pub type_definitions: HashMap<LocalId<DefinitionId>, ast::TypeDefinition>,
    /// Method block definitions
    pub method_blocks_definitions:
        HashMap<LocalId<DefinitionId>, ast::MethodsBlock<ast::FunctionDefinition>>,
    /// Class definitions
    pub class_definitions: HashMap<LocalId<DefinitionId>, ast::ClassDefinition>,
    /// Instance definitions
    pub instance_definitions: HashMap<LocalId<DefinitionId>, ast::InstanceDefinition>,

    /// A mapping assigning each definition to its respective type.
    pub definition_types: HashMap<LocalId<DefinitionId>, LocalId<TypeId>>,

    /// All the type values generated during typechecking.
    /// Maps their respective handle to the value of the type.
    pub typeid_to_type: HashMap<LocalId<TypeId>, Type>,

    /// Maps a type to its type id.
    ///
    /// Note that different type ids can refer
    /// to types that are structurally equal so this mapping is not a perfect
    /// reverse. In this case this map will recognize such types as equal.
    /// However, as the types are checked using structural equality throughout
    /// typechecking, this collision should not effect it.
    ///
    ///
    /// Because the DB owns the type map but at the same time is needed
    /// to calculate the hash and type equality for the type we need to use a
    /// RefCell to store the type map.
    pub reverse_type_aliases: Rc<RefCell<TypeHashMap<LocalId<TypeId>>>>,

    /// Method blocks within the package in more digestable form.
    pub method_blocks: HashMap<LocalId<DefinitionId>, MethodBlockInfo>,

    /// Class info within the package in more digestable form.
    pub classes: HashMap<LocalId<DefinitionId>, ClassInfo>,

    /// Instances within the package in more digestable form.
    pub instances: HashMap<LocalId<DefinitionId>, InstanceInfo>,

    /// All the methods from instances and method blocks defined within this
    /// package
    pub methods: HashMap<LocalId<DefinitionId>, MethodInfo>,

    /// All the type constraints for any definition within the package
    pub constraints: HashMap<LocalId<DefinitionId>, Vec<GenericConstraint>>,

    /// Namespaces that the definitions has been defined in
    pub namespaces: HashMap<LocalId<DefinitionId>, Vec<String>>,
    /// Mapping from a definition item to a file it has been defined in
    pub files: HashMap<LocalId<DefinitionId>, PathBuf>,
    /// Imports for each given file
    pub imports: HashMap<PathBuf, ()>,
}

#[derive(Error, Debug)]
pub enum QueryError {
    #[error("Definition {0:?} not found")]
    DefinitionNotFound(Id<DefinitionId>),
    #[error("Type {0:?} not found")]
    TypeNotFound(Id<TypeId>),

    #[error("Methods unnsopported for type {0:?}")]
    MethodsUnsupported(Type),

    #[error("Type {0:?} is not hashable")]
    InvalidHash(Type),

    #[error("No corresponding type id for type {0:?}")]
    NoTypeIdForType(Type),

    #[error("Unification error: {0}")]
    UnificationError(#[from] Box<UnificationError>),
}

pub struct MethodHandle {
    name: String,
    definition: DefinitionId,
}

impl Database {
    const INITIAL_TYPE_MAP_CAPACITY: u64 = 1000;

    pub fn new() -> Self {
        Self {
            packages: Vec::new(),
            packages_map: HashMap::new(),
            variable_definitions: HashMap::new(),
            function_definitions: HashMap::new(),
            type_definitions: HashMap::new(),
            method_blocks_definitions: HashMap::new(),
            class_definitions: HashMap::new(),
            instance_definitions: HashMap::new(),
            globals: HashMap::new(),
            definition_types: HashMap::new(),
            typeid_to_type: HashMap::new(),
            reverse_type_aliases: Rc::new(RefCell::new(TypeHashMap::new(
                Self::INITIAL_TYPE_MAP_CAPACITY as u64,
            ))),
            method_blocks: HashMap::new(),
            classes: HashMap::new(),
            instances: HashMap::new(),
            methods: HashMap::new(),
            constraints: HashMap::new(),
            namespaces: HashMap::new(),
            files: Default::default(),
            imports: Default::default(),
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

    pub fn add_type_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::TypeDefinition,
        namespace: &[String],
        file: PathBuf,
    ) {
        let id = LocalId::new(id);
        assert!(!self.type_definitions.contains_key(&id));
        assert!(self
            .globals
            .insert(definition.name.clone(), id.clone())
            .is_none());
        self.type_definitions.insert(id, definition);
        self.namespaces.insert(id, namespace.to_vec());
        self.files.insert(id, file);
    }

    pub fn add_function_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::FunctionDefinition,
        namespace: &[String],
        file: PathBuf,
    ) {
        let id = LocalId::new(id);
        assert!(!self.function_definitions.contains_key(&id));
        assert!(self
            .globals
            .insert(definition.name.clone(), id.clone())
            .is_none());
        self.function_definitions.insert(id, definition);
        self.namespaces.insert(id, namespace.to_vec());
        self.files.insert(id, file);
    }

    pub fn add_const_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::ConstDefinition,
        namespace: &[String],
        file: PathBuf,
    ) {
        let id = LocalId::new(id);
        assert!(!self.variable_definitions.contains_key(&id));
        assert!(self
            .globals
            .insert(definition.name.clone(), id.clone())
            .is_none());
        self.variable_definitions.insert(id, definition);
        self.namespaces.insert(id, namespace.to_vec());
        self.files.insert(id, file);
    }

    pub fn add_method_block_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::MethodsBlock<ast::FunctionDefinition>,
        namespace: &[String],
        file: PathBuf,
    ) {
        let id = LocalId::new(id);
        assert!(!self.method_blocks_definitions.contains_key(&id));
        if definition.name.is_some() {
            assert!(self
                .globals
                .insert(definition.name.clone().unwrap(), id.clone())
                .is_none());
        }
        self.method_blocks_definitions.insert(id, definition);
        self.namespaces.insert(id, namespace.to_vec());
        self.files.insert(id, file);
    }

    pub fn add_class_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::ClassDefinition,
        namespace: &[String],
        file: PathBuf,
    ) {
        let id = LocalId::new(id);
        assert!(!self.class_definitions.contains_key(&id));
        assert!(self
            .globals
            .insert(definition.name.clone(), id.clone())
            .is_none());
        self.class_definitions.insert(id, definition);
        self.namespaces.insert(id, namespace.to_vec());
        self.files.insert(id, file);
    }

    pub fn add_instance_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::InstanceDefinition,
        namespace: &[String],
        file: PathBuf,
    ) {
        let id = LocalId::new(id);
        assert!(!self.instance_definitions.contains_key(&id));
        if definition.name.is_some() {
            assert!(self
                .globals
                .insert(definition.name.clone().unwrap(), id.clone())
                .is_none());
        }
        self.instance_definitions.insert(id, definition);
        self.namespaces.insert(id, namespace.to_vec());
        self.files.insert(id, file);
    }

    pub fn get_global(&self, name: &str) -> Option<LocalId<DefinitionId>> {
        self.globals.get(name).cloned()
    }

    pub fn get_type(&self, id: Id<DefinitionId>) -> Option<&Type> {
        match id {
            Id::Global { package, id } if package == CURRENT_PACKAGE_ID => {
                let id = LocalId::new(id);
                let tid = self.definition_types.get(&id)?;
                self.typeid_to_type.get(&tid)
            }
            Id::Global { package, id } => {
                let id = LocalId::new(id);
                let package_info = self.get_package(package);
                let tid = package_info.definition_types.get(&id)?;
                package_info.typeid_to_type.get(tid)
            }
            Id::Local(id) => {
                let tid = self.definition_types.get(&id)?;
                self.typeid_to_type.get(tid)
            }
        }
    }

    /// Get a type of a global name relative to the current namespace.
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
                    return self.get_type(definition_id.clone().into());
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
        self.get_type(definition_id.clone().into())
    }

    pub fn get_definition_id_by_unresolved_name(
        &self,
        current_namespace: &[String],
        path: &[String],
        name: &str,
    ) -> Option<Id<DefinitionId>> {
        if let Some(potential_package) = path.first() {
            if let Some(package_id) = self.package_id(potential_package) {
                let package_info = self.get_package(package_id.clone());
                let mut expanded_name = path[1..].to_vec();
                expanded_name.push(name.to_string());
                let expanded_name = expanded_name.join("::");
                return package_info
                    .globals
                    .get(&expanded_name)
                    .cloned()
                    .map(|id| id.as_global(package_id));
            }
        }
        let mut expanded_name = current_namespace.to_vec();
        expanded_name.extend(path.iter().cloned());
        expanded_name.push(name.to_string());
        let expand_name = expanded_name.join("::");
        self.globals
            .get(&expand_name)
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
    }

    // Resolving Tids

    pub fn resolve_tid(&self, type_id: Id<TypeId>) -> Result<Type, QueryError> {
        self.resolve_tid_ref(type_id).cloned()
    }

    pub fn resolve_tid_ref(&self, type_id: Id<TypeId>) -> Result<&Type, QueryError> {
        match type_id {
            Id::Global { package, id } if package == CURRENT_PACKAGE_ID => {
                let id = LocalId::new(id);
                self.typeid_to_type
                    .get(&id)
                    .ok_or(QueryError::TypeNotFound(id.into()))
            }
            Id::Global { package, id } => {
                let id = LocalId::new(id);
                let package_info = self.get_package(package);
                package_info
                    .typeid_to_type
                    .get(&id)
                    .ok_or(QueryError::TypeNotFound(id.into()))
            }
            Id::Local(id) => self
                .typeid_to_type
                .get(&id)
                .ok_or(QueryError::TypeNotFound(id.into())),
        }
    }

    pub fn type_id_from_definition(
        &self,
        definition_id: LocalId<DefinitionId>,
    ) -> Result<LocalId<TypeId>, QueryError> {
        self.definition_types
            .get(&definition_id)
            .cloned()
            .ok_or(QueryError::DefinitionNotFound(definition_id.into()))
    }

    // Resolving aliases

    /// Given a type id to the alias resolve it recursively until the type is
    /// concrete.
    pub fn resolve_alias(&self, mut typ: Id<TypeId>) -> Result<Id<TypeId>, QueryError> {
        loop {
            let resolved = self.resolve_tid_ref(typ)?;
            if let Type::Alias(for_type) = resolved {
                typ = for_type.clone();
            } else {
                return Ok(typ);
            }
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
                let t1 = self.resolve_tid_ref(for_type_1)?;
                let t2 = self.resolve_tid_ref(for_type_2)?;
                (t1, t2)
            }
            (&Type::Alias(for_type), t2) => {
                let t1 = self.resolve_tid_ref(for_type)?;
                (t1, t2)
            }
            (t1, &Type::Alias(for_type)) => {
                let t2 = self.resolve_tid_ref(for_type)?;
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
            Type::Struct { def, .. } => def.hash(state),
            Type::ADT { def, .. } => def.hash(state),
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
                let resolved = self.resolve_alias(*for_type)?;
                let resolved = self.resolve_tid_ref(resolved)?;
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

    /// Creates ids for all of the type definitions and classes with the dummy
    /// info.
    ///
    /// This is useful to be able to reference the types and classes for
    /// recursive definitions.
    pub fn create_type_and_class_stumps(&mut self, session: &Session) {
        // create ids for all type definitions
        // for now insert a bogus type there
        for (id, ast_node) in &self.type_definitions {
            let type_id = session.id_provider().next();
            let type_id = LocalId::new(TypeId(type_id));
            self.definition_types.insert(id.clone(), type_id);
            self.typeid_to_type.insert(type_id, Type::Unit);
        }
        // Same for class definitions if we need to look them up
        for (id, ast_node) in &self.class_definitions {
            self.classes.insert(
                id.clone(),
                ClassInfo {
                    name: ast_node.name.clone(),
                    static_methods: Vec::new(),
                    method_blocks: Vec::new(),
                },
            );
        }
    }

    /// Go through all of the type definitions as ast nodes and
    /// lower them into the types used for the typechecking.
    ///
    /// Also puts everything into the correct hashmaps for easy quering.
    pub fn lower_type_definitions(&mut self, session: &Session) {
        let type_definitions = self.type_definitions.clone();
        for (
            id,
            ast::TypeDefinition {
                name,
                definition,
                type_variables,
                span,
            },
        ) in type_definitions
        {
            let namespace = self.namespaces.get(&id).unwrap().clone();
            let mut prefex_scope = PrefexScope::new();
            for var in type_variables.iter() {
                prefex_scope.add_type_var(&var.name);
            }
            let mut t = match definition {
                ast::DefinedType::Record(ast::Record { fields }) => {
                    let fields = fields
                        .iter()
                        .map(|ast::TypedIdentifier { name, typ }| {
                            let t = self.ast_type_to_type_shallow(
                                session,
                                &mut prefex_scope,
                                &namespace,
                                typ,
                            );
                            (name.clone(), t)
                        })
                        .collect();
                    Type::Struct {
                        def: id.as_global(CURRENT_PACKAGE_ID),
                        fields,
                    }
                }
                ast::DefinedType::ADT(ast::ADT { discriminants }) => {
                    let constructors = discriminants
                        .iter()
                        .map(
                            |ast::Discriminant {
                                 constructor,
                                 arguments,
                             }| {
                                (
                                    constructor.clone(),
                                    match arguments {
                                        ast::DiscriminantKind::Empty => Type::Unit,
                                        ast::DiscriminantKind::Tuple(args) => Type::Tuple(
                                            args.iter()
                                                .map(|t| {
                                                    self.ast_type_to_type_shallow(
                                                        session,
                                                        &mut prefex_scope,
                                                        &namespace,
                                                        t,
                                                    )
                                                })
                                                .collect(),
                                        ),
                                        ast::DiscriminantKind::Record(fields) => Type::Struct {
                                            def: Id::Global {
                                                package: CURRENT_PACKAGE_ID,
                                                // Bogus id for the struct, ideally we will never
                                                // try to
                                                // resovle it
                                                id: DefinitionId(session.id_provider().next()),
                                            },
                                            fields: fields
                                                .iter()
                                                .map(|(name, ty)| {
                                                    (
                                                        name.clone(),
                                                        self.ast_type_to_type_shallow(
                                                            session,
                                                            &mut prefex_scope,
                                                            &namespace,
                                                            ty,
                                                        ),
                                                    )
                                                })
                                                .collect(),
                                        },
                                    },
                                )
                            },
                        )
                        .collect();
                    Type::ADT {
                        def: id.as_global(CURRENT_PACKAGE_ID),
                        constructors,
                    }
                }
                ast::DefinedType::Alias(ast::Alias { for_type }) => {
                    self.ast_type_to_type_shallow(session, &mut prefex_scope, &namespace, &for_type)
                }
            };
            if !prefex_scope.is_empty() {
                t = Type::Scheme {
                    prefex: type_variables.iter().map(|v| v.name.clone()).collect(),
                    typ: Box::new(t),
                };
            }
            let tid = self.definition_types.get(&id).unwrap();
            self.typeid_to_type.insert(tid.clone(), t);
        }
    }

    /// Translate ast type to ty::Type.
    ///
    /// Any type references are resolved to their type ids.
    /// Any builtin composite types are created fresh
    fn ast_type_to_type_shallow(
        &mut self,
        session: &Session,
        prefex_scope: &mut PrefexScope,
        namespace: &[String],
        ty: &ast::Typ,
    ) -> Type {
        match ty {
            ast::Typ::Unit => Type::Unit,
            ast::Typ::Name(name) => match name {
                ast::Name::Global {
                    package,
                    definition,
                } => {
                    let package_info = self.get_package(PackageId(*package));
                    let LocalId(typeid) = package_info
                        .definition_types
                        .get(&LocalId::new(DefinitionId(*definition)))
                        .cloned()
                        .unwrap();
                    Type::Alias(Id::Global {
                        package: PackageId(*package),
                        id: typeid,
                    })
                }
                ast::Name::Local(name) => {
                    let def = self.get_global(name).unwrap();
                    let typeid = self.definition_types.get(&def).unwrap();
                    Type::Alias(Id::Local(typeid.clone()))
                }
                ast::Name::Unresolved { path, identifier }
                    if path.is_empty() && prefex_scope.contains(identifier) =>
                {
                    let (shift, position) = prefex_scope.get_with_position(identifier).unwrap();
                    Type::Generic(DeBruijn(shift.try_into().unwrap()), *position)
                }
                ast::Name::Unresolved { path, identifier } => {
                    let id = self
                        .get_definition_id_by_unresolved_name(namespace, path, identifier)
                        .expect(&format!(
                            "expected definition to be found, {:?} {:?}",
                            path, identifier
                        ));
                    match id {
                        Id::Global { package, id } if package == CURRENT_PACKAGE_ID => {
                            let id = LocalId::new(id);
                            assert!(self.type_definitions.contains_key(&id));
                            let tid = self.definition_types.get(&id).unwrap();
                            Type::Alias(tid.as_global(CURRENT_PACKAGE_ID))
                        }
                        Id::Global { package, id } => {
                            let package_info = self.get_package(package);
                            let id = LocalId::new(id);
                            let tid = package_info.definition_types.get(&id).unwrap();
                            Type::Alias(tid.as_global(package))
                        }
                        Id::Local(id) => {
                            assert!(self.type_definitions.contains_key(&id));
                            let tid = self.definition_types.get(&id).unwrap();
                            Type::Alias(tid.as_global(CURRENT_PACKAGE_ID))
                        }
                    }
                }
            },
            ast::Typ::Application { callee, args } => {
                let callee =
                    self.ast_type_to_type_shallow(session, prefex_scope, namespace, callee);
                let args = args
                    .iter()
                    .map(|t| self.ast_type_to_type_shallow(session, prefex_scope, namespace, t))
                    .collect();
                let tid = self.create_type(
                    session,
                    Type::App {
                        typ: Box::new(callee),
                        args,
                    },
                );
                Type::Alias(Id::Local(tid))
            }
            ast::Typ::Array(inner) => {
                let ty = Type::Array(Box::new(self.ast_type_to_type_shallow(
                    session,
                    prefex_scope,
                    namespace,
                    inner,
                )));
                let tid = self.create_type(session, ty);
                Type::Alias(Id::Local(tid))
            }

            ast::Typ::Function { args, ret } => {
                let args = args
                    .iter()
                    .map(|t| self.ast_type_to_type_shallow(session, prefex_scope, namespace, t))
                    .collect();
                let ret =
                    Box::new(self.ast_type_to_type_shallow(session, prefex_scope, namespace, ret));
                let tid = self.create_type(session, Type::Function { args, ret });
                Type::Alias(Id::Local(tid))
            }
            ast::Typ::Tuple(inner) => {
                let inner = inner
                    .iter()
                    .map(|t| self.ast_type_to_type_shallow(session, prefex_scope, namespace, t))
                    .collect();
                let tid = self.create_type(session, Type::Tuple(inner));
                Type::Alias(Id::Local(tid))
            }
            ast::Typ::Poly {
                free_variables,
                bounds,
                typ,
            } if free_variables.is_empty() => {
                self.ast_type_to_type_shallow(session, prefex_scope, namespace, typ)
            }
            ast::Typ::Poly {
                free_variables,
                bounds,
                typ,
            } => {
                panic!(
                    "poly types as arguments and fields are not supported yet {:?} => {:?}",
                    free_variables, typ
                );
            }
            ast::Typ::ToInfere => Type::ToInfere,
        }
    }

    fn ast_type_bound_to_generic_constraint_shallow(
        &mut self,
        session: &Session,
        prefex_scope: &mut PrefexScope,
        bound: &ast::Typ,
        current_namespace: &[String],
    ) -> GenericConstraint {
        match bound {
            ast::Typ::Application {
                callee: box ast::Typ::Name(name),
                args,
            } => {
                let class: Id<DefinitionId> = match name {
                    ast::Name::Unresolved { path, identifier } => {
                        let id = self
                            .get_definition_id_by_unresolved_name(
                                current_namespace,
                                path,
                                identifier,
                            )
                            .unwrap();
                        match id {
                            Id::Global { package, id } if package == CURRENT_PACKAGE_ID => {
                                let id = LocalId::new(id);
                                assert!(self.classes.contains_key(&id));
                                id.as_global(CURRENT_PACKAGE_ID)
                            }
                            Id::Global { package, id } => {
                                let package_info = self.get_package(package);
                                let id = LocalId::new(id);
                                assert!(package_info.classes.contains_key(&id));
                                id.as_global(package)
                            }
                            Id::Local(id) => {
                                assert!(self.classes.contains_key(&id));
                                id.as_global(CURRENT_PACKAGE_ID)
                            }
                        }
                    }
                    ast::Name::Global {
                        package,
                        definition,
                    } => {
                        let package = PackageId(*package);
                        let id = LocalId::new(DefinitionId(*definition));
                        let package_info = self.get_package(package);
                        assert!(package_info.classes.contains_key(&id));
                        id.as_global(package)
                    }
                    ast::Name::Local(id) => {
                        let id = self.globals.get(id).unwrap();
                        assert!(self.classes.contains_key(id));
                        id.as_global(CURRENT_PACKAGE_ID)
                    }
                };
                let args = args
                    .iter()
                    .map(|t| {
                        self.ast_type_to_type_shallow(session, prefex_scope, current_namespace, t)
                    })
                    .collect();
                GenericConstraint { class, args }
            }
            _ => panic!("Type bound needs to be an application of a class"),
        }
    }
    /// Lookups type into the reverse hash to get its type id.
    /// If the type is not present in the hash it is added with a fresh type id.
    pub fn create_type(&mut self, session: &Session, ty: Type) -> LocalId<TypeId> {
        let lookup = self
            .reverse_type_aliases
            .borrow()
            .get(&self, &ty)
            .unwrap()
            .cloned();
        match lookup {
            Some(tid) => tid,
            None => {
                let id = session.id_provider().next();
                let id = LocalId::new(TypeId(id));
                self.typeid_to_type.insert(id.clone(), ty.clone());
                self.insert_into_type_map(ty, id.clone());
                id
            }
        }
    }

    /// Inserts value into the type map.
    ///
    /// Because the DB owns the type map but at the same time is needed
    /// to calculate the hash for the type we need to use a RefCell to store the
    /// type map.
    ///
    /// This is a helper method that makes the user skip the manual borrowing.
    pub fn insert_into_type_map(&mut self, ty: Type, tid: LocalId<TypeId>) {
        let reverse_type_aliases = self.reverse_type_aliases.clone();
        reverse_type_aliases
            .borrow_mut()
            .insert(&self, ty, tid)
            .unwrap();
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

pub struct PopulateDbTypeDefinitions<'a, 's> {
    pub session: &'s Session,
    pub db: &'a mut Database,
}

impl Database {
    pub fn dump_types(&self) {
        for (id, t) in &self.typeid_to_type {
            println!("{:?} => {}", id.0, t.pretty());
        }
    }
}
