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

#[derive(Debug, Clone)]
pub struct GenericConstraint {
    /// Class used for the constraint
    pub class: Id<DefinitionId>,
    /// Arguments used for the constraint
    pub args: Vec<Type>,
}

const DUMMY_TYPE_ID: LocalId<TypeId> = LocalId(TypeId(0));

/// Might reference a definition from another package or the current one.
///
/// If the package is equal to CURRENT_PACKAGE_ID then the id is local to the
/// currently compiled package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id<T> {
    pub package: PackageId,
    pub id: T,
}

impl<T: Default> Id<T> {
    pub fn dummy() -> Self {
        Self {
            package: CURRENT_PACKAGE_ID,
            id: T::default(),
        }
    }
}

/// Id referencing package the item is in.
///
/// For example a local type id of a function references a type
/// accessible from the package the function is in.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct LocalId<T>(pub T);

impl<T> LocalId<T> {
    pub fn new(id: T) -> Self {
        Self(id)
    }

    pub fn as_global(self, package: PackageId) -> Id<T> {
        Id {
            package,
            id: self.0,
        }
    }
}

/// Id that identifies a definition within a package
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct DefinitionId(pub UniqueId);

impl Default for DefinitionId {
    fn default() -> Self {
        Self(0)
    }
}

/// Id that identifies a type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeId(pub UniqueId);

impl Default for TypeId {
    fn default() -> Self {
        Self(0)
    }
}

/// Id that identifies a package
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct PackageId(pub UniqueId);

impl Default for PackageId {
    fn default() -> Self {
        Self(0)
    }
}

pub const CURRENT_PACKAGE_ID: PackageId = PackageId(0);

/// Information about a method blocks defined within a class
#[derive(Debug, Clone)]
pub struct ClassMethodBlock {
    /// References methods block definition.
    ///
    /// Use it to retrieve the methods from the method blocks
    pub id: LocalId<DefinitionId>,
    /// Receiver type for the methods block
    pub receiver: Id<TypeId>,
}

impl ClassMethodBlock {
    pub fn dummy() -> Self {
        Self {
            id: LocalId::new(DefinitionId::default()),
            receiver: Id::dummy(),
        }
    }
}

/// Information about a class definition
#[derive(Debug, Clone)]
pub struct ClassInfo {
    pub name: String,
    // Generic arguments for the class instance
    pub arguments: Vec<String>,
    // Collection of all the static methods defined within the class
    // and their types
    pub static_methods: Vec<MethodHandle>,
    /// Method blocks defined within the class
    pub method_blocks: Vec<ClassMethodBlock>,
}

impl ClassInfo {
    pub const DUMMY: Self = Self {
        name: String::new(),
        arguments: Vec::new(),
        static_methods: Vec::new(),
        method_blocks: Vec::new(),
    };
}

#[derive(Debug, Clone)]
/// Information about a method blocks within an instance
pub struct InstanceMethodBlock {
    /// References method blocks definition. Access its kind to get methods
    pub id: LocalId<DefinitionId>,
    /// Receiver type for the methods block
    pub receiver: Id<TypeId>,
}

impl InstanceMethodBlock {
    pub fn dummy() -> Self {
        Self {
            id: LocalId::new(DefinitionId::default()),
            receiver: Id::dummy(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstanceInfo {
    /// Optional name of the instance
    pub name: Option<String>,
    /// Free type variables within the receiver type
    pub free_vars: Vec<String>,
    /// Type of the reeiver the instance is defined for, contains name of the
    /// typeclass and its arguments
    pub receiver: GenericConstraint,
    /// Static methods implementations defined within the instance
    pub static_methods: Vec<MethodHandle>,
    /// Method blocks implemented within the instance
    pub method_blocks: Vec<InstanceMethodBlock>,
    /// Should be autoimported if the given definition is autoimported
    /// if none it should not be autoimported
    pub autoimported: Option<LocalId<DefinitionId>>,
}

impl InstanceInfo {
    pub fn dummy() -> Self {
        Self {
            name: None,
            free_vars: Vec::new(),
            receiver: GenericConstraint {
                class: Id::dummy(),
                args: Vec::new(),
            },
            static_methods: Vec::new(),
            method_blocks: Vec::new(),
            autoimported: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodBlockInfo {
    pub name: Option<String>,
    /// Receiver type for the methods block
    pub receiver: Id<TypeId>,
    /// Methods definitions
    pub methods: Vec<MethodHandle>,
    pub free_vars: Vec<String>,
}

impl MethodBlockInfo {
    pub fn dummy() -> Self {
        Self {
            name: None,
            receiver: Id::dummy(),
            methods: Vec::new(),
            free_vars: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub ty: LocalId<TypeId>,
}

impl MethodInfo {
    pub const DUMMY: Self = Self {
        name: String::new(),
        ty: DUMMY_TYPE_ID,
    };
}

pub struct FileInfo {
    pub path: PathBuf,
    pub namespace: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum DefinitionKind {
    ConstVar,
    Method(MethodInfo),
    ClassMethod,
    MethodBlock(MethodBlockInfo),
    Class(ClassInfo),
    Instance(InstanceInfo),
    Type,
    Function,
}

impl DefinitionKind {
    pub fn is_class(&self) -> bool {
        matches!(self, Self::Class(_))
    }

    pub fn as_class(&self) -> Option<&ClassInfo> {
        match self {
            Self::Class(info) => Some(info),
            _ => None,
        }
    }

    pub fn as_method_block(&self) -> Option<&MethodBlockInfo> {
        match self {
            Self::MethodBlock(info) => Some(info),
            _ => None,
        }
    }

    pub fn as_method_block_mut(&mut self) -> Option<&mut MethodBlockInfo> {
        match self {
            Self::MethodBlock(info) => Some(info),
            _ => None,
        }
    }

    pub fn as_instance(&self) -> Option<&InstanceInfo> {
        match self {
            Self::Instance(info) => Some(info),
            _ => None,
        }
    }

    pub fn as_method(&self) -> Option<&MethodInfo> {
        match self {
            Self::Method(info) => Some(info),
            _ => None,
        }
    }
}

/// Description of the defnition.
///
/// Depending on the kind of the definition it will have different fields
/// filled.
#[derive(Debug, Clone)]
pub struct Definition {
    /// Name of the item, if the name has not been provided then its empty
    pub name: String,
    /// Kind of the definition
    pub kind: DefinitionKind,
    /// Generic constraints associated with the definition
    pub constraints: Vec<GenericConstraint>,
    /// Type associated with the definition
    pub ty: LocalId<TypeId>,
    /// File in which the defnition is in
    pub file: LocalId<DefinitionId>,
    /// Items that should be imported whenever this defintion is
    pub implicit_imports: Vec<LocalId<DefinitionId>>,
    /// Possible parent of the definition, for methods it could be their method
    /// block for method blocks it might be their instance, class or just
    /// nothing.
    pub parent: Option<LocalId<DefinitionId>>,
}

impl Definition {
    pub fn is_class(&self) -> bool {
        self.kind.is_class()
    }
}

pub struct PackageInfo {
    /// Name of the package
    pub name: String,
    /// All the globaly visible definitions within the package
    pub globals: HashMap<String, LocalId<DefinitionId>>,
    /// Used to lookup type id into a type
    pub typeid_to_type: HashMap<LocalId<TypeId>, Type>,
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
    /// All the defnitions within the package, functions, methods, instances,
    pub definition: HashMap<LocalId<DefinitionId>, Definition>,
}

impl PackageInfo {
    pub fn empty(name: &str) -> Self {
        Self {
            name: name.to_string(),
            globals: HashMap::new(),
            typeid_to_type: HashMap::new(),
            method_blocks: HashMap::new(),
            classes: HashMap::new(),
            instances: HashMap::new(),
            methods: HashMap::new(),
            definition: Default::default(),
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

    /// Information about the files
    pub file_info: HashMap<LocalId<DefinitionId>, FileInfo>,

    /// All the defnitions within the package, functions, methods, instances,
    /// classes, method blocks, types and so on.
    pub definitions: HashMap<LocalId<DefinitionId>, Definition>,

    /// Mapping of the primitive types to their type ids
    pub primitive_types: HashMap<Type, Id<DefinitionId>>,
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

#[derive(Debug, Clone)]
pub struct MethodHandle {
    pub name: String,
    pub definition: LocalId<DefinitionId>,
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
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
            typeid_to_type: HashMap::new(),
            reverse_type_aliases: Rc::new(RefCell::new(TypeHashMap::new(
                Self::INITIAL_TYPE_MAP_CAPACITY,
            ))),
            file_info: Default::default(),
            definitions: Default::default(),
            primitive_types: Default::default(),
        }
    }

    pub fn add_primitive_type(&mut self, typ: Type, id: Id<DefinitionId>) {
        self.primitive_types.insert(typ, id);
    }

    pub fn get_primitive_type(&self, typ: &Type) -> Option<Id<DefinitionId>> {
        self.primitive_types.get(typ).cloned()
    }

    // Package queries
    pub fn add_package(&mut self, package: PackageInfo) {
        self.packages_map
            .insert(package.name.clone(), PackageId(self.packages.len() + 1));
        self.packages.push(package);
    }

    pub fn get_package(&self, id: PackageId) -> &PackageInfo {
        if id == CURRENT_PACKAGE_ID {
            panic!("Cannot get current package info, use db instead")
        }
        // this is important as the current package id is 0
        // so we need to subtract 1 to get the correct index
        // into the dependencies vector
        &self.packages[id.0 - 1]
    }

    pub fn package_id(&self, of: &str) -> Option<PackageId> {
        self.packages_map.get(of).cloned()
    }

    pub fn package_info(&self, of: PackageId) -> Option<&PackageInfo> {
        self.packages.get(of.0)
    }
    // Definitions

    pub fn add_file(
        &mut self,
        session: &Session,
        file: impl Into<PathBuf>,
        namespace: &[String],
    ) -> LocalId<DefinitionId> {
        let id = LocalId::new(DefinitionId(session.id_provider().next()));
        self.file_info.insert(
            id,
            FileInfo {
                path: file.into(),
                namespace: namespace.to_vec(),
            },
        );
        id
    }

    pub fn add_type_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::TypeDefinition,
        file: LocalId<DefinitionId>,
    ) {
        let id = LocalId::new(id);
        assert!(!self.type_definitions.contains_key(&id));
        assert!(self.globals.insert(definition.name.clone(), id).is_none());
        self.definitions.insert(
            id,
            Definition {
                name: definition.name.clone(),
                kind: DefinitionKind::Type,
                constraints: Vec::new(),
                implicit_imports: Default::default(),
                ty: DUMMY_TYPE_ID,
                file,
                parent: None,
            },
        );
        self.type_definitions.insert(id, definition);
    }

    pub fn add_function_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::FunctionDefinition,
        file: LocalId<DefinitionId>,
    ) {
        let id = LocalId::new(id);
        assert!(!self.function_definitions.contains_key(&id));
        assert!(self.globals.insert(definition.name.clone(), id).is_none());

        self.definitions.insert(
            id,
            Definition {
                name: definition.name.clone(),
                kind: DefinitionKind::Function,
                implicit_imports: Default::default(),
                constraints: Vec::new(),
                ty: DUMMY_TYPE_ID,
                file,
                parent: None,
            },
        );
        self.function_definitions.insert(id, definition);
    }

    pub fn add_const_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::ConstDefinition,
        file: LocalId<DefinitionId>,
    ) {
        let id = LocalId::new(id);
        assert!(!self.variable_definitions.contains_key(&id));
        assert!(self.globals.insert(definition.name.clone(), id).is_none());

        self.definitions.insert(
            id,
            Definition {
                name: definition.name.clone(),
                kind: DefinitionKind::ConstVar,
                implicit_imports: Default::default(),
                constraints: Vec::new(),
                ty: DUMMY_TYPE_ID,
                file,
                parent: None,
            },
        );
        self.variable_definitions.insert(id, definition);
    }

    pub fn add_method_block_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::MethodsBlock<ast::FunctionDefinition>,
        file: LocalId<DefinitionId>,
    ) {
        let id = LocalId::new(id);
        assert!(!self.method_blocks_definitions.contains_key(&id));
        if definition.name.is_some() {
            assert!(self
                .globals
                .insert(definition.name.clone().unwrap(), id)
                .is_none());
        }

        self.definitions.insert(
            id,
            Definition {
                name: definition.name.clone().unwrap_or_default(),
                kind: DefinitionKind::MethodBlock(MethodBlockInfo::dummy()),
                implicit_imports: Default::default(),
                constraints: Vec::new(),
                ty: DUMMY_TYPE_ID,
                file,
                parent: None,
            },
        );
        self.method_blocks_definitions.insert(id, definition);
    }

    pub fn add_class_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::ClassDefinition,
        file: LocalId<DefinitionId>,
    ) {
        let id = LocalId::new(id);
        assert!(!self.class_definitions.contains_key(&id));
        assert!(self.globals.insert(definition.name.clone(), id).is_none());
        self.definitions.insert(
            id,
            Definition {
                name: definition.name.clone(),
                implicit_imports: Default::default(),
                kind: DefinitionKind::Class(ClassInfo::DUMMY.clone()),
                constraints: Vec::new(),
                ty: DUMMY_TYPE_ID,
                file,
                parent: None,
            },
        );
        self.class_definitions.insert(id, definition);
    }

    pub fn add_instance_definition(
        &mut self,
        id: DefinitionId,
        definition: ast::InstanceDefinition,
        file: LocalId<DefinitionId>,
    ) {
        let id = LocalId::new(id);
        assert!(!self.instance_definitions.contains_key(&id));
        if definition.name.is_some() {
            assert!(self
                .globals
                .insert(definition.name.clone().unwrap(), id)
                .is_none());
        }
        self.definitions.insert(
            id,
            Definition {
                name: definition.name.clone().unwrap_or_default(),
                kind: DefinitionKind::Instance(InstanceInfo::dummy()),
                implicit_imports: Default::default(),
                constraints: Vec::new(),
                ty: DUMMY_TYPE_ID,
                file,
                parent: None,
            },
        );
        self.instance_definitions.insert(id, definition);
    }

    pub fn get_global(&self, name: &str) -> Option<LocalId<DefinitionId>> {
        self.globals.get(name).cloned()
    }

    pub fn get_instance(&self, id: Id<DefinitionId>) -> Option<InstanceInfo> {
        self.get_definition_map(id, |def| def.kind.as_instance().cloned().unwrap())
    }

    pub fn get_class(&self, id: Id<DefinitionId>) -> Option<ClassInfo> {
        self.get_definition_map(id, |def| def.kind.as_class().cloned().unwrap())
    }

    pub fn get_definition<'a>(&'a self, id: Id<DefinitionId>) -> Option<Definition> {
        self.get_definition_map(id, |def| def.clone())
    }

    pub fn get_definition_map<R>(
        &self,
        id: Id<DefinitionId>,
        func: impl FnOnce(&Definition) -> R,
    ) -> Option<R> {
        match id {
            Id { package, id } if package == CURRENT_PACKAGE_ID => {
                let id = LocalId::new(id);
                let def = self.definitions.get(&id)?;
                Some(func(def))
            }
            Id { package, id } => {
                let id = LocalId::new(id);
                let package_info = self.get_package(package);
                let definition = package_info.definition.get(&id)?;
                Some(func(definition))
            }
        }
    }

    pub fn get_type(&self, id: Id<DefinitionId>) -> Option<&Type> {
        match id {
            Id { package, id } if package == CURRENT_PACKAGE_ID => {
                let id = LocalId::new(id);
                let tid = self.type_id_from_definition(id).unwrap();
                self.typeid_to_type.get(&tid)
            }
            Id { package, id } => {
                let id = LocalId::new(id);
                let package_info = self.get_package(package);
                let tid = package_info.definition.get(&id)?.ty;
                package_info.typeid_to_type.get(&tid)
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
                let package_info = self.get_package(package_id);
                let mut expanded_name = path[1..].to_vec();
                expanded_name.push(name.to_string());
                let expanded_name = expanded_name.join("::");
                if let Some(definition_id) = package_info.globals.get(&expanded_name) {
                    return self.get_type((*definition_id).as_global(package_id));
                }
                return None;
            }
        }
        let mut expanded_name = current_namespace.to_vec();
        expanded_name.extend(path.iter().cloned());
        expanded_name.push(name.to_string());
        let expand_name = expanded_name.join("::");
        println!("Expanded name {expand_name}");
        let Some(definition_id) = self.globals.get(&expand_name) else {
            return None;
        };
        self.get_type((*definition_id).as_global(CURRENT_PACKAGE_ID))
    }

    pub fn get_definition_id_by_unresolved_name(
        &self,
        current_namespace: &[String],
        path: &[String],
        name: &str,
    ) -> Option<Id<DefinitionId>> {
        if let Some(potential_package) = path.first() {
            if let Some(package_id) = self.package_id(potential_package) {
                let package_info = self.get_package(package_id);
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
            Id { package, id } if package == CURRENT_PACKAGE_ID => {
                let id = LocalId::new(id);
                self.typeid_to_type
                    .get(&id)
                    .ok_or(QueryError::TypeNotFound(id.as_global(package)))
            }
            Id { package, id } => {
                let id = LocalId::new(id);
                let package_info = self.get_package(package);
                package_info
                    .typeid_to_type
                    .get(&id)
                    .ok_or(QueryError::TypeNotFound(id.as_global(package)))
            }
        }
    }

    pub fn type_id_from_definition(
        &self,
        definition_id: LocalId<DefinitionId>,
    ) -> Result<LocalId<TypeId>, QueryError> {
        self.definitions
            .get(&definition_id)
            .ok_or_else(|| {
                QueryError::DefinitionNotFound(definition_id.as_global(CURRENT_PACKAGE_ID))
            })
            .map(|x| x.ty)
    }

    // Resolving aliases

    /// Given a type id to the alias resolve it recursively until the type is
    /// concrete.
    pub fn resolve_alias(&self, mut typ: Id<TypeId>) -> Result<Id<TypeId>, QueryError> {
        loop {
            let resolved = self.resolve_tid_ref(typ)?;
            if let Type::Alias(for_type) = resolved {
                typ = *for_type;
            } else {
                return Ok(typ);
            }
        }
    }

    pub fn resolve_if_alias(&self, typ: &Type) -> Result<Type, QueryError> {
        match typ {
            Type::Alias(for_type) => self.resolve_alias_to_type(*for_type),
            _ => Ok(typ.clone()),
        }
    }

    pub fn resolve_alias_to_type(&self, typ: Id<TypeId>) -> Result<Type, QueryError> {
        let resolved = self.resolve_alias(typ)?;
        self.resolve_tid(resolved)
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
                self.hash_type_with_hasher(resolved, state)?;
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
        for id in self.type_definitions.keys() {
            let type_id = session.id_provider().next();
            let type_id = LocalId::new(TypeId(type_id));
            self.definitions
                .entry(*id)
                .and_modify(|def| def.ty = type_id);
            self.typeid_to_type.insert(type_id, Type::Unit);
        }
        // Same for class definitions if we need to look them up
        for (id, ast_node) in &self.class_definitions {
            let class = self.definitions.get_mut(id).unwrap();
            class.kind = DefinitionKind::Class(ClassInfo {
                name: ast_node.name.clone(),
                arguments: ast_node.args.clone(),
                static_methods: Vec::new(),
                method_blocks: Vec::new(),
            });
        }
    }

    pub fn get_namespace(&self, id: LocalId<DefinitionId>) -> &[String] {
        let file_id = self.definitions.get(&id).unwrap().file;
        self.file_info.get(&file_id).unwrap().namespace.as_slice()
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
                name: _,
                definition,
                type_variables,
                span: _,
                constraints,
            },
        ) in type_definitions
        {
            let namespace = self.get_namespace(id).to_vec();
            let mut prefex_scope = PrefexScope::with_empty_scope();
            prefex_scope.add_type_vars(&type_variables);
            let constraints = constraints
                .iter()
                .map(|c| {
                    self.ast_type_bound_to_generic_constraint_shallow(
                        session,
                        &mut prefex_scope,
                        c,
                        &namespace,
                    )
                })
                .collect();
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
                                            def: Id {
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
                    prefex: type_variables.clone(),
                    typ: Box::new(t),
                };
            }
            let tid = self.type_id_from_definition(id).unwrap();
            self.typeid_to_type.insert(tid, t);
            self.definitions.get_mut(&id).unwrap().constraints = constraints;
        }
    }

    pub fn lower_class_definitions(&mut self, session: &Session) {
        let class_definitions = self.class_definitions.clone();
        for (
            id,
            ast::ClassDefinition {
                name: _,
                bounds,
                args,
                body,
                ..
            },
        ) in class_definitions
        {
            let namespace = self.get_namespace(id).to_vec();
            let file = self.definitions.get(&id).unwrap().file;
            let mut prefex_scope = PrefexScope::with_empty_scope();
            prefex_scope.add_type_vars(&args);
            let constrains = bounds
                .iter()
                .map(|bound| {
                    self.ast_type_bound_to_generic_constraint_shallow(
                        session,
                        &mut prefex_scope,
                        bound,
                        &namespace,
                    )
                })
                .collect::<Vec<_>>();
            let mut static_methods = Vec::new();
            let mut method_blocks: Vec<ClassMethodBlock> = Vec::new();
            for bitem in &body {
                match bitem {
                    ast::ClassDefinitionItem::MethodBlock(ast::MethodsBlock {
                        name,
                        typ,
                        methods,
                    }) => {
                        if name.is_some() {
                            panic!("named method blocks within classes are not supported")
                        }
                        let mut block_bounds = Vec::new();
                        let mut block_free_vars = Vec::new();
                        let mut block_receiver = typ.clone();
                        if let Some((free_variables, bounds, typ)) = self.unwrap_poly_type(&typ) {
                            block_bounds = bounds.into();
                            block_free_vars = free_variables.into();
                            block_receiver = typ.clone();
                        }
                        if !block_bounds.is_empty() {
                            panic!("bounds on method blocks within classes are not supported")
                        }
                        if !block_free_vars.is_empty() {
                            prefex_scope.new_scope();
                            prefex_scope.add_type_vars(&block_free_vars);
                        }

                        let block_receiver = self.ast_type_to_type_shallow(
                            session,
                            &mut prefex_scope,
                            &namespace,
                            &block_receiver,
                        );
                        let receiver = self.create_type(session, block_receiver);
                        let mut method_block_definition = Definition {
                            name: "".to_string(),
                            kind: DefinitionKind::MethodBlock(MethodBlockInfo {
                                name: None,
                                receiver: receiver.as_global(CURRENT_PACKAGE_ID),
                                methods: vec![],
                                free_vars: block_free_vars,
                            }),
                            constraints: vec![],
                            ty: receiver.clone(),
                            file,
                            implicit_imports: vec![],
                            parent: Some(id),
                        };
                        let method_block_id =
                            LocalId::new(DefinitionId(session.id_provider().next()));
                        for ast::Method { item: meth, .. } in methods {
                            let method_handle = MethodHandle {
                                name: meth.name.clone(),
                                definition: self.lower_method(
                                    &mut prefex_scope,
                                    session,
                                    &namespace,
                                    file,
                                    method_block_id,
                                    &meth.name,
                                    &meth.typ,
                                ),
                            };
                            method_block_definition
                                .kind
                                .as_method_block_mut()
                                .unwrap()
                                .methods
                                .push(method_handle);
                        }
                        if !block_bounds.is_empty() {
                            prefex_scope.pop_scope();
                        }
                        method_blocks.push(ClassMethodBlock {
                            id: method_block_id,
                            receiver: receiver.as_global(CURRENT_PACKAGE_ID),
                        });
                        self.definitions
                            .insert(method_block_id, method_block_definition);
                    }
                    ast::ClassDefinitionItem::Function(ast::FuncDecl { name, typ }) => {
                        static_methods.push(MethodHandle {
                            name: name.clone(),
                            definition: self.lower_method(
                                &mut prefex_scope,
                                session,
                                &namespace,
                                file,
                                id,
                                name,
                                typ,
                            ),
                        });
                    }
                } // end match bitem
            } // end for bitem in body
            let class = self.definitions.get_mut(&id).unwrap();
            *class = Definition {
                kind: DefinitionKind::Class(ClassInfo {
                    static_methods,
                    method_blocks,
                    name: class.name.clone(),
                    arguments: args.clone(),
                }),
                constraints: constrains,
                ..class.clone()
            };
        } // end for def in class_definitions
    }

    pub fn lower_method_blocks(&mut self, session: &Session) {
        let method_blocks = self.method_blocks_definitions.clone();
        for (id, ast::MethodsBlock { typ, methods, .. }) in method_blocks.iter() {
            let namespace = self.get_namespace(*id).to_vec();
            let file = self.definitions.get(id).unwrap().file;

            let mut block_bounds = Vec::new();
            let mut block_free_vars = Vec::new();
            let mut block_receiver = typ.clone();

            println!("AAAAAAAA Method block type {typ:?}");
            if let Some((free_variables, bounds, typ)) = self.unwrap_poly_type(typ) {
                block_bounds = bounds.into();
                block_free_vars = free_variables.into();
                block_receiver = typ.clone();
            }
            println!("AAAAAAAAAA Block bounds {block_bounds:?}");
            let mut prefex_scope = PrefexScope::with_empty_scope();
            prefex_scope.add_type_vars(&block_free_vars);

            let block_constraints = block_bounds
                .iter()
                .map(|bound| {
                    self.ast_type_bound_to_generic_constraint_shallow(
                        session,
                        &mut prefex_scope,
                        bound,
                        &namespace,
                    )
                })
                .collect::<Vec<_>>();
            let block_receiver = self.ast_type_to_type_shallow(
                session,
                &mut prefex_scope,
                &namespace,
                &block_receiver,
            );
            let mut block_methods = Vec::new();
            for ast::Method {
                item: ast::FunctionDefinition { name, typ, .. },
                ..
            } in methods
            {
                block_methods.push(MethodHandle {
                    name: name.clone(),
                    definition: self.lower_method(
                        &mut prefex_scope,
                        session,
                        &namespace,
                        file,
                        *id,
                        name,
                        typ,
                    ),
                });
            } // end for method in methods
            let receiver = self
                .create_type(session, block_receiver)
                .as_global(CURRENT_PACKAGE_ID);
            let method_block = self.definitions.get_mut(id).unwrap();
            *method_block = Definition {
                kind: DefinitionKind::MethodBlock(MethodBlockInfo {
                    receiver,
                    methods: block_methods,
                    free_vars: block_free_vars,
                    name: if method_block.name.is_empty() {
                        None
                    } else {
                        Some(method_block.name.clone())
                    },
                }),
                constraints: block_constraints,
                ..method_block.clone()
            };
        } // end for block in method_blocks
    }

    pub fn lower_instances(&mut self, session: &Session) {
        let instance_definitions = self.instance_definitions.clone();
        for (
            id,
            ast::InstanceDefinition {
                name,
                free_variables,
                bounds,
                instanced_class,
                body,
            },
        ) in &instance_definitions
        {
            println!("Lowering instance {name:?}");
            let namespace = self.get_namespace(*id).to_vec();
            let file = self.definitions.get(id).unwrap().file;
            let mut prefex_scope = PrefexScope::with_empty_scope();
            println!("Free vars are {free_variables:?}");
            prefex_scope.add_type_vars(&free_variables);
            let constrains = bounds
                .iter()
                .map(|bound| {
                    self.ast_type_bound_to_generic_constraint_shallow(
                        session,
                        &mut prefex_scope,
                        bound,
                        &namespace,
                    )
                })
                .collect::<Vec<_>>();
            let receiver = self.ast_type_bound_to_generic_constraint_shallow(
                session,
                &mut prefex_scope,
                instanced_class,
                &namespace,
            );
            let mut static_methods = Vec::new();
            let mut method_blocks = Vec::new();
            for instance_item in body {
                match instance_item {
                    ast::InstanceDefinitionItem::FunctionDefinition(ast::FunctionDefinition {
                        name,
                        typ,
                        ..
                    }) => {
                        println!("Lowering instance method {name:?}");
                        static_methods.push(MethodHandle {
                            name: name.clone(),
                            definition: self.lower_method(
                                &mut prefex_scope,
                                session,
                                &namespace,
                                file,
                                *id,
                                name,
                                typ,
                            ),
                        });
                    }
                    ast::InstanceDefinitionItem::MethodsBlock(ast::MethodsBlock {
                        name,
                        typ,
                        methods,
                    }) => {
                        println!("Lowering instance methods block {typ:?}");
                        if name.is_some() {
                            panic!("named method blocks within instances are not supported")
                        }
                        let mut block_bounds = Vec::new();
                        let mut block_free_vars = Vec::new();
                        let mut block_receiver = typ.clone();
                        if let Some((free_variables, bounds, typ)) = self.unwrap_poly_type(&typ) {
                            block_bounds = bounds.into();
                            block_free_vars = free_variables.into();
                            block_receiver = typ.clone();
                        }
                        if !block_bounds.is_empty() {
                            panic!("bounds on method blocks within instances are not supported")
                        }
                        if !block_free_vars.is_empty() {
                            prefex_scope.new_scope();
                            prefex_scope.add_type_vars(&block_free_vars);
                        }

                        let block_receiver = self.ast_type_to_type_shallow(
                            session,
                            &mut prefex_scope,
                            &namespace,
                            &block_receiver,
                        );
                        let receiver = self.create_type(session, block_receiver);
                        let method_blocks_id =
                            LocalId::new(DefinitionId(session.id_provider().next()));
                        let mut method_blocks_definition = Definition {
                            name: "".to_string(),
                            kind: DefinitionKind::MethodBlock(MethodBlockInfo {
                                name: None,
                                receiver: receiver.as_global(CURRENT_PACKAGE_ID),
                                methods: vec![],
                                free_vars: block_free_vars,
                            }),
                            constraints: vec![],
                            ty: receiver.clone(),
                            file,
                            implicit_imports: vec![],
                            parent: Some(*id),
                        };
                        for ast::Method { item: meth, .. } in methods {
                            let method_handle = MethodHandle {
                                name: meth.name.clone(),
                                definition: self.lower_method(
                                    &mut prefex_scope,
                                    session,
                                    &namespace,
                                    file,
                                    method_blocks_id,
                                    &meth.name,
                                    &meth.typ,
                                ),
                            };
                            method_blocks_definition
                                .kind
                                .as_method_block_mut()
                                .unwrap()
                                .methods
                                .push(method_handle);
                        }
                        if !block_bounds.is_empty() {
                            prefex_scope.pop_scope();
                        }
                        method_blocks.push(InstanceMethodBlock {
                            id: method_blocks_id,
                            receiver: receiver.as_global(CURRENT_PACKAGE_ID),
                        });
                        self.definitions
                            .insert(method_blocks_id, method_blocks_definition);
                    }
                }
            }
            let instance = self.definitions.get_mut(id).unwrap();
            *instance = Definition {
                kind: DefinitionKind::Instance(InstanceInfo {
                    receiver,
                    static_methods,
                    method_blocks,
                    autoimported: None,
                    name: name.clone(),
                    free_vars: free_variables.clone(),
                }),
                constraints: constrains,
                ..instance.clone()
            };
        }
    }

    pub fn lower_method(
        &mut self,
        prefex_scope: &mut PrefexScope,
        session: &Session,
        namespace: &[String],
        file: LocalId<DefinitionId>,
        parent: LocalId<DefinitionId>,
        name: &str,
        typ: &ast::Typ,
    ) -> LocalId<DefinitionId> {
        let mut f_bounds = Vec::new();
        let mut f_free_vars = Vec::new();
        let mut f_typ = typ.clone();
        if let Some((free_variables, bounds, typ)) = self.unwrap_poly_type(&typ) {
            f_bounds = bounds.into();
            f_free_vars = free_variables.into();
            f_typ = typ.clone();
        }
        if !f_free_vars.is_empty() {
            prefex_scope.new_scope();
            prefex_scope.add_type_vars(&f_free_vars);
        }
        let f_typ = self.ast_type_to_type_shallow(session, prefex_scope, &namespace, &f_typ);
        let f_constraints = f_bounds
            .iter()
            .map(|bound| {
                self.ast_type_bound_to_generic_constraint_shallow(
                    session,
                    prefex_scope,
                    bound,
                    &namespace,
                )
            })
            .collect::<Vec<_>>();
        if !f_free_vars.is_empty() {
            prefex_scope.pop_scope();
        }
        let ty = self.create_type(session, f_typ);
        let definition = Definition {
            name: name.to_owned(),
            kind: DefinitionKind::Method(MethodInfo {
                name: name.to_owned(),
                ty,
            }),
            constraints: f_constraints,
            ty,
            file,
            implicit_imports: Vec::new(),
            parent: Some(parent),
        };
        self.add_definition(session, definition)
    }

    pub fn lower_functions(&mut self, session: &Session) {
        let function_definitions = self.function_definitions.clone();
        for (
            id,
            ast::FunctionDefinition {
                name,
                typ,
                parameters,
                body,
                attributes,
            },
        ) in function_definitions.iter()
        {
            let namespace = self.get_namespace(*id).to_vec();
            let file = self.definitions.get(id).unwrap().file;
            let mut prefex_scope = PrefexScope::with_empty_scope();
            let mut f_bounds = Vec::new();
            let mut f_free_vars = Vec::new();
            let mut f_typ = typ.clone();
            if let Some((free_variables, bounds, typ)) = self.unwrap_poly_type(&typ) {
                f_bounds = bounds.into();
                f_free_vars = free_variables.into();
                f_typ = typ.clone();
            }
            prefex_scope.add_type_vars(&f_free_vars);
            let f_typ =
                self.ast_type_to_type_shallow(session, &mut prefex_scope, &namespace, &f_typ);
            let f_constraints = f_bounds
                .iter()
                .map(|bound| {
                    self.ast_type_bound_to_generic_constraint_shallow(
                        session,
                        &mut prefex_scope,
                        bound,
                        &namespace,
                    )
                })
                .collect::<Vec<_>>();
            prefex_scope.pop_scope();
            let ty = self.create_type(session, f_typ);
            let instance = self.definitions.get_mut(id).unwrap();
            *instance = Definition {
                kind: DefinitionKind::Function,
                constraints: f_constraints,
                ty,
                file,
                implicit_imports: Vec::new(),
                ..instance.clone()
            };
        }
    }

    /// Translate ast type to ty::Type.
    ///
    /// Any type references are resolved to their type ids.
    /// Any builtin composite types are created fresh
    pub fn ast_type_to_type_shallow(
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
                } if PackageId(*package) == CURRENT_PACKAGE_ID => {
                    let def = LocalId::new(DefinitionId(*definition));
                    let typeid = self.type_id_from_definition(def).unwrap();
                    Type::Alias(typeid.as_global(CURRENT_PACKAGE_ID))
                }
                ast::Name::Global {
                    package,
                    definition,
                } => {
                    let package_info = self.get_package(PackageId(*package));
                    let LocalId(typeid) = package_info
                        .definition
                        .get(&LocalId::new(DefinitionId(*definition)))
                        .map(|def| def.ty)
                        .unwrap();
                    Type::Alias(Id {
                        package: PackageId(*package),
                        id: typeid,
                    })
                }

                ast::Name::Local(name) => {
                    let def = self.get_global(name).unwrap();
                    let typeid = self.type_id_from_definition(def).unwrap();
                    Type::Alias(typeid.as_global(CURRENT_PACKAGE_ID))
                }
                ast::Name::Unresolved { path, identifier }
                    if path.is_empty() && prefex_scope.contains(identifier) =>
                {
                    let (shift, position) = prefex_scope.get_with_position(identifier).unwrap();
                    Type::Generic(DeBruijn(shift.try_into().unwrap()), *position)
                }
                ast::Name::Unresolved { path, identifier } => {
                    println!("Resolving {:?} {:?} {:?}", namespace, path, identifier);
                    println!("Prefex scope is");
                    for (k, v) in prefex_scope.iter() {
                        println!("{:?} {:?}", k, v);
                    }
                    let id = self
                        .get_definition_id_by_unresolved_name(namespace, path, identifier)
                        .unwrap_or_else(|| {
                            panic!(
                                "expected definition to be found, {:?} {:?}",
                                path, identifier
                            )
                        });
                    match id {
                        Id { package, id } if package == CURRENT_PACKAGE_ID => {
                            let id = LocalId::new(id);
                            assert!(self.type_definitions.contains_key(&id));
                            let tid = self.type_id_from_definition(id).unwrap();
                            Type::Alias(tid.as_global(CURRENT_PACKAGE_ID))
                        }
                        Id { package, id } => {
                            let package_info = self.get_package(package);
                            let id = LocalId::new(id);
                            let tid = package_info.definition.get(&id).map(|def| def.ty).unwrap();
                            Type::Alias(tid.as_global(package))
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
                Type::Alias(tid.as_global(CURRENT_PACKAGE_ID))
            }
            ast::Typ::Array(inner) => {
                let ty = Type::Array(Box::new(self.ast_type_to_type_shallow(
                    session,
                    prefex_scope,
                    namespace,
                    inner,
                )));
                let tid = self.create_type(session, ty);
                Type::Alias(tid.as_global(CURRENT_PACKAGE_ID))
            }

            ast::Typ::Function { args, ret } => {
                let args = args
                    .iter()
                    .map(|t| self.ast_type_to_type_shallow(session, prefex_scope, namespace, t))
                    .collect();
                let ret =
                    Box::new(self.ast_type_to_type_shallow(session, prefex_scope, namespace, ret));
                let tid = self.create_type(session, Type::Function { args, ret });
                Type::Alias(tid.as_global(CURRENT_PACKAGE_ID))
            }
            ast::Typ::Tuple(inner) => {
                let inner = inner
                    .iter()
                    .map(|t| self.ast_type_to_type_shallow(session, prefex_scope, namespace, t))
                    .collect();
                let tid = self.create_type(session, Type::Tuple(inner));
                Type::Alias(tid.as_global(CURRENT_PACKAGE_ID))
            }
            ast::Typ::Poly {
                free_variables,
                bounds: _,
                typ,
            } if free_variables.is_empty() => {
                self.ast_type_to_type_shallow(session, prefex_scope, namespace, typ)
            }
            ast::Typ::Poly {
                free_variables,
                bounds: _bounds,
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
        ast::TypeBound { head: name, args }: &ast::TypeBound,
        current_namespace: &[String],
    ) -> GenericConstraint {
        let class: Id<DefinitionId> = match name {
            ast::Name::Unresolved { path, identifier } => {
                let id = self
                    .get_definition_id_by_unresolved_name(current_namespace, path, identifier)
                    .unwrap();
                match id {
                    Id { package, id } if package == CURRENT_PACKAGE_ID => {
                        let id = LocalId::new(id);
                        assert!(self.definitions.get(&id).map(Definition::is_class).unwrap());
                        id.as_global(CURRENT_PACKAGE_ID)
                    }
                    Id { package, id } => {
                        let package_info = self.get_package(package);
                        let id = LocalId::new(id);
                        assert!(package_info.classes.contains_key(&id));
                        id.as_global(package)
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
                assert!(self.definitions.get(id).map(Definition::is_class).unwrap());
                id.as_global(CURRENT_PACKAGE_ID)
            }
        };
        let args = args
            .iter()
            .map(|t| self.ast_type_to_type_shallow(session, prefex_scope, current_namespace, t))
            .collect();
        GenericConstraint { class, args }
    }
    /// Lookups type into the reverse hash to get its type id.
    /// If the type is not present in the hash it is added with a fresh type id.
    pub fn create_type(&mut self, session: &Session, ty: Type) -> LocalId<TypeId> {
        let lookup = self
            .reverse_type_aliases
            .borrow()
            .get(self, &ty)
            .unwrap()
            .cloned();
        match lookup {
            Some(tid) => tid,
            None => {
                let id = session.id_provider().next();
                let id = LocalId::new(TypeId(id));
                self.typeid_to_type.insert(id, ty.clone());
                self.insert_into_type_map(ty, id);
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
            .insert(self, ty, tid)
            .unwrap();
    }

    pub fn add_definition(
        &mut self,
        session: &Session,
        definition: Definition,
    ) -> LocalId<DefinitionId> {
        let id = LocalId::new(DefinitionId(session.id_provider().next()));
        self.definitions.insert(id, definition);
        id
    }

    pub fn classes(&self) -> impl Iterator<Item = (LocalId<DefinitionId>, &ClassInfo)> {
        self.definitions.iter().filter_map(|(id, def)| match def {
            Definition {
                kind: DefinitionKind::Class(c),
                ..
            } => Some((id.clone(), c)),
            _ => None,
        })
    }

    pub fn instances(&self) -> impl Iterator<Item = (LocalId<DefinitionId>, &InstanceInfo)> {
        self.definitions.iter().filter_map(|(id, def)| match def {
            Definition {
                kind: DefinitionKind::Instance(c),
                ..
            } => Some((id.clone(), c)),
            _ => None,
        })
    }

    pub fn unwrap_poly_type<'a>(
        &'a self,
        typ: &'a ast::Typ,
    ) -> Option<(&[String], &[ast::TypeBound], &ast::Typ)> {
        match typ {
            ast::Typ::Poly {
                free_variables,
                bounds,
                typ,
            } => Some((free_variables.as_slice(), bounds.as_slice(), typ)),
            _ => None,
        }
    }

    pub fn is_resolved_type(&self, t: &Type) -> bool {
        match t {
            Type::Fresh(_) | Type::ToInfere => false,
            // recursively check if there is Fresh type within the typ
            Type::Struct { .. } | Type::ADT { .. } => true,
            Type::Function { args, ret } => {
                args.iter().all(|t| self.is_resolved_type(t)) && self.is_resolved_type(ret)
            }
            Type::Tuple(args) => args.iter().all(|t| self.is_resolved_type(t)),
            Type::Array(inner) => self.is_resolved_type(inner),
            Type::Alias(tid) => self.is_resolved_type(&self.resolve_alias_to_type(*tid).unwrap()),
            Type::App { typ, args } => {
                self.is_resolved_type(typ) && args.iter().all(|t| self.is_resolved_type(t))
            }
            Type::Scheme { typ, .. } => self.is_resolved_type(typ),
            _ => true,
        }
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
    pub fn dump_all(&self) {
        self.dump_types();
        self.dump_classes();
        self.dump_definitions();
    }

    pub fn dump_types(&self) {
        println!("Types:\n");
        for (id, t) in &self.typeid_to_type {
            println!("{:?} => {}", id.0, t.pretty());
        }
    }

    pub fn dump_classes(&self) {
        println!("\nClasses:\n");
        for (id, t) in self.definitions.iter().filter_map(|(id, def)| match def {
            Definition {
                kind: DefinitionKind::Class(c),
                ..
            } => Some((id, c)),
            _ => None,
        }) {
            println!("{:?} => {:#?}", id.0, t);
        }
    }

    pub fn dump_definitions(&self) {
        println!("\nDefinitions:\n");
        for (id, t) in &self.definitions {
            println!("{:?} => {:#?}", id.0, t);
        }
    }
}
