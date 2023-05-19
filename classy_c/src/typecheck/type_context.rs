use std::collections::HashMap;

use crate::{syntax::ast, typecheck::r#type::Type};

pub type TypeId = usize;
pub type DefId = usize;
pub type Name = String;

pub struct TypCtx {
    /// Associates type ids with their types. Useful for resolving aliases.
    pub definitions: HashMap<TypeId, Type>,
    /// Associates a type name with its type id.
    pub types: HashMap<Name, TypeId>,
    /// Associates a definition id with its original ast type definition node.
    pub nodes: HashMap<DefId, ast::TopLevelItem>,
    /// Associates names of items, that are not types, like variables or
    /// functions with their type.
    pub variables: HashMap<Name, TypeId>,

    pub next_id: TypeId,

    pub unit_id: TypeId,
}

impl TypCtx {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            unit_id: 0,
            definitions: HashMap::new(),
            types: HashMap::new(),
            nodes: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

impl TypCtx {
    pub fn mk_tuple(&mut self, of: &[Type]) -> TypeId {
        let typ = Type::Tuple(of.into());
        self.add_type(typ)
    }

    pub fn mk_function(&mut self, args: &[Type], ret: Type) -> TypeId {
        let typ = Type::Function {
            args: args.into(),
            ret: Box::new(ret),
        };
        self.add_type(typ)
    }

    pub fn mk_struct(&mut self, def_id: DefId, fields: &[(Name, Type)]) -> TypeId {
        assert!(self.nodes.contains_key(&def_id));
        let typ = Type::Struct {
            def: def_id,
            fields: fields.into(),
        };
        self.add_type(typ)
    }

    pub fn insert_struct(&mut self, at: TypeId, def_id: DefId, fields: &[(Name, Type)]) {
        assert!(self.nodes.contains_key(&def_id));
        let typ = Type::Struct {
            def: def_id,
            fields: fields.into(),
        };
        self.add_type_definition(at, typ)
    }

    pub fn update_type_def(&mut self, at: TypeId, typ: Type) {
        self.add_type_definition(at, typ)
    }

    pub fn add_type(&mut self, typ: Type) -> TypeId {
        let id = self.next_id();
        self.add_type_definition(id, typ);
        id
    }

    /// Get a type of a variable with the given name.
    pub fn type_of(&self, name: &str) -> Option<Type> {
        let id = self.variables.get(name)?;
        self.definitions.get(id).cloned()
    }

    /// Resolve the name of the type to its Type representation.
    pub fn get_type(&self, name: &str) -> Option<Type> {
        let id = self.types.get(name)?;
        self.definitions.get(id).cloned()
    }

    pub fn add_variable(&mut self, name: impl Into<String>, typ: TypeId) {
        let name = name.into();
        assert!(
            !self.variables.contains_key(&name),
            "double definition of variable: {}",
            name
        );
        self.variables.insert(name, typ);
    }

    pub fn update_variable(&mut self, name: impl Into<String>, typ: TypeId) {
        let name = name.into();
        assert!(
            self.variables.contains_key(&name),
            "variable not defined: {}",
            name
        );
        self.variables.insert(name, typ);
    }

    /// Associates the type with the given id. So that when asking
    /// about the id it will resolve to the given type.
    fn add_type_definition(&mut self, id: TypeId, typ: Type) {
        self.definitions.insert(id, typ);
    }

    fn next_id(&mut self) -> TypeId {
        let res = self.next_id;
        self.next_id += 1;
        res
    }

    /// Creates an id and associates it with the given type definition node.
    /// Used for structs.
    ///
    /// Caller has to ensure that no duplicate nodes are pushed as
    /// no identity checking is done to prevent duplicates.
    pub fn add_type_node(&mut self, node: &ast::TypeDefinition) -> DefId {
        let id = self.next_id();
        self.nodes
            .insert(id, ast::TopLevelItem::TypeDefinition(node.clone()));
        id
    }

    pub fn add_fn_node(&mut self, node: &ast::FunctionDefinition) -> DefId {
        let id = self.next_id();
        self.nodes
            .insert(id, ast::TopLevelItem::FunctionDefinition(node.clone()));
        id
    }

    /// Associates given type name with the given type id.
    pub fn add_type_name(&mut self, name: impl Into<Name>, id: TypeId) -> Option<TypeId> {
        let name = name.into();
        assert!(
            !self.types.contains_key(&name),
            "double definition of type: {}",
            name
        );
        self.types.insert(name, id)
    }

    pub fn reserve_id(&mut self) -> TypeId {
        self.next_id()
    }

    pub fn debug_string(&self) -> String {
        let mut s = "TypCtx {\n\tdefinitions:".to_owned();
        let mut tmp = Vec::new();
        for (id, def) in &self.definitions {
            tmp.push((id, def));
        }
        tmp.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
        for (id, def) in tmp {
            s = s + &format!("\n\t\t{id}: {def:?}");
        }

        s = s + "\n\tvariables:";

        let mut tmp = Vec::new();
        for (name, type_id) in &self.variables {
            tmp.push((name, type_id));
        }
        tmp.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
        for (name, type_id) in tmp {
            let resolved_type = match self.definitions.get(type_id) {
                Some(t) => format!("{:?}", t),
                None => format!("unknown type: {}", type_id),
            };
            s = s + &format!("\n\t\t{name}: {type_id} => {resolved_type}");
        }

        s = s + "\n\tnames:";

        let mut tmp = Vec::new();
        for (id, def) in &self.types {
            tmp.push((def, id));
        }
        tmp.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
        for (id, name) in tmp {
            s = s + &format!("\n\t\t{id}: {name}");
        }
        s = s + "\n\tnodes:";

        let mut tmp = Vec::new();
        for (id, def) in &self.nodes {
            tmp.push((id, def));
        }
        tmp.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
        for (id, type_def) in tmp {
            s = s + &format!("\n\t\t{id}: {type_def:?}");
        }
        s
    }
}
