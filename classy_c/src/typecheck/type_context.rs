use std::collections::HashMap;

use crate::{syntax::ast, typecheck::r#type::Type};

pub type TypeId = usize;
pub type DefId = usize;
pub type Name = String;

pub struct TypCtx {
    pub definitions: HashMap<TypeId, Type>,
    pub names: HashMap<Name, TypeId>,
    pub nodes: HashMap<DefId, ast::TypeDefinition>,

    pub next_id: TypeId,
}

impl TypCtx {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            definitions: HashMap::new(),
            names: HashMap::new(),
            nodes: HashMap::new(),
        }
    }

    pub fn mk_tuple(&mut self, of: &[Type]) -> TypeId {
        let typ = Type::Tuple(of.into());
        self.add_type(typ)
    }

    // pub fn mk_array(&mut self, of: TypeId) -> TypeId {
    //     assert!(self.definitions.contains_key(&of));
    //     let typ = Type::Array(of);
    //     self.add_type(typ)
    // }

    // pub fn mk_function(&mut self, args: &[TypeId], ret: TypeId) -> TypeId {
    //     assert!(self.definitions.contains_key(&ret));
    //     for id in args {
    //         assert!(self.definitions.contains_key(id));
    //     }
    //     let typ = Type::Function {
    //         args: args.into(),
    //         ret,
    //     };
    //     self.add_type(typ)
    // }

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
        self.add_definition(at, typ)
    }

    pub fn update_def(&mut self, at: TypeId, typ: Type) {
        self.add_definition(at, typ)
    }

    pub fn add_type(&mut self, typ: Type) -> TypeId {
        let id = self.next_id();
        self.add_definition(id, typ);
        id
    }

    fn add_definition(&mut self, id: TypeId, typ: Type) {
        self.definitions.insert(id, typ);
    }

    fn next_id(&mut self) -> TypeId {
        let res = self.next_id;
        self.next_id += 1;
        res
    }

    /// caller has to ensure that no duplicate nodes are pushed as
    /// no identity checking is done to prevent duplicates.
    pub fn add_node(&mut self, node: &ast::TypeDefinition) -> DefId {
        let id = self.next_id();
        self.nodes.insert(id, node.clone());
        id
    }

    pub fn add_name(&mut self, name: impl Into<Name>, id: TypeId) -> Option<TypeId> {
        self.names.insert(name.into(), id)
    }

    pub fn reserve_type_id(&mut self) -> TypeId {
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
        s = s + "\n\tnames:";

        let mut tmp = Vec::new();
        for (id, def) in &self.names {
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
