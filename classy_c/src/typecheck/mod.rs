use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::syntax::ast;

type TypeId = usize;
type DefId = usize;
type Name = String;

pub struct TypCtx {
    pub definitions: HashMap<TypeId, Type>,
    //pub reverse_definitions: HashMap<Type, TypeId>,
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

    pub fn mk_tuple(&mut self, of: &[TypeId]) -> TypeId {
        for id in of {
            assert!(self.definitions.contains_key(id));
        }
        let typ = Type::Tuple(of.into());
        self.add_type(typ)
    }

    pub fn mk_array(&mut self, of: TypeId) -> TypeId {
        assert!(self.definitions.contains_key(&of));
        let typ = Type::Array(of);
        self.add_type(typ)
    }

    pub fn mk_function(&mut self, args: &[TypeId], ret: TypeId) -> TypeId {
        assert!(self.definitions.contains_key(&ret));
        for id in args {
            assert!(self.definitions.contains_key(id));
        }
        let typ = Type::Function {
            args: args.into(),
            ret,
        };
        self.add_type(typ)
    }

    pub fn mk_struct(&mut self, def_id: DefId, fields: &[(Name, TypeId)]) -> TypeId {
        assert!(self.nodes.contains_key(&def_id));
        for (_, t) in fields {
            assert!(self.definitions.contains_key(t));
        }
        let typ = Type::Struct {
            def: def_id,
            fields: fields.into(),
        };
        self.add_type(typ)
    }

    pub fn insert_struct(&mut self, at: TypeId, def_id: DefId, fields: &[(Name, TypeId)]) {
        assert!(self.nodes.contains_key(&def_id));
        for (_, t) in fields {
            assert!(self.definitions.contains_key(t));
        }
        let typ = Type::Struct {
            def: def_id,
            fields: fields.into(),
        };
        self.add_definition(at, typ)
    }

    pub fn update_def(&mut self, at: TypeId, typ: Type) {
        self.add_definition(at, typ)
    }

    fn add_type(&mut self, typ: Type) -> TypeId {
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

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Type {
    Int,
    UInt,
    Bool,
    String,
    Float,
    Struct {
        def: TypeId,
        // maps fields to the TypeId of the type
        fields: Vec<(Name, TypeId)>,
    },
    ADT {
        def: TypeId,
        // maps constructors to the TypeId of the type
        // can be a tuple type
        constructors: Vec<(Name, TypeId)>,
    },
    Function {
        args: Vec<TypeId>,
        ret: TypeId,
    },
    Tuple(Vec<TypeId>),
    Array(TypeId),
    Alias(TypeId),
}

pub struct AddTypes<'ctx> {
    ctx: &'ctx mut TypCtx,
}

impl<'ctx> AddTypes<'ctx> {
    pub fn new(ctx: &'ctx mut TypCtx) -> Self {
        Self { ctx }
    }
}

impl<'ast, 'ctx> ast::Visitor<'ast> for AddTypes<'ctx> {
    fn visit_fn_def(&mut self, _node: &'ast ast::FunctionDefinition) {}
    fn visit_type_def(&mut self, node: &'ast ast::TypeDefinition) {
        println!("Adding node for: {node:?}");
        let _id = self.ctx.add_node(node);
        let type_id = self.ctx.reserve_type_id();
        self.ctx.add_name(node.name.clone(), type_id);
    }
}

pub fn insert_primitive_types(ctx: &mut TypCtx) {
    let uint_t = Type::UInt;
    let int_t = Type::Int;
    let bool_t = Type::Bool;
    let string_t = Type::String;
    let float_t = Type::Float;
    let id = ctx.add_type(uint_t);
    ctx.add_name("Int", id);
    let id = ctx.add_type(int_t);
    ctx.add_name("UInt", id);
    let id = ctx.add_type(bool_t);
    ctx.add_name("Bool", id);
    let id = ctx.add_type(string_t);
    ctx.add_name("String", id);
    let id = ctx.add_type(float_t);
    ctx.add_name("Float", id);
}

pub fn resolve_type_names(ctx: &mut TypCtx) {
    let mut updates = HashMap::new();
    for (_, def) in &ctx.nodes {
        let name = &def.name;
        let exp_msg = format!("the types should have been prepopulated: {name}");
        let type_id = ctx.names.get(name).expect(&exp_msg);
        match &def.definition {
            ast::DefinedType::Alias(ast::Alias { for_type: inner }) => {
                let resolved_id = resolve_type(ctx, inner);
                assert!(updates.insert(*type_id, Type::Alias(resolved_id)).is_none());
            }
            _ => unimplemented!(),
        }
    }
    for (id, t) in updates {
        ctx.update_def(id, t)
    }
}

fn resolve_type(ctx: &TypCtx, typ: &ast::Typ) -> TypeId {
    match typ {
        ast::Typ::Name(n) => ctx
            .names
            .get(n)
            .expect(&format!("type not found, {n}"))
            .clone(),
        _ => unimplemented!(),
    }
}

pub fn resolve_aliases(ctx: &mut TypCtx) {
    let resolver = AliasResolver::new();
    resolver.resolve_aliases(ctx);
}

struct AliasResolver {
    resolved: HashMap<TypeId, TypeId>,
}

impl AliasResolver {
    fn new() -> Self {
        Self {
            resolved: HashMap::new(),
        }
    }

    fn resolve_aliases(mut self, ctx: &mut TypCtx) {
        for (typ_id, typ) in &ctx.definitions {
            let mut resolved_this_path = HashSet::new();
            match typ {
                Type::Alias(for_type) => {
                    let new_id = self.resolve_deep(ctx, &mut resolved_this_path, *for_type);
                    self.resolved.insert(*typ_id, new_id);
                    for resolved_type in resolved_this_path {
                        self.resolved.insert(resolved_type, new_id);
                    }
                }
                _ => {}
            }
        }
        for (type_id, for_type) in self.resolved {
            println!("Updating definition for {type_id} => {for_type}");
            let t = match ctx.definitions.get(&for_type).unwrap() {
                Type::Struct { .. }
                | Type::ADT { .. }
                | Type::Tuple { .. }
                | Type::Array(..)
                | Type::Function { .. } => Type::Alias(for_type),
                t @ (Type::Bool | Type::Float | Type::Int | Type::String | Type::UInt) => t.clone(),
                Type::Alias(..) => {
                    unreachable!("no alias should point to another alias at this point")
                }
            };
            ctx.update_def(type_id, t)
        }
    }

    fn resolve_deep(
        &mut self,
        ctx: &TypCtx,
        resolved_this_path: &mut HashSet<TypeId>,
        follow_type: TypeId,
    ) -> TypeId {
        if resolved_this_path.contains(&follow_type) {
            panic!("type alias loop")
        }
        resolved_this_path.insert(follow_type);
        if let Some(resolved_id) = self.resolved.get(&follow_type) {
            return *resolved_id;
        };
        let t = ctx.definitions.get(&follow_type).unwrap();
        match t {
            Type::Alias(for_t) => self.resolve_deep(ctx, resolved_this_path, *for_t),
            _ => {
                resolved_this_path.remove(&follow_type);
                follow_type
            }
        }
    }
}
