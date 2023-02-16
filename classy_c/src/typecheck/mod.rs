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
    let id = ctx.add_type(Type::UInt);
    ctx.add_name("UInt", id);
    let id = ctx.add_type(Type::Int);
    ctx.add_name("Int", id);
    let id = ctx.add_type(Type::Bool);
    ctx.add_name("Bool", id);
    let id = ctx.add_type(Type::String);
    ctx.add_name("String", id);
    let id = ctx.add_type(Type::Float);
    ctx.add_name("Float", id);
}

pub fn resolve_type_names(mut ctx: TypCtx) -> TypCtx {
    let mut updates = HashMap::new();
    for (def_id, def) in &ctx.nodes {
        let name = &def.name;
        let exp_msg = format!("the types should have been prepopulated: {name}");
        let type_id = ctx.names.get(name).expect(&exp_msg);
        let resolved_type = match &def.definition {
            ast::DefinedType::Alias(ast::Alias { for_type: inner }) => {
                let resolved_id =
                    resolve_type(&ctx.names, &mut ctx.definitions, &mut ctx.next_id, inner);
                Type::Alias(resolved_id)
            }
            ast::DefinedType::Record(ast::Record { fields }) => {
                let mut resolved_fields = Vec::with_capacity(fields.len());
                for ast::TypedName { name, typ } in fields {
                    let resolved_id =
                        resolve_type(&ctx.names, &mut ctx.definitions, &mut ctx.next_id, typ);
                    resolved_fields.push((name.clone(), Type::Alias(resolved_id)));
                }
                Type::Struct {
                    def: *def_id,
                    fields: resolved_fields,
                }
            }
            // only adt left
            _ => unimplemented!(),
        };
        if let Some(t) = updates.insert(*type_id, resolved_type) {
            panic!(
                "Redefinition of type: {} => {}, previous value {:?}",
                *type_id, def.name, t
            );
        }
    }
    for (id, t) in updates {
        ctx.update_def(id, t)
    }
    ctx
}

fn resolve_type(
    names: &HashMap<String, TypeId>,
    definitions: &mut HashMap<TypeId, Type>,
    next_id: &mut TypeId,
    typ: &ast::Typ,
) -> TypeId {
    match typ {
        ast::Typ::Name(n) => names.get(n).expect(&format!("type not found, {n}")).clone(),
        ast::Typ::Tuple(types) => {
            let resolved = types
                .iter()
                .map(|t| resolve_type(names, definitions, next_id, t))
                .map(Type::Alias)
                .collect();
            let id = *next_id;
            *next_id += 1;
            definitions.insert(id, Type::Tuple(resolved));
            id
        }
        // todo: for other types like a function or an array, we actually
        // need to create them first.
        _ => unimplemented!(),
    }
}

pub fn resolve_aliases(ctx: &mut TypCtx) {
    let mut resolver = AliasResolver::new();
    resolver.resolve_aliases(ctx);
    resolver.update_names(ctx);
    resolver.resolve_aliases_shallow(ctx);
    resolver.remove_top_level_aliases(ctx);
}

// TODO, based on type equality remove
// redundant aliases, for example
// 0: Int
// 1: Int
// 2: struct { a: 1 }
// The field in 2 can point to 0, then we can compare types
// using just the id for equality. The one should be removed
// and all references pointing to 1 should point to 0 now.
struct AliasResolver {
    resolved: HashMap<TypeId, TypeId>,
}

impl AliasResolver {
    fn new() -> Self {
        Self {
            resolved: HashMap::new(),
        }
    }

    fn resolve_aliases(&mut self, ctx: &mut TypCtx) {
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
        for (type_id, for_type) in &self.resolved {
            println!("Updating definition for {type_id} => {for_type}");
            let t = match ctx.definitions.get(&for_type).unwrap() {
                Type::Struct { .. }
                | Type::ADT { .. }
                | Type::Tuple { .. }
                | Type::Array(..)
                | Type::Function { .. }
                | Type::Bool
                | Type::Float
                | Type::Int
                | Type::String
                | Type::UInt => Type::Alias(*for_type),
                Type::Alias(..) => {
                    unreachable!("no alias should point to another alias at this point")
                }
            };
            ctx.update_def(*type_id, t)
        }
    }

    fn update_names(&mut self, ctx: &mut TypCtx) {
        let mut updates = Vec::new();
        for (name, tid) in &ctx.names {
            if let Type::Alias(for_type) = ctx.definitions.get(tid).unwrap() {
                updates.push((name.clone(), *for_type));
            }
        }
        for (name, tid) in updates {
            ctx.names.insert(name, tid);
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

    /// Resolves struct field aliases if possible, meaning subsituting
    /// them will not result in a reference cycle.
    /// The substitution is shallow so it only resolves aliases 2 levels deep.
    /// This method should be called after top level aliases have already
    /// been resolved.
    fn resolve_aliases_shallow(&mut self, ctx: &mut TypCtx) {
        let mut updated_defs = HashMap::new();
        for (typ_id, typ) in &ctx.definitions {
            if let Type::Alias(_) = typ {
                // skip top level aliases, keep them to resolve alias chains,
                // we will remove them later.
                continue;
            }
            updated_defs.insert(*typ_id, self.resolve_shallow_aliases_in_type(ctx, typ));
        }
        for (id, typ) in updated_defs {
            ctx.update_def(id, typ)
        }
    }

    fn resolve_shallow_aliases_in_type(&self, ctx: &TypCtx, typ: &Type) -> Type {
        match typ {
            Type::Struct {
                def: def_id,
                fields,
            } => {
                let resolved_fields = fields
                    .iter()
                    .map(|(fname, ftyp)| {
                        assert!(if let Type::Alias(_) = ftyp {
                            true
                        } else {
                            false
                        });
                        let resolved_type = self.resolve_shallow_aliases_in_type(ctx, ftyp);
                        (fname.clone(), resolved_type)
                    })
                    .collect();
                Type::Struct {
                    def: *def_id,
                    fields: resolved_fields,
                }
            }
            Type::Alias(for_type) => {
                let typ = ctx.definitions.get(for_type).unwrap();
                match typ {
                    // We will get at most 2 long chain in case of
                    // struct field => top level alias => specific type
                    // so we strip the outer alias layer and resolve again
                    // this type we are sure to hit some other type after
                    // resolving this alias
                    Type::Alias(follow) => {
                        self.resolve_shallow_aliases_in_type(ctx, &Type::Alias(*follow))
                    }
                    t @ (Type::UInt | Type::Int | Type::Bool | Type::Float | Type::String) => {
                        t.clone()
                    }
                    // Do not resolve this types as they migh create reference cycles.
                    Type::Struct { .. } => Type::Alias(*for_type),
                    Type::Function { .. } => Type::Alias(*for_type),
                    Type::Tuple { .. } => Type::Alias(*for_type),
                    Type::ADT { .. } => Type::Alias(*for_type),
                    Type::Array { .. } => Type::Alias(*for_type),
                }
            }
            Type::Tuple(fields) => {
                let resolved_fields = fields
                    .iter()
                    .map(|f| self.resolve_shallow_aliases_in_type(ctx, f))
                    .collect();
                Type::Tuple(resolved_fields)
            }
            t @ (Type::UInt | Type::Int | Type::Bool | Type::Float | Type::String) => t.clone(),
            _ => unimplemented!(),
        }
    }

    fn remove_top_level_aliases(&mut self, ctx: &mut TypCtx) {
        ctx.definitions
            .retain(|_, v| if let Type::Alias(_) = v { false } else { true })
    }
}

fn types_eq(ctx: &TypCtx, t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::Alias(f1), Type::Alias(f2)) if f1 == f2 => {
            return true;
        }
        _ => {}
    }
    let t1 = if let Type::Alias(for_type) = t1 {
        ctx.definitions.get(for_type).unwrap()
    } else {
        t1
    };
    let t2 = if let Type::Alias(for_type) = t2 {
        ctx.definitions.get(for_type).unwrap()
    } else {
        t2
    };
    match (t1, t2) {
        (Type::Int, Type::Int) => true,
        (Type::UInt, Type::UInt) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::String, Type::String) => true,
        (Type::Float, Type::Float) => true,
        (Type::Struct { def: def1, .. }, Type::Struct { def: def2, .. }) => def1 == def2,
        (Type::Tuple(fields1), Type::Tuple(fields2)) => {
            if fields1.len() != fields2.len() {
                return false;
            }
            for (t1, t2) in fields1.iter().zip(fields2) {
                if !types_eq(ctx, t1, t2) {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

pub fn dedup_trivially_eq_types(ctx: &mut TypCtx) {
    let mut duplicates = HashMap::new();
    for (t_id_1, typ_1) in &ctx.definitions {
        for (t_id_2, typ_2) in &ctx.definitions {
            if t_id_1 == t_id_2 {
                continue;
            }
            if !types_eq(ctx, typ_1, typ_2) {
                continue;
            }
            // different ids pointing to the same type
            // we need to chose one as the one we will retain
            // we chose the smaller one.
            let type_to_delete = std::cmp::max(t_id_1, t_id_2);
            let type_to_retain = std::cmp::min(t_id_1, t_id_2);
            match duplicates.get(type_to_delete) {
                None => {
                    duplicates.insert(*type_to_delete, *type_to_retain);
                }
                Some(old_type) if old_type == type_to_retain => {}
                Some(old_type) if old_type < type_to_retain => {
                    duplicates.insert(*type_to_retain, *old_type);
                }
                Some(old_type) => {
                    duplicates.insert(*old_type, *type_to_retain);
                    duplicates.insert(*type_to_delete, *type_to_retain);
                }
            }
        }
    }
    for (id, typ_id) in &duplicates {
        println!("this type can be replaced with {id} => {typ_id}")
    }
    ctx.definitions.retain(|k, _| !duplicates.contains_key(k));
    for (_, typ) in ctx.definitions.iter_mut() {
        *typ = replace_aliases_with_map(typ, &duplicates)
    }
}

fn replace_aliases_with_map(typ: &Type, map: &HashMap<TypeId, TypeId>) -> Type {
    match typ {
        t @ (Type::UInt | Type::Int | Type::Bool | Type::Float | Type::String) => t.clone(),
        Type::Struct { def, fields } => Type::Struct {
            def: *def,
            fields: fields
                .iter()
                .map(|(n, t)| (n.clone(), replace_aliases_with_map(t, map)))
                .collect(),
        },
        Type::Tuple(fields) => Type::Tuple(
            fields
                .iter()
                .map(|t| replace_aliases_with_map(t, map))
                .collect(),
        ),
        Type::Alias(for_type) => match map.get(for_type) {
            None => Type::Alias(*for_type),
            Some(t) => Type::Alias(*t),
        },
        _ => unimplemented!(),
    }
}