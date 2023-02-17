pub mod r#type;
pub mod type_context;

use std::collections::{HashMap, HashSet};

use crate::syntax::ast;

use r#type::*;
use type_context::*;

/// How to extend
/// 1. Add type to AddTypes - resolve type
/// 2. Then add type to resolve aliases shallow for type
/// 3. Then add for replace aliases with map
/// 4. Also add a type for equality
/// Actually resolve aliases and replace aliases with map should be
/// a special case of fold but well.

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
        ast::Typ::Function { args, ret } => {
            let resolved_args = args
                .iter()
                .map(|t| resolve_type(names, definitions, next_id, t))
                .map(Type::Alias)
                .collect();
            let resolved_ret = Type::Alias(resolve_type(names, definitions, next_id, ret));
            let id = *next_id;
            *next_id += 1;
            definitions.insert(
                id,
                Type::Function {
                    args: resolved_args,
                    ret: Box::new(resolved_ret),
                },
            );
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
            Type::Function { args, ret } => {
                let resolved_args = args
                    .iter()
                    .map(|t| self.resolve_shallow_aliases_in_type(ctx, t))
                    .collect();
                let resolved_ret = self.resolve_shallow_aliases_in_type(ctx, ret);
                Type::Function {
                    args: resolved_args,
                    ret: Box::new(resolved_ret),
                }
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
            fields1
                .iter()
                .zip(fields2)
                .all(|(t1, t2)| types_eq(ctx, t1, t2))
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
                return false;
            }
            types_eq(ctx, ret1, ret2)
                && args1
                    .iter()
                    .zip(args2)
                    .all(|(t1, t2)| types_eq(ctx, t1, t2))
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
    for (_, id) in &mut ctx.names {
        if let Some(new_id) = duplicates.get(id) {
            *id = *new_id;
        }
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
        Type::Function { args, ret } => {
            let args = args
                .iter()
                .map(|t| replace_aliases_with_map(t, map))
                .collect();
            let ret = replace_aliases_with_map(ret, map);
            Type::Function {
                args,
                ret: Box::new(ret),
            }
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        syntax::{ast::Visitor, lexer::Lexer, parser::Parser},
        typecheck::{
            self,
            r#type::Type,
            type_context::{TypCtx, TypeId},
        },
    };

    macro_rules! map {
        { $($key:literal => $val:expr),* $(,)? } => {
            {
                let mut m = HashMap::new();
                $(m.insert($key, $val));*;
                m
            }
        };
    }

    macro_rules! type_test {
        ($name:ident, $source:literal, $expected:expr) => {
            #[test]
            fn $name() {
                #[allow(unused_imports)]
                use Type::*;
                run_test($source, $expected)
            }
        };
    }

    macro_rules! tuple {
        ($($args:expr),*) => {{
            Type::Tuple(vec![$($args),*])
        }};
    }

    macro_rules! function {
        (($($args:expr),*) -> $ret:expr ) => {
            {
                Type::Function { args: vec![$($args),*], ret: Box::new($ret) }
            }
        };
    }

    macro_rules! struct_t {
        ($($field:ident: $t:expr),* $(,)?) => {
            Type::Struct {
                def: 0,
                fields: vec![
                    $(
                        (
                            (stringify!($field)).into(),
                            $t
                        )
                    ),*
                ]
            }
        };
    }

    fn test_types_eq(ctx: &TypCtx, t1: &Type, t2: &Type) -> bool {
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
            (
                Type::Struct {
                    fields: fields1,
                    ..
                },
                Type::Struct {
                    fields: fields2,
                    ..
                },
            ) => {
                fields1.len() == fields2.len() &&
                    fields1.iter().zip(fields2).all(
                        |((n1, t1), (n2, t2))| { n1 == n2 && test_types_eq(ctx, t1, t2) })
            }
            (Type::Tuple(fields1), Type::Tuple(fields2)) => {
                if fields1.len() != fields2.len() {
                    return false;
                }
                fields1
                    .iter()
                    .zip(fields2)
                    .all(|(t1, t2)| test_types_eq(ctx, t1, t2))
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
                    return false;
                }
                test_types_eq(ctx, ret1, ret2)
                    && args1
                        .iter()
                        .zip(args2)
                        .all(|(t1, t2)| test_types_eq(ctx, t1, t2))
            }
            _ => false,
        }
    }

    fn run_test(source: &str, mut expected: HashMap<TypeId, Type>) {
        let lex = Lexer::new(source);
        let mut parser = Parser::new(lex);
        let res = parser.parse().unwrap();
        assert!(parser.errors().is_empty(), "errors while parsing");
        let mut tctx = TypCtx::new();
        let int_id = tctx.add_type(Type::Int);
        tctx.add_name("Int", int_id);
        let mut add_types = typecheck::AddTypes::new(&mut tctx);
        add_types.visit(&res);
        tctx = typecheck::resolve_type_names(tctx);
        println!("{}", tctx.debug_string());
        typecheck::resolve_aliases(&mut tctx);
        println!("{}", tctx.debug_string());
        typecheck::dedup_trivially_eq_types(&mut tctx);

        expected.insert(1000000, Type::Int);
        if tctx.definitions.len() != expected.len() {
            similar_asserts::assert_eq!(expected, tctx.definitions)
        }

        tctx.definitions
            .extend(expected.iter().map(|(k, v)| (k.clone(), v.clone())));
        for (id, t) in expected {
            assert!(
                tctx.definitions
                    .iter()
                    .any(|(&k, v)| { k != id && test_types_eq(&tctx, &t, v) }),
                "no type equal to {:?} found ",
                t
            );
        }
    }

    type_test! {
        test_function_deduplication,
        "type A = (Int) -> Int; type B = (Int) -> Int;",
        map! {
            105 => function!((Int) -> Int),
        }
    }

    type_test! {
        test_tuple_deduplication,
        r#"
        type A1 = (Int, (Int, (Int, Int)))
        type B1 = (Int, (Int, Int))
        type C1 = (Int, Int)

        type A2 = (Int, (Int, (Int, Int)))
        type B2 = (Int, (Int, Int))
        type C2 = (Int, Int)
        "#,
        map! {
            117 => tuple!(Int, Alias(114)),
            114 => tuple!(Int, Alias(113)),
            113 => tuple!(Int, Int),
        }
    }

    type_test! {
        resolving_indirect_alias_in_a_structs_field,
        r#"
        type A { a: B }
        type B = C
        type C = Int
        "#,
        map! {
            101 => struct_t!{ a: Int },
        }
    }

    type_test!{
        resolving_indirect_aliases_in_types_of_structs_field,
        r#"
        type A { a: (B) -> Int } 
        type B = (C, C)
        type C = Int
        "#,
        map! {
            101 => struct_t!{ a: Alias(102) },
            102 => function!((Alias(103)) -> Int),
            103 => tuple!(Int, Int)
        }
    }
}
