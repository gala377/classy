pub mod add_types;
pub mod alias_resolver;
pub mod constraints;
pub mod constrait_solver;
pub mod fix_fresh;
pub mod inference;
pub mod scope;
pub mod r#type;
pub mod type_context;

use std::collections::HashMap;

use crate::syntax::{
    self,
    ast::{self, Visitor},
};

use add_types::AddTypes;
use alias_resolver::AliasResolver;
use r#type::*;
use type_context::*;

pub use inference::run;

/// Create a top level type context containing all types and functions with their
/// respective types in the simplest form possible.
pub fn prepare_for_typechecking(program: &syntax::ast::Program) -> TypCtx {
    let mut tctx = TypCtx::new();
    let mut add_types = AddTypes::with_primitive_types(&mut tctx);
    add_types.visit(program);
    tctx = resolve_type_names(tctx);
    AliasResolver::resolve(&mut tctx);
    dedup_trivially_eq_types(&mut tctx);
    tctx
}

/// Actually creates the types for all ast node in the type context.
/// Populates type ids created earlier with actual type definitions.
///
/// How to extend
/// 1. Add type to AddTypes - resolve type
/// 2. Then add type to resolve aliases shallow for type
/// 3. Then add for replace aliases with map
/// 4. Also add a type for equality
/// Actually resolve aliases and replace aliases with map should be
/// a special case of fold but well.
pub fn resolve_type_names(mut ctx: TypCtx) -> TypCtx {
    let mut type_updates = HashMap::new();
    let nodes = ctx.nodes.clone();
    for (def_id, def) in &nodes {
        match def {
            ast::TopLevelItem::TypeDefinition(ast::TypeDefinition {
                name, definition, ..
            }) => {
                resolve_top_level_type(&mut ctx, name, definition, def_id, &mut type_updates);
            }
            ast::TopLevelItem::FunctionDefinition(ast::FunctionDefinition {
                name, typ, ..
            }) => resolve_fn_def(typ, &mut ctx, name, *def_id),
        }
    }
    for (id, t) in type_updates {
        ctx.update_type_def(id, t)
    }
    ctx
}

fn resolve_fn_def(typ: &ast::Typ, ctx: &mut TypCtx, name: &String, def_id: usize) {
    match typ {
        ast::Typ::Function {
            args,
            ret,
            generics,
        } => {
            let resolved_args: Vec<_> = args
                .iter()
                .map(|t| {
                    resolve_type(
                        &ctx.types,
                        &mut ctx.definitions,
                        &mut ctx.next_id,
                        ctx.unit_id,
                        ctx.to_infere_id,
                        generics,
                        def_id,
                        t,
                    )
                })
                .collect();
            let resolved_ret = resolve_type(
                &ctx.types,
                &mut ctx.definitions,
                &mut ctx.next_id,
                ctx.unit_id,
                ctx.to_infere_id,
                generics,
                def_id,
                ret,
            );
            let function_t = ctx.mk_function_scheme(generics.clone(), &resolved_args, resolved_ret);
            assert!(
                ctx.variables.insert(name.clone(), function_t).is_some(),
                "updating type definition for funtion that does not exist {}",
                name
            );
        }
        ast::Typ::Name(_) => todo!("Name aliases for function types not yet supported"),
        ast::Typ::Application { .. } => todo!("Generic types for function types not yet supported"),
        _ => panic!("invalid type for function definition {} => {:?}", name, typ),
    }
}

fn resolve_top_level_type(
    ctx: &mut TypCtx,
    name: &String,
    definition: &ast::DefinedType,
    def_id: &usize,
    updates: &mut HashMap<usize, Type>,
) {
    let exp_msg = format!("the types should have been prepopulated: {name}");
    let type_id = ctx.types.get(name).expect(&exp_msg);
    let resolved_type = match definition {
        ast::DefinedType::Alias(ast::Alias { for_type: inner }) => {
            let resolved = resolve_type(
                &ctx.types,
                &mut ctx.definitions,
                &mut ctx.next_id,
                ctx.unit_id,
                ctx.to_infere_id,
                &Vec::new(),
                *def_id,
                inner,
            );
            // invariant: because the prefex is empty this will always be an alias
            resolved
        }
        ast::DefinedType::Record(ast::Record { fields }) => {
            let mut resolved_fields = Vec::with_capacity(fields.len());
            // invariant: because the prefex is empty this will always be an alias
            for ast::TypedName { name, typ } in fields {
                let resolved = resolve_type(
                    &ctx.types,
                    &mut ctx.definitions,
                    &mut ctx.next_id,
                    ctx.unit_id,
                    ctx.to_infere_id,
                    &Vec::new(),
                    *def_id,
                    typ,
                );
                resolved_fields.push((name.clone(), resolved));
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
            *type_id, name, t
        );
    }
}

fn resolve_type(
    names: &HashMap<String, TypeId>,
    definitions: &mut HashMap<TypeId, Type>,
    next_id: &mut TypeId,
    unit_id: TypeId,
    to_infere_id: TypeId,
    prefex: &Vec<String>,
    curr_def_id: usize,
    typ: &ast::Typ,
) -> Type {
    match typ {
        ast::Typ::Name(n) if prefex.contains(n) => {
            let pos = prefex.iter().position(|x| x == n).unwrap();
            Type::Generic(curr_def_id, pos)
        }
        ast::Typ::Name(n) => {
            Type::Alias(names.get(n).expect(&format!("type not found, {n}")).clone())
        }
        ast::Typ::Tuple(types) => {
            let resolved = types
                .iter()
                .map(|t| {
                    resolve_type(
                        names,
                        definitions,
                        next_id,
                        unit_id,
                        to_infere_id,
                        prefex,
                        curr_def_id,
                        t,
                    )
                })
                .collect();
            let id = *next_id;
            *next_id += 1;
            definitions.insert(id, Type::Tuple(resolved));
            Type::Alias(id)
        }
        ast::Typ::Function {
            args,
            ret,
            generics: _generics,
        } => {
            // TODO:
            // We would need to change it if we want higher order functions
            // We need to have new generic indexes or like, new identification\
            // number for the generic if we want those types to work
            // As we need to have new generics here
            let resolved_args = args
                .iter()
                .map(|t| {
                    resolve_type(
                        names,
                        definitions,
                        next_id,
                        unit_id,
                        to_infere_id,
                        prefex,
                        curr_def_id,
                        t,
                    )
                })
                .collect();
            let resolved_ret = resolve_type(
                names,
                definitions,
                next_id,
                unit_id,
                to_infere_id,
                prefex,
                curr_def_id,
                ret,
            );
            let id = *next_id;
            *next_id += 1;
            definitions.insert(
                id,
                Type::Function {
                    args: resolved_args,
                    ret: Box::new(resolved_ret),
                },
            );
            Type::Alias(id)
        }
        ast::Typ::Unit => Type::Alias(unit_id),
        ast::Typ::ToInfere => Type::Alias(to_infere_id),
        // todo: for other types like a function or an array, we actually
        // need to create them first.
        ast::Typ::Array(inner) => {
            let resolved = resolve_type(
                names,
                definitions,
                next_id,
                unit_id,
                to_infere_id,
                prefex,
                curr_def_id,
                inner,
            );
            let id = *next_id;
            *next_id += 1;
            definitions.insert(id, Type::Array(Box::new(resolved)));
            Type::Alias(id)
        }
        _ => unimplemented!(),
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
        match ctx.definitions.get(for_type) {
            Some(t) => t,
            None => return false,
        }
    } else {
        t1
    };
    let t2 = if let Type::Alias(for_type) = t2 {
        match ctx.definitions.get(for_type) {
            Some(t) => t,
            None => return false,
        }
    } else {
        t2
    };
    match (t1, t2) {
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
        ) => prefex_1.len() == prefex_2.len() && types_eq(ctx, typ_1, typ_2),
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
    // just debug printing
    for (id, typ_id) in &duplicates {
        println!("this type can be replaced with {id} => {typ_id}")
    }
    // update type aliases to point to the common type
    for (_, id) in &mut ctx.types {
        if let Some(new_id) = duplicates.get(id) {
            *id = *new_id;
        }
    }
    // update variable names to point to the common type
    for (_, id) in &mut ctx.variables {
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
        t @ (Type::UInt
        | Type::Int
        | Type::Byte
        | Type::Bool
        | Type::Float
        | Type::String
        | Type::Unit
        | Type::ToInfere) => t.clone(),
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
        g @ Type::Generic(_, _) => g.clone(),
        Type::Scheme { prefex, typ } => Type::Scheme {
            prefex: prefex.clone(),
            typ: Box::new(replace_aliases_with_map(typ, map)),
        },
        Type::Array(inner) => Type::Array(Box::new(replace_aliases_with_map(inner, map))),
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
        (id: $id:literal, $($field:ident: $t:expr),* $(,)?) => {
            Type::Struct {
                def: $id,
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
            (Type::Unit, Type::Unit) => true,
            (
                Type::Struct {
                    fields: fields1,
                    def: def1,
                },
                Type::Struct {
                    fields: fields2,
                    def: def2,
                },
            ) => {
                if def1 == def2 {
                    return true;
                }
                fields1.len() == fields2.len()
                    && fields1
                        .iter()
                        .zip(fields2)
                        .all(|((n1, t1), (n2, t2))| n1 == n2 && test_types_eq(ctx, t1, t2))
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
            (Type::Divergent, _) => true,
            (_, Type::Divergent) => true,
            (Type::Generic(d1, n1), Type::Generic(d2, n2)) => d1 == d2 && n1 == n2,
            (
                Type::Scheme {
                    typ: t1,
                    prefex: p1,
                },
                Type::Scheme {
                    typ: t2,
                    prefex: p2,
                },
            ) => p1.len() == p2.len() && test_types_eq(ctx, t1, t2),
            _ => false,
        }
    }

    fn apply_type_ctx_passes(source: &str) -> TypCtx {
        let lex = Lexer::new(source);
        let mut parser = Parser::new(lex);
        let res = parser.parse().unwrap();
        assert!(parser.errors().is_empty(), "errors while parsing");
        let mut tctx = TypCtx::new();
        let int_id = tctx.add_type(Type::Int);
        tctx.add_type_name("Int", int_id);
        let mut add_types = typecheck::AddTypes::new(&mut tctx);
        add_types.visit(&res);
        tctx = typecheck::resolve_type_names(tctx);
        println!("{}", tctx.debug_string());
        typecheck::alias_resolver::AliasResolver::resolve(&mut tctx);
        println!("{}", tctx.debug_string());
        typecheck::dedup_trivially_eq_types(&mut tctx);
        tctx
    }

    fn run_test(source: &str, mut expected: HashMap<TypeId, Type>) {
        let mut tctx = apply_type_ctx_passes(source);
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

    type_test! {
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

    #[test]
    fn test_mutualy_recursive_structs_resolution() {
        let source = r#"
            type A { a: C }
            type B { a: D }
            type C = B
            type D = A
        "#;
        let tctx = apply_type_ctx_passes(source);
        let mut actual = tctx.definitions;
        actual.retain(|_, v| *v != Type::Int);
        assert!(
            actual.len() == 2,
            "there should be only 2 types left (2 structs)"
        );
        let actual: Vec<_> = actual.into_iter().collect();
        let [(id1, t1), (id2, t2)] = &actual[..] else {
            panic!("expected 2 types exactly")
        };
        match t1 {
            Type::Struct { fields, .. } => match fields
                .iter()
                .map(|(s, t)| (s.as_str(), t))
                .collect::<Vec<_>>()
                .as_slice()
            {
                [("a", Type::Alias(for_type))] if for_type == id2 => {}
                _ => panic!("T1 does not have an expected shape {t1:?}"),
            },
            _ => panic!("T1 is does not have expected shape {t1:?}"),
        }

        match t2 {
            Type::Struct { fields, .. } => match fields
                .iter()
                .map(|(s, t)| (s.as_str(), t))
                .collect::<Vec<_>>()
                .as_slice()
            {
                [("a", Type::Alias(for_type))] if for_type == id1 => {}
                _ => panic!("T1 does not have an expected shape {t2:?}"),
            },
            _ => panic!("T1 is does not have expected shape {t2:?}"),
        }
    }
}
