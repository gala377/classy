pub mod add_types;
pub mod alias_resolver;
pub mod assign_meth_blocks_base_types;
pub mod ast_to_type;
pub mod constraints;
pub mod constrait_solver;
pub mod fix_fresh;
pub mod inference;
pub mod scope;
pub mod type_context;
pub mod types;

use classy_syntax::{
    self,
    ast::{self, Visitor},
};
use std::collections::HashMap;

use add_types::AddTypes;
use alias_resolver::AliasResolver;
use type_context::*;
use types::*;

pub use inference::run;

/// Create a top level type context containing all types and functions with
/// their respective types in the simplest form possible.
pub fn prepare_for_typechecking(program: &classy_syntax::ast::SourceFile) -> TypCtx {
    let mut tctx = TypCtx::new();
    let mut add_types = AddTypes::with_primitive_types(&mut tctx);
    add_types.visit(program);
    tctx = resolve_type_names(tctx);
    AliasResolver::resolve(&mut tctx);
    dedup_trivially_eq_types(&mut tctx);
    // Also, as a final transformation on method blocks
    // creates a reverse mapping to retrieve method block based on its defintion id
    tctx = assign_meth_blocks_base_types::assign_base_types(tctx);
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
///
/// Actually resolve aliases and replace aliases with map should be
/// a special case of fold but well.
pub fn resolve_type_names(mut ctx: TypCtx) -> TypCtx {
    let mut type_updates = HashMap::new();
    let nodes = ctx.nodes.clone();
    for (def_id, def) in &nodes {
        match def {
            ast::TopLevelItemKind::TypeDefinition(ast::TypeDefinition {
                name,
                definition,
                type_variables,
                ..
            }) => {
                ast_to_type::resolve_top_level_type(
                    &mut ctx,
                    name,
                    definition,
                    def_id,
                    type_variables,
                    &mut type_updates,
                );
            }
            ast::TopLevelItemKind::ConstDefinition(ast::ConstDefinition { name, typ, .. }) => {
                ast_to_type::resolve_const_def(name, typ, &mut ctx);
            }

            ast::TopLevelItemKind::FunctionDefinition(ast::FunctionDefinition {
                name,
                typ,
                ..
            }) => ast_to_type::resolve_fn_def(typ, &mut ctx, name),
            ast::TopLevelItemKind::MethodsBlock(ast::MethodsBlock {
                name: _name,
                typ,
                methods,
            }) => {
                ast_to_type::resolve_methods_block(typ, &mut ctx, methods);
            }
            t => todo!("{t:?}"),
        }
    }
    for (id, t) in type_updates {
        ctx.update_type_def(id, t)
    }
    ctx
}

pub fn dedup_trivially_eq_types(ctx: &mut TypCtx) {
    let mut duplicates = HashMap::new();
    for (t_id_1, typ_1) in &ctx.definitions {
        for (t_id_2, typ_2) in &ctx.definitions {
            if t_id_1 == t_id_2 {
                continue;
            }
            if !types::types_eq(ctx, typ_1, typ_2) {
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
    for id in ctx.types.values_mut() {
        if let Some(new_id) = duplicates.get(id) {
            *id = *new_id;
        }
    }
    // update variable names to point to the common type
    for id in ctx.variables.values_mut() {
        if let Some(new_id) = duplicates.get(id) {
            *id = *new_id;
        }
    }
    // update method blocks, their types and their methods types
    // to point to a common type
    // note that base types have still not been resolved.
    let mut new_methods_entry = HashMap::new();
    for (id, sets) in &ctx.methods {
        let new_id = duplicates.get(id).unwrap_or(id);
        let mut new_method_sets = Vec::new();
        for set in sets {
            let mut new_methods = HashMap::new();
            for (meth_name, meth_type) in &set.methods {
                let new_type = duplicates.get(meth_type).unwrap_or(meth_type);
                new_methods.insert(meth_name.clone(), *new_type);
            }
            new_method_sets.push(MethodSet {
                def_id: set.def_id,
                specialisation: *new_id,
                methods: new_methods,
            })
        }
        let existing_set: &mut Vec<MethodSet> = new_methods_entry.entry(*new_id).or_default();
        existing_set.append(&mut new_method_sets);
    }
    ctx.methods = new_methods_entry;

    // Remove unnecessary definitions
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
        Type::App { typ, args } => Type::App {
            typ: Box::new(replace_aliases_with_map(typ, map)),
            args: args
                .iter()
                .map(|t| replace_aliases_with_map(t, map))
                .collect(),
        },
        Type::Array(inner) => Type::Array(Box::new(replace_aliases_with_map(inner, map))),
        Type::ADT { def, constructors } => Type::ADT {
            def: *def,
            constructors: constructors
                .iter()
                .map(|(n, t)| (n.clone(), replace_aliases_with_map(t, map)))
                .collect(),
        },
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::typecheck::{
        self,
        type_context::{TypCtx, TypeId},
        types::Type,
    };
    use classy_syntax::{ast::Visitor, lexer::Lexer, parser::Parser};
    use std::collections::HashMap;

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
            .extend(expected.iter().map(|(k, v)| (*k, v.clone())));
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
