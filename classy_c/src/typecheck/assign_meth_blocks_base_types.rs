use std::{any::TypeId, collections::HashMap};

use super::{
    r#type::Type,
    type_context::{MethodSet, TypCtx},
};

pub fn assign_base_types(mut tctx: TypCtx) -> TypCtx {
    let mut new_method_blocks = HashMap::new();
    println!("\n\n\n\n\nASSIGNING BASE TYPES\n\n\n\n\n");
    for (typ_id, old_sets) in &tctx.methods {
        let base_typ = get_base_type(&tctx, &tctx.resolve_alias(*typ_id), *typ_id);
        let sets: &mut Vec<MethodSet> = new_method_blocks.entry(base_typ).or_default();
        sets.extend_from_slice(old_sets);
    }
    tctx.methods = new_method_blocks;
    tctx
}

fn get_base_type(tctx: &TypCtx, t: &Type, previous_alias_id: usize) -> usize {
    println!("Getting base type for: {t:?}");
    match t {
        Type::Alias(tid) => get_base_type(tctx, &tctx.resolve_alias(*tid), *tid),
        Type::Scheme { typ, .. } => get_base_type(tctx, &*typ, previous_alias_id),
        Type::App { typ, .. } => get_base_type(tctx, &*typ, previous_alias_id),
        Type::Int
        | Type::UInt
        | Type::Bool
        | Type::String
        | Type::Float
        | Type::Struct { .. }
        | Type::Function { .. }
        | Type::Unit
        | Type::Tuple(_)
        | Type::ADT { .. }
        | Type::Array(_)
        | Type::Byte => {
            println!("Type can be resolved to previous alias: {previous_alias_id}");
            previous_alias_id
        }
        t => panic!("Not appropriate type for methods {t:?}"),
    }
}
