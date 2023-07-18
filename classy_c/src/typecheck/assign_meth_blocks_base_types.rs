use std::collections::HashMap;

use super::{
    r#type::Type,
    type_context::{MethodSet, TypCtx},
};

pub fn assign_base_types(mut tctx: TypCtx) -> TypCtx {
    let mut new_method_blocks = HashMap::new();
    for (typ_id, old_sets) in &tctx.methods {
        let base_typ = get_base_type(&tctx, &tctx.resolve_alias(*typ_id), *typ_id);
        let sets: &mut HashMap<usize, HashMap<String, usize>> =
            new_method_blocks.entry(base_typ).or_default();
        for set in old_sets {
            let methods: &mut HashMap<String, usize> = sets.entry(set.specialisation).or_default();
            for (name, typ) in &set.methods {
                if methods.insert(name.clone(), *typ).is_some() {
                    panic!("redefinition of method: {name}");
                }
            }
        }
    }
    let mut tmp = HashMap::new();
    for (id, methods_set) in new_method_blocks {
        let sets: &mut Vec<_> = tmp.entry(id).or_default();
        for (specialisation, methods) in methods_set {
            sets.push(MethodSet {
                specialisation,
                methods,
            });
        }
    }
    tctx.methods = tmp;
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
