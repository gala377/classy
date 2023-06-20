use std::collections::HashMap;

use super::{
    constrait_solver::TypeReplacer,
    r#type::{Type, TypeFolder},
    type_context::TypCtx,
};

pub fn fix_types_after_inference(
    substitutions: &mut HashMap<usize, Type>,
    tctx: &mut TypCtx,
    env: &mut HashMap<usize, Type>,
) {
    fix_fresh_vars_in_substitutions(substitutions, tctx);
    fix_nested_types(substitutions, tctx);
    fix_types_in_env(substitutions, env);
}

pub fn fix_types_in_env(substitutions: &HashMap<usize, Type>, env: &mut HashMap<usize, Type>) {
    for (_, typ) in env {
        if let &mut Type::Fresh(id) = typ {
            *typ = substitutions.get(&id).unwrap().clone();
        }
    }
}

fn fix_fresh_vars_in_substitutions(substitutions: &mut HashMap<usize, Type>, tctx: &mut TypCtx) {
    let mut replacers = Vec::new();
    for (id, typ) in substitutions.iter() {
        let for_type = match typ {
            Type::Struct { def, .. } => Type::Alias(tctx.def_id_to_typ_id(*def)),
            Type::ADT { def, .. } => Type::Alias(tctx.def_id_to_typ_id(*def)),
            t => t.clone(),
        };
        replacers.push(TypeReplacer {
            fresh_type_id: *id,
            for_type,
        });
    }
    for replacer in &mut replacers {
        for (_, typ) in substitutions.iter_mut() {
            *typ = replacer.fold_type(typ.clone());
        }
        tctx.fold_types(replacer)
    }
}

struct ReplaceStructsWithAliases<'ctx> {
    top_level: bool,
    tctx: &'ctx TypCtx,
}

impl TypeFolder for ReplaceStructsWithAliases<'_> {
    fn fold_struct(&mut self, def: usize, fields: Vec<(std::string::String, Type)>) -> Type {
        if self.top_level {
            let new_folder = &mut ReplaceStructsWithAliases {
                top_level: false,
                tctx: self.tctx,
            };
            super::r#type::fold_struct(new_folder, def, fields)
        } else {
            Type::Alias(self.tctx.def_id_to_typ_id(def))
        }
    }
}

fn fix_nested_types(substitutions: &mut HashMap<usize, Type>, tctx: &TypCtx) {
    for (_, typ) in substitutions.iter_mut() {
        let mut replacer = ReplaceStructsWithAliases {
            top_level: true,
            tctx,
        };
        *typ = replacer.fold_type(typ.clone());
    }
}
