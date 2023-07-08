use std::{collections::HashMap, rc::Rc, cell::RefCell};

use super::{
    constrait_solver::TypeReplacer,
    r#type::{Type, TypeFolder},
    type_context::TypCtx, scope::Scope,
};

pub fn fix_types_after_inference(
    name: &str,
    substitutions: &mut HashMap<usize, Type>,
    tctx: &mut TypCtx,
    env: &mut HashMap<usize, Type>,
    global_scope: Rc<RefCell<Scope>>,
) {
    fix_fresh_vars_in_substitutions(substitutions, tctx);
    fix_nested_types(substitutions, tctx);
    fix_types_in_env(substitutions, env);
    let new_t = tctx.variables.get(name).unwrap().clone();
    tctx
        .definitions
        .iter_mut()
        .filter(|(id, _)| **id == new_t)
        .for_each(|(_, t)| *t = GeneralizerHelper::generalize(t.clone()));
    let new_t = tctx.definitions.get(&new_t).unwrap().clone();
    global_scope.borrow_mut().add_variable(name, new_t);
}

struct FreshFixer<'a> {
    substitutions: &'a HashMap<usize, Type>,
}

impl<'a> TypeFolder for FreshFixer<'a> {
    fn fold_fresh(&mut self, id: usize) -> Type {
        if let Some(typ) = self.substitutions.get(&id) {
            return typ.clone();
        }
        Type::Fresh(id)
    }
}

pub fn fix_types_in_env(substitutions: &HashMap<usize, Type>, env: &mut HashMap<usize, Type>) {
    let mut replacer = FreshFixer {
        substitutions: &substitutions,
    };
    for (_, typ) in env {
        *typ = replacer.fold_type(typ.clone());
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

struct TypeGeneralizer;

impl TypeFolder for TypeGeneralizer {
    fn fold_function(&mut self, args: Vec<Type>, ret: Type) -> Type {
        let mut helper = GeneralizerHelper {
            bindings: HashMap::new(),
            current_id: 0,
        };
        let args = args.into_iter().map(|t| helper.fold_type(t)).collect();
        let ret = helper.fold_type(ret);
        if helper.bindings.is_empty() {
            Type::Function {
                args,
                ret: Box::new(ret),
            }
        } else {
            let mut prefex = Vec::new();
            for i in 0..helper.current_id {
                prefex.push(PREFEX_NAMES[i % PREFEX_NAMES.len()].to_owned());
            }
            Type::Scheme {
                prefex,
                typ: Box::new(Type::Function {
                    args,
                    ret: Box::new(ret),
                }),
            }
        }
    }
}

struct GeneralizerHelper {
    bindings: HashMap<usize, Type>,
    current_id: usize,
}

impl GeneralizerHelper {
    pub fn generalize(t: Type) -> Type {
        let mut helper = GeneralizerHelper {
            bindings: HashMap::new(),
            current_id: 0,
        };
        let t = helper.fold_type(t);
        if helper.bindings.is_empty() {
            t
        } else {
            let mut prefex = Vec::new();
            for i in 0..helper.current_id {
                prefex.push(PREFEX_NAMES[i % PREFEX_NAMES.len()].to_owned());
            }
            Type::Scheme {
                prefex,
                typ: Box::new(t),
            }
        }
    }

    pub fn new_generic(&mut self) -> Type {
        let id = self.current_id;
        self.current_id += 1;
        Type::Generic(0, id)
    }
}

impl TypeFolder for GeneralizerHelper {
    fn fold_fresh(&mut self, id: usize) -> Type {
        match self.bindings.get(&id) {
            Some(t) => t.clone(),
            None => {
                let t = self.new_generic();
                self.bindings.insert(id, t.clone());
                t
            }
        }
    }
}

static PREFEX_NAMES: &[&str] = &[
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s",
    "t", "u", "w", "x", "y", "z",
];
