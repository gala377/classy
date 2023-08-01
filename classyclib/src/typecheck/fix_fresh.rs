use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    constrait_solver::FreshTypeReplacer,
    scope::Scope,
    type_context::TypCtx,
    types::{DeBruijn, Type, TypeFolder},
};

pub fn fix_types_after_inference(
    substitutions: &mut HashMap<usize, Type>,
    tctx: &mut TypCtx,
    env: &mut HashMap<usize, Type>,
) {
    fix_types_in_env(substitutions, env);
    fix_fresh_vars_in_substitutions(substitutions, tctx);
    fix_nested_types(substitutions, tctx);
    //generalize_types(tctx, name, global_scope, env);
}

pub fn generalize_types(
    tctx: &mut TypCtx,
    name: &str,
    global_scope: Rc<RefCell<Scope>>,
    env: &mut HashMap<usize, Type>,
) {
    // Generalize and update type in typ ctx
    let new_t = *tctx.variables.get(name).unwrap();
    let mut generalizer = GeneralizerHelper {
        bindings: HashMap::new(),
        current_id: 0,
        debruijn: DeBruijn::zero(),
        above: 0,
    };
    let t = tctx.definitions.get_mut(&new_t).unwrap();
    *t = generalizer.generalize(t.clone());
    let new_t = tctx.definitions.get(&new_t).unwrap().clone();
    global_scope.borrow_mut().add_variable(name, new_t);
    // Update generalized type in env
    fix_types_in_env(&generalizer.bindings, env);
}

pub fn generalize_type_above(
    above: usize,
    name: &str,
    scope: Rc<RefCell<Scope>>,
    env: &mut HashMap<usize, Type>,
) {
    let t = scope.borrow().type_of(name).unwrap().clone();
    let mut generalizer = GeneralizerHelper {
        bindings: HashMap::new(),
        current_id: 0,
        debruijn: DeBruijn::zero(),
        above,
    };
    let new_t = match generalizer.generalize(t) {
        t @ Type::Scheme { .. } => t,
        t => Type::Scheme {
            prefex: Vec::new(),
            typ: Box::new(t),
        },
    };
    scope.borrow_mut().add_variable(name, new_t.clone());
    fix_types_in_env(&generalizer.bindings, env);
}

pub fn fix_types_in_env(substitutions: &HashMap<usize, Type>, env: &mut HashMap<usize, Type>) {
    let mut replacer = FreshTypeReplacer {
        substitutions: substitutions.clone(),
    };
    for typ in env.values_mut() {
        *typ = replacer.fold_type(typ.clone()).unwrap();
    }
}

fn fix_fresh_vars_in_substitutions(substitutions: &mut HashMap<usize, Type>, tctx: &mut TypCtx) {
    let mut replacer = FreshTypeReplacer {
        substitutions: HashMap::new(),
    };
    for (id, typ) in substitutions.iter() {
        replacer.substitutions.insert(*id, typ.clone());
    }
    for (_, typ) in substitutions.iter_mut() {
        *typ = replacer.fold_type(typ.clone()).unwrap();
    }
    tctx.fold_types(&mut replacer);
}

struct ReplaceStructsWithAliases<'ctx> {
    top_level: bool,
    tctx: &'ctx TypCtx,
}

impl TypeFolder for ReplaceStructsWithAliases<'_> {
    type Error = ();

    fn fold_struct(
        &mut self,
        def: usize,
        fields: Vec<(std::string::String, Type)>,
    ) -> Result<Type, ()> {
        if self.top_level {
            let new_folder = &mut ReplaceStructsWithAliases {
                top_level: false,
                tctx: self.tctx,
            };
            super::types::fold_struct(new_folder, def, fields)
        } else {
            let type_id = self.tctx.def_id_to_typ_id(def);
            match self.tctx.definitions.get(&type_id).unwrap() {
                // Only replace in case its a plain, non generic struct
                // if its anything but that then it is probably an instantiated type
                Type::Struct { .. } => Ok(Type::Alias(type_id)),
                t => self.fold_type(t.clone()),
            }
        }
    }
}

fn fix_nested_types(substitutions: &mut HashMap<usize, Type>, tctx: &TypCtx) {
    for (_, typ) in substitutions.iter_mut() {
        let mut replacer = ReplaceStructsWithAliases {
            top_level: true,
            tctx,
        };
        *typ = replacer.fold_type(typ.clone()).unwrap();
    }
}

struct GeneralizerHelper {
    bindings: HashMap<usize, Type>,
    current_id: usize,
    debruijn: DeBruijn,
    above: usize,
}

impl GeneralizerHelper {
    pub fn generalize(&mut self, t: Type) -> Type {
        let t = self.fold_type(t).unwrap();
        if self.bindings.is_empty() {
            println!("Folded type and bindigns are empty");
            t
        } else {
            println!("Folded type and bindings are not empty");
            let mut prefex = Vec::new();
            for i in 0..self.current_id {
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
        Type::Generic(self.debruijn.clone(), id)
    }
}

impl TypeFolder for GeneralizerHelper {
    type Error = ();

    fn fold_scheme(
        &mut self,
        prefex: Vec<super::type_context::Name>,
        typ: Type,
    ) -> Result<Type, ()> {
        self.debruijn += 1;
        let t = self.fold_type(typ)?;
        self.debruijn -= 1;
        Ok(Type::Scheme {
            prefex,
            typ: Box::new(t),
        })
    }

    fn fold_fresh(&mut self, id: usize) -> Result<Type, ()> {
        if id < self.above {
            return Ok(Type::Fresh(id));
        }
        Ok(self.bindings.get(&id).cloned().unwrap_or_else(|| {
            let t = self.new_generic();
            self.bindings.insert(id, t.clone());
            t
        }))
    }
}

static PREFEX_NAMES: &[&str] = &[
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s",
    "t", "u", "w", "x", "y", "z",
];
