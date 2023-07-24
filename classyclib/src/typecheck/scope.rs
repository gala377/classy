use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    r#type::Type,
    type_context::{TypCtx, TypeId},
};

pub struct Scope {
    resolved_types: HashMap<String, Type>,
    resolved_vars: HashMap<String, Type>,
    type_ids: HashMap<usize, Type>,
    parent: Option<Rc<RefCell<Scope>>>,
    increases_debruijn: bool,
}

impl Scope {
    pub fn get_global(scope: Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
        let mut current = scope.clone();
        return loop {
            let parent = current.borrow().parent.clone();
            if parent.is_none() {
                break current;
            }
            current = parent.unwrap();
        };
    }

    pub fn is_global(&self, name: &str) -> bool {
        if self.resolved_vars.contains_key(name) {
            if self.parent.is_none() {
                return true;
            }
            return false;
        }
        if self.parent.is_none() {
            return false;
        }
        self.parent.as_ref().unwrap().borrow().is_global(name)
    }

    pub fn from_type_ctx(type_ctx: &TypCtx) -> Self {
        let resolved_vars = {
            type_ctx
                .variables
                .iter()
                .map(|(name, typ_id)| {
                    let typ = type_ctx.definitions.get(typ_id).unwrap().clone();
                    (name.clone(), typ)
                })
                .collect()
        };
        let resolved_types = {
            type_ctx
                .types
                .iter()
                .map(|(name, typ_id)| {
                    let typ = type_ctx.definitions.get(typ_id).unwrap().clone();
                    (name.clone(), typ)
                })
                .collect()
        };
        Self {
            resolved_types,
            resolved_vars,
            type_ids: type_ctx.definitions.clone(),
            parent: None,
            increases_debruijn: false,
        }
    }

    pub fn new(parent: Rc<RefCell<Scope>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            resolved_types: HashMap::new(),
            resolved_vars: HashMap::new(),
            type_ids: HashMap::new(),
            parent: Some(parent),
            increases_debruijn: false,
        }))
    }

    pub fn new_debruijn(parent: Rc<RefCell<Scope>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            resolved_types: HashMap::new(),
            resolved_vars: HashMap::new(),
            type_ids: HashMap::new(),
            parent: Some(parent),
            increases_debruijn: true,
        }))
    }

    pub fn type_of(&self, name: &str) -> Option<Type> {
        self.resolved_vars.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().type_of(name))
                .map(|t| match t {
                    Type::Generic(index, pos) if self.increases_debruijn => {
                        Type::Generic(index + 1, pos)
                    }
                    t => t,
                })
        })
    }

    pub fn type_of_with_pos(&self, name: &str) -> Option<(usize, Type)> {
        self.resolved_vars
            .get(name)
            .cloned()
            .map(|t| (0, t))
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.borrow().type_of_with_pos(name))
                    .map(|(pos, t)| (pos + 1, t))
            })
    }

    pub fn lookup_type(&self, name: &str) -> Option<Type> {
        self.resolved_types.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().lookup_type(name))
        })
    }

    pub fn add_variable(&mut self, name: &str, typ: Type) {
        self.resolved_vars.insert(name.to_owned(), typ);
    }

    pub fn resolve_alias(&self, for_type: TypeId) -> Option<Type> {
        self.type_ids.get(&for_type).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().resolve_alias(for_type))
        })
    }
}
