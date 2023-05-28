use std::collections::{HashMap, HashSet};

use super::{
    r#type::Type,
    type_context::{TypCtx, TypeId},
};

pub struct AliasResolver {
    resolved: HashMap<TypeId, TypeId>,
}

impl AliasResolver {
    pub fn resolve(ctx: &mut TypCtx) {
        let mut resolver = AliasResolver::new();
        resolver.resolve_aliases(ctx);
        resolver.update_names(ctx);
        resolver.resolve_aliases_shallow(ctx);
        resolver.remove_top_level_aliases(ctx);
    }

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
                | Type::Unit
                | Type::UInt
                | Type::Generic(_)
                | Type::Scheme { .. } => Type::Alias(*for_type),
                Type::Alias(..) => {
                    unreachable!("no alias should point to another alias at this point")
                }
                _ => unreachable!("other types should not be visible here"),
            };
            ctx.update_type_def(*type_id, t)
        }
    }

    fn update_names(&mut self, ctx: &mut TypCtx) {
        let mut updates = Vec::new();
        for (name, tid) in &ctx.types {
            if let Type::Alias(for_type) = ctx.definitions.get(tid).unwrap() {
                updates.push((name.clone(), *for_type));
            }
        }
        for (name, tid) in updates {
            ctx.types.insert(name, tid);
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
            ctx.update_type_def(id, typ)
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
                    t @ (Type::UInt
                    | Type::Int
                    | Type::Bool
                    | Type::Float
                    | Type::String
                    | Type::Unit
                    | Type::ToInfere
                    | Type::Generic(_)) => t.clone(),
                    // Do not resolve this types as they migh create reference cycles.
                    Type::Struct { .. } => Type::Alias(*for_type),
                    Type::Function { .. } => Type::Alias(*for_type),
                    Type::Tuple { .. } => Type::Alias(*for_type),
                    Type::ADT { .. } => Type::Alias(*for_type),
                    Type::Array { .. } => Type::Alias(*for_type),
                    Type::Scheme { .. } => Type::Alias(*for_type),
                    _ => unreachable!("other types should not be visible here"),
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
            Type::Scheme { prefex, typ } => Type::Scheme {
                prefex: prefex.clone(),
                typ: Box::new(self.resolve_shallow_aliases_in_type(ctx, typ)),
            },
            t @ (Type::UInt
            | Type::Int
            | Type::Bool
            | Type::Float
            | Type::String
            | Type::Unit
            | Type::ToInfere
            | Type::Generic(_)) => t.clone(),
            _ => unimplemented!(),
        }
    }

    fn remove_top_level_aliases(&mut self, ctx: &mut TypCtx) {
        ctx.definitions
            .retain(|_, v| if let Type::Alias(_) = v { false } else { true })
    }
}
