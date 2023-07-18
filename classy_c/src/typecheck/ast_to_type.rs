use std::collections::HashMap;

use crate::syntax::ast;

use super::{
    r#type::{DeBruijn, Type},
    type_context::{TypCtx, TypeId},
};
use crate::scope::Scope;

pub type PrefexScope = Scope<String, usize>;

impl PrefexScope {
    pub fn add_type_var(&mut self, name: impl Into<String>) {
        self.add(name.into(), self.curr_scope_len());
    }

    pub fn add_type_vars(&mut self, vars: &[String]) {
        for var in vars {
            self.add_type_var(var);
        }
    }
}

pub fn resolve_fn_def(typ: &ast::Typ, ctx: &mut TypCtx, name: &String) {
    let mut resolver = TypeResolver::new(ctx);
    let mut scope = PrefexScope::new();
    let Type::Alias(typ) = resolver.resolve_type(&mut scope, typ) else {
        panic!("Alias resolver should only return aliases")
    };
    assert!(
        ctx.variables.insert(name.clone(), typ).is_some(),
        "updating type definition for funtion that does not exist {}",
        name
    );
}

pub fn resolve_top_level_type(
    ctx: &mut TypCtx,
    name: &String,
    definition: &ast::DefinedType,
    def_id: &usize,
    type_variables: &Vec<ast::TypeVariable>,
    updates: &mut HashMap<usize, Type>,
) {
    let exp_msg = format!("the types should have been prepopulated: {name}");
    let type_id = *ctx.types.get(name).expect(&exp_msg);
    let mut scope = PrefexScope::new();
    for generic in type_variables {
        scope.add_type_var(generic.name.clone());
    }
    let prefex = type_variables.iter().map(|x| x.name.clone()).collect();
    let mut resolver = TypeResolver::new(ctx);
    let resolved_type = match definition {
        ast::DefinedType::Alias(ast::Alias { for_type: inner }) => {
            let resolved = resolver.resolve_type(&mut scope, inner);
            if scope.is_empty() {
                resolved
            } else {
                Type::Scheme {
                    prefex,
                    typ: Box::new(resolved),
                }
            }
        }
        ast::DefinedType::Record(ast::Record { fields }) => {
            let mut resolved_fields = Vec::with_capacity(fields.len());
            for ast::TypedName { name, typ } in fields {
                let resolved = resolver.resolve_type(&mut scope, typ);
                resolved_fields.push((name.clone(), resolved));
            }
            if scope.is_empty() {
                Type::Struct {
                    def: *def_id,
                    fields: resolved_fields,
                }
            } else {
                Type::Scheme {
                    prefex,
                    typ: Box::new(Type::Struct {
                        def: *def_id,
                        fields: resolved_fields,
                    }),
                }
            }
        }
        ast::DefinedType::ADT(ast::ADT { discriminants }) => {
            let mut resolved_discriminants = Vec::with_capacity(discriminants.len());
            for ast::Discriminant {
                constructor,
                arguments,
            } in discriminants
            {
                match arguments {
                    ast::DiscriminantKind::Tuple(types) => {
                        let resolved = types
                            .iter()
                            .map(|t| resolver.resolve_type(&mut scope, t))
                            .collect();
                        resolved_discriminants.push((constructor.clone(), Type::Tuple(resolved)));
                    }
                    ast::DiscriminantKind::Record(fields) => {
                        let mut resolved_fields = Vec::with_capacity(fields.len());
                        for (name, typ) in fields {
                            let resolved = resolver.resolve_type(&mut scope, typ);
                            resolved_fields.push((name.clone(), resolved));
                        }
                        resolved_discriminants.push((
                            constructor.clone(),
                            Type::Struct {
                                def: *def_id,
                                fields: resolved_fields,
                            },
                        ));
                    }
                    ast::DiscriminantKind::Empty => {
                        resolved_discriminants.push((constructor.clone(), Type::Unit));
                    }
                }
            }
            let t = Type::ADT {
                def: *def_id,
                constructors: resolved_discriminants,
            };
            if scope.is_empty() {
                t
            } else {
                Type::Scheme {
                    prefex,
                    typ: Box::new(t),
                }
            }
        }
    };
    if let Some(t) = updates.insert(type_id, resolved_type) {
        panic!(
            "Redefinition of type: {} => {}, previous value {:?}",
            type_id, name, t
        );
    }
}

pub fn resolve_methods_block(
    for_t: &ast::Typ,
    ctx: &mut TypCtx,
    methods: &[ast::FunctionDefinition],
) {
    let mut resolver = TypeResolver::new(ctx);
    let mut scope = PrefexScope::new();
    let resolved_for_t = resolver.resolve_type(&mut scope, for_t);
    let generics = match for_t {
        ast::Typ::Poly(generics, _) => generics.clone(),
        ast::Typ::Function { generics, .. } => generics.clone(),
        _ => vec![],
    };
    if !generics.is_empty() {
        scope.new_scope();
        scope.add_type_vars(&generics);
    }

    for meth in methods {}

    if !generics.is_empty() {
        scope.pop_scope();
    }
}

pub fn resolve_methods_block_type(typ: &ast::Typ, ctx: &mut TypCtx) -> TypeId {
    let mut resolver = TypeResolver::new(ctx);
    match typ {
        ast::Typ::Function {
            args,
            ret,
            generics,
        } => {
            let mut scope = PrefexScope::new();
            scope.add_type_vars(generics);
            let resolved_args: Vec<_> = args
                .iter()
                .map(|t| resolver.resolve_type(&mut scope, t))
                .collect();
            let resolved_ret = resolver.resolve_type(&mut scope, ret);
            ctx.mk_function_scheme(generics.clone(), &resolved_args, resolved_ret)
        }
        ast::Typ::Name(_) => {
            let resolved = resolver.resolve_type(&mut PrefexScope::new(), typ);
            ctx.add_type(resolved)
        }
        ast::Typ::Application { callee, args } => {
            let mut scope = PrefexScope::new();
            let resolved_callee = resolver.resolve_type(&mut scope, callee);
            let resolved_args: Vec<_> = args
                .iter()
                .map(|t| resolver.resolve_type(&mut scope, t))
                .collect();
            ctx.add_type(Type::App {
                typ: Box::new(resolved_callee),
                args: resolved_args,
            })
        }
        ast::Typ::Poly(generics, t) => {
            let mut scope = PrefexScope::new();
            scope.add_type_vars(generics);
            let resolved = resolver.resolve_type(&mut scope, t);
            ctx.add_type(resolved)
        }
        _ => panic!("invalid type for methods definition {:?}", typ),
    }
}

struct TypeResolver<'ctx> {
    names: &'ctx HashMap<String, TypeId>,
    definitions: &'ctx mut HashMap<TypeId, Type>,
    next_id: &'ctx mut TypeId,
    unit_id: TypeId,
    to_infere_id: TypeId,
}

impl<'ctx> TypeResolver<'ctx> {
    pub fn new(tctx: &'ctx mut TypCtx) -> Self {
        Self {
            names: &tctx.types,
            definitions: &mut tctx.definitions,
            next_id: &mut tctx.next_id,
            unit_id: tctx.unit_id,
            to_infere_id: tctx.to_infere_id,
        }
    }

    fn next_id(&mut self) -> usize {
        let id = *self.next_id;
        *self.next_id += 1;
        id
    }

    fn add_definition(&mut self, typ: Type) -> Type {
        let id = self.next_id();
        self.definitions.insert(id, typ);
        Type::Alias(id)
    }

    pub fn resolve_type(&mut self, prefex: &mut PrefexScope, typ: &ast::Typ) -> Type {
        match typ {
            ast::Typ::Name(n) => match prefex.get(n) {
                Some(index) => {
                    let pos = prefex.position(n).unwrap();
                    Type::Generic(DeBruijn(pos as isize), *index)
                }
                None => Type::Alias(
                    *self
                        .names
                        .get(n)
                        .unwrap_or_else(|| panic!("type not found, {n}")),
                ),
            },
            ast::Typ::Tuple(types) => {
                let resolved = types.iter().map(|t| self.resolve_type(prefex, t)).collect();
                self.add_definition(Type::Tuple(resolved))
            }
            ast::Typ::Function {
                args,
                ret,
                generics,
            } => {
                if generics.is_empty() {
                    let resolved_ars = args.iter().map(|t| self.resolve_type(prefex, t)).collect();
                    let resolved_ret = self.resolve_type(prefex, ret);
                    return self.add_definition(Type::Function {
                        args: resolved_ars,
                        ret: Box::new(resolved_ret),
                    });
                }
                prefex.with_scope(|scope| {
                    scope.add_type_vars(generics);
                    let resolved_args = args.iter().map(|t| self.resolve_type(scope, t)).collect();
                    let resolved_ret = self.resolve_type(scope, ret);
                    self.add_definition(Type::Function {
                        args: resolved_args,
                        ret: Box::new(resolved_ret),
                    })
                })
            }
            ast::Typ::Unit => Type::Alias(self.unit_id),
            ast::Typ::ToInfere => Type::Alias(self.to_infere_id),
            ast::Typ::Array(inner) => {
                let resolved = self.resolve_type(prefex, inner);
                self.add_definition(Type::Array(Box::new(resolved)))
            }
            ast::Typ::Application { callee, args } => {
                let resolved_callee = self.resolve_type(prefex, callee);
                let resolved_args = args.iter().map(|t| self.resolve_type(prefex, t)).collect();
                let t = Type::App {
                    typ: Box::new(resolved_callee),
                    args: resolved_args,
                };
                self.add_definition(t)
            }
            ast::Typ::Poly(generics, t) => {
                if generics.is_empty() {
                    return self.resolve_type(prefex, t);
                }
                let t = prefex.with_scope(|scope| {
                    scope.add_type_vars(generics);
                    self.resolve_type(scope, t)
                });
                self.add_definition(Type::Scheme {
                    prefex: generics.clone(),
                    typ: Box::new(t),
                })
            }
        }
    }
}
