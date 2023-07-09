use std::collections::HashMap;

use crate::syntax::ast;

use super::{type_context::{TypCtx, TypeId}, r#type::Type};


pub fn resolve_fn_def(typ: &ast::Typ, ctx: &mut TypCtx, name: &String, def_id: usize) {
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

pub fn resolve_top_level_type(
    ctx: &mut TypCtx,
    name: &String,
    definition: &ast::DefinedType,
    def_id: &usize,
    type_variables: &Vec<ast::TypeVariable>,
    updates: &mut HashMap<usize, Type>,
) {
    let exp_msg = format!("the types should have been prepopulated: {name}");
    let type_id = ctx.types.get(name).expect(&exp_msg);
    let prefex = type_variables
        .iter()
        .map(|v| v.name.clone())
        .collect::<Vec<_>>();
    let resolved_type = match definition {
        ast::DefinedType::Alias(ast::Alias { for_type: inner }) => {
            let resolved = resolve_type(
                &ctx.types,
                &mut ctx.definitions,
                &mut ctx.next_id,
                ctx.unit_id,
                ctx.to_infere_id,
                &prefex,
                *def_id,
                inner,
            );
            if prefex.is_empty() {
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
                let resolved = resolve_type(
                    &ctx.types,
                    &mut ctx.definitions,
                    &mut ctx.next_id,
                    ctx.unit_id,
                    ctx.to_infere_id,
                    &prefex,
                    *def_id,
                    typ,
                );
                resolved_fields.push((name.clone(), resolved));
            }
            if prefex.is_empty() {
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
        ast::Typ::Application {
            callee,
            generics,
            args,
        } => {
            let resolved_callee = resolve_type(
                names,
                definitions,
                next_id,
                unit_id,
                to_infere_id,
                prefex,
                curr_def_id,
                callee,
            );
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
            let t = Type::App {
                typ: Box::new(resolved_callee),
                args: resolved_args,
            };
            if !generics.is_empty() {
                /* 
                What we need to do is somehow scope the prefex
                Because those generics create a higher kinded type
                this case applies for something like that
                
                type Foo a m = (m, a)
                type Bar m = forall a => Foo a m
                
                In the case of Bar, Foo has a typevar `a` scoped to itself
                but typevar `m` is scoped to Bar.
                */
                todo!("Higher kinded types are not yet supported")
            }
            let id = *next_id;
            *next_id += 1;
            definitions.insert(
                id,
                t,
            );
            Type::Alias(id)
        }
    }
}
