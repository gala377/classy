use serde::{Deserialize, Serialize};

use classy_syntax::ast;

use crate::typecheck::{self, type_context::TypCtx};

pub mod package2;

#[derive(Serialize, Deserialize, Debug)]
pub enum Type {
    Unit,
    Name(String),
    Struct { fields: Vec<(String, Type)> },
    Function { args: Vec<Type>, ret: Box<Type> },
    Tuple(Vec<Type>),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Package {
    pub name: String,
    pub functions: Vec<Function>,
    pub types: Vec<(String, Type)>,
}

impl Package {
    pub fn new(name: &str, tctx: &TypCtx) -> Self {
        make_package(name, tctx)
    }

    pub fn read_headers(&self, tctx: &mut TypCtx) {
        read_package(self, tctx)
    }
}

fn make_package(name: &str, tctx: &TypCtx) -> Package {
    let mut package = Package {
        name: name.to_string(),
        functions: Vec::new(),
        types: Vec::new(),
    };
    for name in tctx.types.keys() {
        match name.as_str() {
            // skip basic types
            "Int" | "UInt" | "Bool" | "String" | "Float" | "Unit" => continue,
            _ => {}
        }
        let serialized = serialize_type(tctx, &tctx.get_type(name).unwrap(), true);
        package.types.push((name.clone(), serialized));
    }
    for (name, typ) in &tctx.variables {
        let typ = tctx.definitions.get(typ).unwrap();
        let typ = serialize_type(tctx, typ, true);
        match typ {
            Type::Function { args, ret } => {
                package.functions.push(Function {
                    name: name.clone(),
                    args,
                    ret,
                });
            }
            _ => panic!("The function types is not a function"),
        }
    }
    package
}

fn read_package(pkg: &Package, tctx: &mut TypCtx) {
    for (name, typ) in &pkg.types {
        let definition = match typ {
            Type::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, typ)| (name.clone(), deserialize_type(typ)))
                    .map(|(name, typ)| ast::TypedIdentifier { name, typ })
                    .collect();
                ast::DefinedType::Record(ast::Record { fields })
            }
            Type::Function { args, ret } => {
                let args = args.iter().map(deserialize_type).collect();
                let ret = deserialize_type(ret);
                ast::DefinedType::Alias(ast::Alias {
                    for_type: ast::Typ::Function {
                        args,
                        ret: Box::new(ret),
                        generics: Vec::new(),
                    },
                })
            }
            Type::Tuple(types) => {
                let types = types.iter().map(deserialize_type).collect();
                ast::DefinedType::Alias(ast::Alias {
                    for_type: ast::Typ::Tuple(types),
                })
            }
            Type::Name(name) => ast::DefinedType::Alias(ast::Alias {
                for_type: ast::Typ::Name(ast::Name {
                    path: vec![],
                    identifier: name.clone(),
                }),
            }),
            Type::Unit => ast::DefinedType::Alias(ast::Alias {
                for_type: ast::Typ::Unit,
            }),
        };
        let node = ast::TypeDefinition {
            name: name.clone(),
            type_variables: Vec::new(),
            definition,
            span: 0..0,
        };
        let _id = tctx.add_type_node(&node);
        let type_id = tctx.reserve_id();
        tctx.add_type_name(node.name.clone(), type_id);
    }
    for Function { name, args, ret } in &pkg.functions {
        let args: Vec<ast::Typ> = args.iter().map(deserialize_type).collect();
        let ret = deserialize_type(ret);
        let pars = std::iter::repeat("unknown".to_owned()).take(args.len());
        let node = ast::FunctionDefinition {
            name: name.clone(),
            // TODO: should be undefined
            body: ast::Expr {
                id: 0,
                kind: ast::ExprKind::Unit,
            },
            typ: ast::Typ::Function {
                args,
                ret: Box::new(ret),
                generics: Vec::new(),
            },
            parameters: pars.collect(),
            attributes: Vec::new(),
        };
        let _id = tctx.add_fn_node(&node);
        let type_id = tctx.reserve_id();
        tctx.add_variable(&node.name, type_id);
    }
}

fn deserialize_type(typ: &Type) -> ast::Typ {
    match typ {
        Type::Name(name) => ast::Typ::Name(ast::Name {
            path: vec![],
            identifier: name.clone(),
        }),
        Type::Struct { .. } => panic!("There should be no nested struct types"),
        Type::Function { args, ret } => {
            let args = args.iter().map(deserialize_type).collect();
            let ret = deserialize_type(ret);
            ast::Typ::Function {
                args,
                generics: Vec::new(),
                ret: Box::new(ret),
            }
        }
        Type::Tuple(vals) => {
            let vals = vals.iter().map(deserialize_type).collect();
            ast::Typ::Tuple(vals)
        }
        Type::Unit => ast::Typ::Unit,
    }
}

fn serialize_type(tctx: &TypCtx, typ: &typecheck::types::Type, top_level: bool) -> Type {
    match typ {
        typecheck::types::Type::Int => Type::Name("Int".to_owned()),
        typecheck::types::Type::Float => Type::Name("Float".to_owned()),
        typecheck::types::Type::Bool => Type::Name("Bool".to_owned()),
        typecheck::types::Type::String => Type::Name("String".to_owned()),
        typecheck::types::Type::UInt => Type::Name("UInt".to_owned()),
        typecheck::types::Type::Unit => Type::Unit,
        typecheck::types::Type::Struct { def, fields } => {
            if top_level {
                let fields = fields
                    .iter()
                    .map(|(name, typ)| (name.clone(), serialize_type(tctx, typ, false)))
                    .collect();
                Type::Struct { fields }
            } else {
                let node = tctx.nodes.get(def).unwrap();
                match node {
                    classy_syntax::ast::TopLevelItemKind::TypeDefinition(ast::TypeDefinition {
                        name,
                        ..
                    }) => Type::Name(name.clone()),
                    classy_syntax::ast::TopLevelItemKind::FunctionDefinition(_) => {
                        panic!("No struct node should map to a function definition")
                    }
                    def => unimplemented!("{def:?}"),
                }
            }
        }
        typecheck::types::Type::ADT { .. } => todo!(),
        typecheck::types::Type::Function { args, ret } => {
            let args = args
                .iter()
                .map(|t| serialize_type(tctx, t, false))
                .collect();
            let ret = Box::new(serialize_type(tctx, ret, false));
            Type::Function { args, ret }
        }
        typecheck::types::Type::Tuple(vals) => Type::Tuple(
            vals.iter()
                .map(|t| serialize_type(tctx, t, false))
                .collect(),
        ),
        typecheck::types::Type::Array(_) => todo!(),
        typecheck::types::Type::Alias(id) => tctx
            .types
            .iter()
            .find(|(_, tid)| **tid == *id)
            .map(|(name, _)| Type::Name(name.clone()))
            .or_else(|| {
                tctx.definitions
                    .get(id)
                    .map(|typ| serialize_type(tctx, typ, top_level))
            })
            .unwrap(),
        typecheck::types::Type::Divergent => panic!("Cannot serialize divergent type"),
        typecheck::types::Type::ToInfere => panic!("Cannot serialize to infere type"),
        typecheck::types::Type::Generic(_, _) => todo!("Cannot serialize generic type"),
        typecheck::types::Type::Scheme { .. } => todo!("Cannot serialize schema type"),
        typecheck::types::Type::Fresh(_) => {
            panic!("Fresh variables should not be present after type inference")
        }
        _ => unimplemented!(),
    }
}
