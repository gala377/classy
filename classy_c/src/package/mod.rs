use serde::{Deserialize, Serialize};

use crate::{
    syntax::ast,
    typecheck::{self, type_context::TypCtx},
};

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

pub fn make_package(name: &str, tctx: &TypCtx) -> Package {
    let mut package = Package {
        name: name.to_string(),
        functions: Vec::new(),
        types: Vec::new(),
    };
    for (name, _) in &tctx.types {
        let serialized = serialize_type(tctx, &tctx.get_type(name).unwrap());
        package.types.push((name.clone(), serialized));
    }
    package
}

// pub fn read_package(pkg: &Package, tctx: &mut TypCtx) -> TypCtx {
//     for (name, typ) in &pkg.types {
//         let node = ast::TypeDefinition {
//             name: name.clone(),
//             type_variables: Vec::new(),
//             definition: match typ {
//                 Type::Name(name) => ast::DefinedType::Alias(ast::Alias {
//                     for_type: ast::Typ::Name(name.clone()),
//                 }),
//                 Type::Struct { fields } => {
//                     let fields = fields
//                         .iter()
//                         .map(|(name, typ)| (name.clone(), ast::Typ::Name(typ.to_string())))
//                         .collect();
//                     ast::DefinedType::Record(fields)
//                 }
//                 Type::Function { args, ret } => {
//                     let args = args.iter().map(|t| ast::Typ::Name(t.to_string())).collect();
//                     let ret = ast::Typ::Name(ret.to_string());
//                     ast::DefinedType::Function(args, ret)
//                 }
//                 Type::Tuple(vals) => {
//                     let vals = vals.iter().map(|t| ast::Typ::Name(t.to_string())).collect();
//                     ast::DefinedType::Tuple(vals)
//                 }
//                 Type::Unit => ast::DefinedType::Alias(ast::Alias {
//                     for_type: ast::Typ::Unit,
//                 }),
//             },
//             span: 0..0,
//         };
//         let _id = tctx.add_type_node(node);
//         let type_id = tctx.reserve_id();
//         self.ctx.add_type_name(node.name.clone(), type_id);
//     }
//     tctx
// }

// fn deserialize_type(typ: &Type) -> ast::Typ {
//     match typ {
//         Type::Name(name) => ast::Typ::Name(name.clone()),
//         Type::Struct { fields } => {
//             let fields = fields
//                 .iter()
//                 .map(|(name, typ)| (name.clone(), deserialize_type(typ)))
//                 .map(|(name, typ)| ast::TypedName {
//                     name,
//                     typ,
//                 })
//                 .collect();
//             ast::Typ::
//             ast::DefinedType::Record(ast::Record { fields })
//         }
//         Type::Function { args, ret } => {
//             let args = args.iter().map(|t| deserialize_type(t)).collect();
//             let ret = deserialize_type(ret);
//             ast::DefinedType::Function(args, ret)
//         }
//         Type::Tuple(vals) => {
//             let vals = vals.iter().map(|t| deserialize_type(t)).collect();
//             ast::DefinedType::Tuple(vals)
//         }
//         Type::Unit => ast::DefinedType::Alias(ast::Alias {
//             for_type: ast::Typ::Unit,
//         }),
//     }
// }

fn serialize_type(tctx: &TypCtx, typ: &typecheck::r#type::Type) -> Type {
    match typ {
        typecheck::r#type::Type::Int => Type::Name("Int".to_owned()),
        typecheck::r#type::Type::Float => Type::Name("Float".to_owned()),
        typecheck::r#type::Type::Bool => Type::Name("Bool".to_owned()),
        typecheck::r#type::Type::String => Type::Name("String".to_owned()),
        typecheck::r#type::Type::UInt => Type::Name("UInt".to_owned()),
        typecheck::r#type::Type::Unit => Type::Unit,
        typecheck::r#type::Type::Struct { fields, .. } => {
            let fields = fields
                .iter()
                .map(|(name, typ)| (name.clone(), serialize_type(tctx, typ)))
                .collect();
            Type::Struct { fields }
        }
        typecheck::r#type::Type::ADT { .. } => todo!(),
        typecheck::r#type::Type::Function { args, ret } => {
            let args = args.iter().map(|t| serialize_type(tctx, t)).collect();
            let ret = Box::new(serialize_type(tctx, ret));
            Type::Function { args, ret }
        }
        typecheck::r#type::Type::Tuple(vals) => {
            Type::Tuple(vals.iter().map(|t| serialize_type(tctx, t)).collect())
        }
        typecheck::r#type::Type::Array(_) => todo!(),
        typecheck::r#type::Type::Alias(id) => tctx
            .types
            .iter()
            .find(|(_, tid)| **tid == *id)
            .map(|(name, _)| Type::Name(name.clone()))
            .unwrap(),
        typecheck::r#type::Type::Divergent => panic!("Cannot serialize divergent type"),
        typecheck::r#type::Type::ToInfere => panic!("Cannot serialize to infere type"),
    }
}
