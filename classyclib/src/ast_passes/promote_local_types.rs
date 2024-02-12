use classy_syntax::ast::{self, Folder};

use crate::{id_provider::IdProvider, session::Session};

use super::AstPass;

pub struct PromoteAnonTypes {
    id_provider: IdProvider,
    types_to_promote: Vec<(String, ast::Record)>,
    current_function: String,
}

impl AstPass for PromoteAnonTypes {
    fn run(&mut self, ast: ast::SourceFile, _: &Session) -> ast::SourceFile {
        self.fold_program(ast)
    }
}

impl Default for PromoteAnonTypes {
    fn default() -> Self {
        Self::new()
    }
}

impl PromoteAnonTypes {
    pub fn new() -> Self {
        Self {
            // we want this to be explicitly new one and not the one shared in the session.
            // The reason for that is that we do not care about the uniqueness of the ids here.
            id_provider: IdProvider::new(),
            current_function: "<not in function>".to_owned(),
            types_to_promote: Vec::new(),
        }
    }
}

impl Folder for PromoteAnonTypes {
    fn fold_program(&mut self, program: ast::SourceFile) -> ast::SourceFile {
        let mut program = ast::fold::fold_program(self, program);
        for (name, record) in self.types_to_promote.iter() {
            let typ = ast::TopLevelItem {
                id: 0,
                export: false,
                kind: ast::TopLevelItemKind::TypeDefinition(ast::TypeDefinition {
                    name: name.clone(),
                    type_variables: Vec::new(),
                    definition: ast::DefinedType::Record(record.clone()),
                    span: 0..0,
                    constraints: Vec::new(),
                }),
            };
            program.items.push(typ);
        }
        program
    }

    fn fold_function_definition(
        &mut self,
        def: ast::FunctionDefinition,
    ) -> ast::FunctionDefinition {
        self.current_function = def.name.clone();
        let def = ast::fold::fold_function_definition(self, def);
        self.current_function = "<not in function>".to_owned();
        def
    }

    fn fold_anon_type(&mut self, fields: Vec<(String, ast::Expr)>) -> ast::ExprKind {
        let id = self.id_provider.next();
        let name = format!("{}@anon_{}", self.current_function, id);
        let record_fields = fields
            .iter()
            .map(|(name, _)| ast::TypedIdentifier {
                name: name.clone(),
                typ: ast::Typ::ToInfere,
            })
            .collect();
        let record = ast::Record {
            fields: record_fields,
        };
        self.types_to_promote.push((name.clone(), record));
        ast::fold::fold_struct_literal(
            self,
            ast::Name::Unresolved {
                path: vec![],
                identifier: name,
            },
            fields.into_iter().collect(),
        )
    }
}
