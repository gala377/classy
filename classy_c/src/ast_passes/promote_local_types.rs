use crate::syntax::ast::{self, Folder};

use super::AstPass;

pub struct PromoteAnonTypes {
    id: usize,
    types_to_promote: Vec<(String, ast::Record)>,
    current_function: String,
}

impl AstPass for PromoteAnonTypes {
    fn run(&mut self, ast: ast::Program) -> ast::Program {
        self.fold_program(ast)
    }
}

impl PromoteAnonTypes {
    pub fn new() -> Self {
        Self {
            id: 0,
            current_function: "<not in function>".to_owned(),
            types_to_promote: Vec::new(),
        }
    }

    pub fn next_id(&mut self) -> usize {
        let id = self.id;
        self.id += 1;
        id
    }
}

impl Folder for PromoteAnonTypes {
    fn fold_program(&mut self, program: ast::Program) -> ast::Program {
        let mut program = ast::fold::fold_program(self, program);
        for (name, record) in self.types_to_promote.iter() {
            let typ = ast::TopLevelItem::TypeDefinition(ast::TypeDefinition {
                name: name.clone(),
                type_variables: Vec::new(),
                definition: ast::DefinedType::Record(record.clone()),
                span: 0..0,
            });
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
        let id = self.next_id();
        let name = format!("{}@anon_{}", self.current_function, id);
        let record_fields = fields
            .iter()
            .map(|(name, _)| ast::TypedName {
                name: name.clone(),
                typ: ast::Typ::ToInfere,
            })
            .collect();
        let record = ast::Record {
            fields: record_fields,
        };
        self.types_to_promote.push((name.clone(), record));
        ast::fold::fold_struct_literal(self, ast::Path(vec![name]), fields.into_iter().collect())
    }
}
