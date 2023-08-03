use std::collections::HashMap;

use classy_syntax::ast::{self, Folder, Visitor};

use crate::session::Session;

use super::AstPass;

pub struct MoveConstInit {
    const_vars: HashMap<String, ast::Expr>,
}

impl MoveConstInit {
    pub fn new() -> Self {
        Self {
            const_vars: HashMap::new(),
        }
    }
}

impl AstPass for MoveConstInit {
    fn run(&mut self, ast: ast::SourceFile, _: &Session) -> ast::SourceFile {
        self.visit(&ast);
        self.fold_program(ast)
    }
}

impl<'ast> ast::visitor::Visitor<'ast> for MoveConstInit {
    fn visit_const_definition(&mut self, def: &'ast ast::ConstDefinition) {
        self.const_vars.insert(def.name.clone(), def.init.clone());
    }
}

impl ast::fold::Folder for MoveConstInit {
    fn fold_function_definition(
        &mut self,
        def: ast::FunctionDefinition,
    ) -> ast::FunctionDefinition {
        if def.name != "main" {
            return def;
        }
        let id = def.body.id;
        let mut inits: Vec<_> = self
            .const_vars
            .iter()
            .map(|(name, init)| ast::Expr {
                id: init.id,
                kind: ast::ExprKind::Assignment {
                    lval: Box::new(ast::Expr {
                        id: init.id,
                        kind: ast::ExprKind::Name(ast::Name::Unresolved {
                            path: vec![],
                            identifier: name.clone(),
                        }),
                    }),
                    rval: Box::new(init.clone()),
                },
            })
            .collect();
        inits.push(def.body);
        let new_body = ast::Expr {
            id,
            kind: ast::ExprKind::Sequence(inits),
        };
        let new_def = ast::FunctionDefinition {
            body: new_body,
            ..def
        };
        new_def
    }
}
