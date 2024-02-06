use classy_syntax::ast;

use crate::session::Session;

use super::AstPass;

pub struct ImportPrelude;

impl AstPass for ImportPrelude {
    fn run(&mut self, mut ast: ast::SourceFile, session: &Session) -> ast::SourceFile {
        let types = vec!["String", "Int", "Bool", "Char", "Float", "Byte", "UInt"];
        for t in types {
            ast.items.insert(
                0,
                ast::TopLevelItem {
                    id: 0,
                    export: false,
                    kind: ast::TopLevelItemKind::NameImport(ast::Name::Unresolved {
                        path: vec!["std".to_string(), "core".to_string()],
                        identifier: t.to_string(),
                    }),
                },
            );
        }
        ast
    }
}
