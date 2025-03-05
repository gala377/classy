use classy_syntax::ast::{self, Folder};

use crate::v2::{
    compile::CompilationError,
    knowledge::{Database, DatabaseFolder, DefinitionId, Id, LocalId, PackageId},
};

use super::AstPass;

/// TODO: We should have a scope there
/// in case someone uses variables with the same name as the types
pub struct ResolveStructLiteralNames<'ctx> {
    db: &'ctx Database,
    namespace: Vec<String>,
    errors: Vec<CompilationError>,
}

impl AstPass for ResolveStructLiteralNames<'_> {
    fn run(&mut self, ast: ast::SourceFile, _: &crate::session::Session) -> ast::SourceFile {
        self.fold_program(ast)
    }
}

impl<'db> ResolveStructLiteralNames<'db> {
    pub fn new(db: &'db Database) -> Self {
        Self {
            db,
            namespace: Default::default(),
            errors: Default::default(),
        }
    }

    pub fn error(&mut self, msg: impl Into<String>) {
        self.errors.push(CompilationError::semantic_error(msg));
    }

    fn resolve_name(&self, name: &ast::Name) -> Option<Id<DefinitionId>> {
        match name {
            ast::Name::Local(_) => None,
            ast::Name::Unresolved { path, identifier } => self
                .db
                .get_definition_id_by_unresolved_name(&self.namespace, path, identifier),
            ast::Name::Global {
                package,
                definition,
            } => Some(Id {
                package: PackageId(*package),
                id: DefinitionId(*definition),
            }),
        }
    }
}

impl DatabaseFolder for ResolveStructLiteralNames<'_> {
    fn process_definition(&mut self, id: LocalId<DefinitionId>) {
        let namespace = self.db.get_namespace(id);
        self.namespace = namespace.into();
    }
}

impl Folder for ResolveStructLiteralNames<'_> {
    fn fold_struct_literal(
        &mut self,
        strct: ast::Name,
        values: std::collections::HashMap<String, ast::Expr>,
    ) -> ast::ExprKind {
        let id = self
            .resolve_name(&strct)
            .expect("Could not resolve struct name");
        let (package, definition) = (id.package, id.id);
        ast::ExprKind::StructLiteral {
            strct: ast::Name::Global {
                package: package.0,
                definition: definition.0,
            },
            values: values
                .into_iter()
                .map(|(k, v)| (k, self.fold_expr(v)))
                .collect(),
        }
    }
}
