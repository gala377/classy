use classy_syntax::ast::{self, fold::Folder, FunctionDefinition};

use crate::session::Session;

use super::AstPass;

pub struct ExpandNamespace {
    namespace: Option<ast::Namespace>,
}

macro_rules! expand_namespace {
    ($self:ident, $on:ident) => {{
        $on.name = $self
            .namespace
            .as_ref()
            .map(|ns| ns.joined_with(&$on.name))
            .unwrap_or($on.name.clone());
        $on
    }};
}

impl ExpandNamespace {
    pub fn new() -> Self {
        Self { namespace: None }
    }
}

impl Folder for ExpandNamespace {
    fn fold_function_definition(&mut self, mut def: FunctionDefinition) -> FunctionDefinition {
        expand_namespace!(self, def)
    }

    fn fold_type_definition(&mut self, mut def: ast::TypeDefinition) -> ast::TypeDefinition {
        expand_namespace!(self, def)
    }

    fn fold_const_definition(&mut self, mut def: ast::ConstDefinition) -> ast::ConstDefinition {
        expand_namespace!(self, def)
    }
}

impl AstPass for ExpandNamespace {
    fn run(&mut self, ast: ast::SourceFile, _: &Session) -> ast::SourceFile {
        self.namespace = ast.namespace.clone();
        self.fold_program(ast)
    }
}
