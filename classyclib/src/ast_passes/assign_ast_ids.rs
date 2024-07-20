use classy_syntax::ast::{self, Folder};

use crate::session::{Session, SharedIdProvider};

use super::AstPass;

pub struct AssignAstIds {
    id_provider: Option<SharedIdProvider>,
}

impl Default for AssignAstIds {
    fn default() -> Self {
        Self::new()
    }
}

impl AssignAstIds {
    pub fn new() -> Self {
        Self { id_provider: None }
    }
}

impl AstPass for AssignAstIds {
    fn run(&mut self, ast: ast::SourceFile, session: &Session) -> ast::SourceFile {
        self.id_provider = Some(session.id_provider());
        self.fold_program(ast)
    }
}

impl Folder for AssignAstIds {
    fn fold_expr(&mut self, expr: ast::Expr) -> ast::Expr {
        let mut expr = ast::fold::fold_expr(self, expr);
        expr.id = self.id_provider.as_ref().unwrap().next();
        expr
    }

    fn fold_pattern(&mut self, pat: ast::Pattern) -> ast::Pattern {
        let mut pat = ast::fold::fold_pattern(self, pat);
        pat.id = self.id_provider.as_ref().unwrap().next();
        pat
    }

    fn fold_top_level_item(&mut self, item: ast::TopLevelItem) -> ast::TopLevelItem {
        let mut item = ast::fold::fold_top_level_item(self, item);
        item.id = self.id_provider.as_ref().unwrap().next();
        item
    }

    fn fold_method_definition(
        &mut self,
        def: ast::Method<ast::FunctionDefinition>,
    ) -> ast::Method<ast::FunctionDefinition> {
        let mut def = ast::fold::fold_method_definition(self, def);
        def.id = self.id_provider.as_ref().unwrap().next();
        def
    }

    fn fold_class_methods_block_method(
        &mut self,
        method: ast::Method<ast::FuncDecl>,
    ) -> ast::Method<ast::FuncDecl> {
        let mut method = ast::fold::fold_class_methods_block_method(self, method);
        method.id = self.id_provider.as_ref().unwrap().next();
        method
    }
}
