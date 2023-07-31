use classy_syntax::ast::{self, Folder};

use crate::{id_provider, session::Session};

use super::AstPass;

pub struct AssignAstIds {
    id_provider: Option<id_provider::IdProvider>,
}

impl AssignAstIds {
    pub fn new() -> Self {
        Self { id_provider: None }
    }
}

impl AstPass for AssignAstIds {
    fn run(&mut self, ast: ast::Program, session: &Session) -> ast::Program {
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
}
