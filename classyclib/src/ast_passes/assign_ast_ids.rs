use classy_syntax::ast::{self, Folder};

use super::AstPass;

pub struct AssignAstIds {
    id: usize,
}

impl Default for AssignAstIds {
    fn default() -> Self {
        Self::new()
    }
}

impl AssignAstIds {
    pub fn new() -> Self {
        Self { id: 1 }
    }
}

impl AstPass for AssignAstIds {
    fn run(&mut self, ast: ast::Program) -> ast::Program {
        self.fold_program(ast)
    }
}

impl Folder for AssignAstIds {
    fn fold_expr(&mut self, expr: ast::Expr) -> ast::Expr {
        let mut expr = ast::fold::fold_expr(self, expr);
        expr.id = self.id;
        self.id += 1;
        expr
    }

    fn fold_pattern(&mut self, pat: ast::Pattern) -> ast::Pattern {
        let mut pat = ast::fold::fold_pattern(self, pat);
        pat.id = self.id;
        self.id += 1;
        pat
    }

    fn fold_top_level_item(&mut self, item: ast::TopLevelItem) -> ast::TopLevelItem {
        let mut item = ast::fold::fold_top_level_item(self, item);
        item.id = self.id;
        self.id += 1;
        item
    }
}
