use crate::syntax::ast::{self, Folder};

use super::AstPass;

pub struct AssignExprId {
    id: usize,
}

impl Default for AssignExprId {
    fn default() -> Self {
        Self::new()
    }
}

impl AssignExprId {
    pub fn new() -> Self {
        Self { id: 1 }
    }
}

impl AstPass for AssignExprId {
    fn run(&mut self, ast: ast::Program) -> ast::Program {
        self.fold_program(ast)
    }
}

impl Folder for AssignExprId {
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
}
