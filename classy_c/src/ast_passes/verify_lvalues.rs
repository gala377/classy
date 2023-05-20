use crate::syntax::ast::{self, Visitor};

use super::AstPass;
pub struct VerifyLvalues;

impl VerifyLvalues {
    fn verify_lvalues<'ast>(&self, lval: &'ast ast::Expr) {
        match lval {
            ast::Expr::Name(_) => (),
            ast::Expr::Access { .. } => (),
            ast::Expr::TypedExpr { expr, .. } => self.verify_lvalues(expr),
            e => panic!("Cannot assign to {e:?}"),
        };
    }
}

impl<'ast> ast::Visitor<'ast> for VerifyLvalues {
    fn visit_assignment(&mut self, lval: &'ast ast::Expr, rval: &'ast ast::Expr) {
        self.verify_lvalues(lval);
        self.visit_expr(rval);
    }
}

impl AstPass for VerifyLvalues {
    fn run(&mut self, ast: ast::Program) -> ast::Program{
        self.visit(&ast);
        ast
    }
}