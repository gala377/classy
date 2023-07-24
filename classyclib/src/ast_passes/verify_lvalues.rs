use classy_syntax::ast::{self, Visitor};

use super::AstPass;
pub struct VerifyLvalues;

impl VerifyLvalues {
    fn verify_lvalues(lval: &ast::ExprKind) {
        match lval {
            ast::ExprKind::Name(_) => (),
            ast::ExprKind::Access { .. } => (),
            ast::ExprKind::TypedExpr { expr, .. } => Self::verify_lvalues(&expr.kind),
            ast::ExprKind::IndexAccess { .. } => (),
            e => panic!("Cannot assign to {e:?}"),
        };
    }
}

impl<'ast> ast::Visitor<'ast> for VerifyLvalues {
    fn visit_assignment(&mut self, lval: &'ast ast::Expr, rval: &'ast ast::Expr) {
        Self::verify_lvalues(&lval.kind);
        self.visit_expr(rval);
    }
}

impl AstPass for VerifyLvalues {
    fn run(&mut self, ast: ast::Program) -> ast::Program {
        self.visit(&ast);
        ast
    }
}
