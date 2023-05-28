use crate::{syntax::ast, typecheck::type_context::TypCtx};

pub mod func_to_struct_literal;
pub mod promote_local_types;
pub mod verify_lvalues;
pub mod assign_expr_id;

pub trait AstPass {
    fn run(&mut self, ast: ast::Program) -> ast::Program;
}

pub fn run_befor_type_context_passes(ast: ast::Program) -> ast::Program {
    let ast = verify_lvalues::VerifyLvalues.run(ast);
    let ast = promote_local_types::PromoteAnonTypes::new().run(ast);
    let ast = assign_expr_id::AssignExprId::new().run(ast);
    ast
}

pub fn run_before_typechecking_passes(tctx: &TypCtx, ast: ast::Program) -> ast::Program {
    let ast = func_to_struct_literal::PromoteCallToStructLiteral::new(&tctx).run(ast);
    ast
}
