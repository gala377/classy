use crate::{syntax::ast, typecheck::type_context::TypCtx};

pub mod assign_expr_id;
pub mod func_to_struct_literal;
pub mod gather_runtime_functions;
pub mod implicit_forall;
pub mod move_const_init;
pub mod order_functions;
pub mod promote_local_types;
pub mod verify_lvalues;

pub trait AstPass {
    fn run(&mut self, ast: ast::Program) -> ast::Program;
}

pub fn run_befor_type_context_passes(ast: ast::Program) -> ast::Program {
    let ast = verify_lvalues::VerifyLvalues.run(ast);
    let ast = promote_local_types::PromoteAnonTypes::new().run(ast);
    let ast = implicit_forall::ImplicitForall::new().run(ast);
    let ast = move_const_init::MoveConstInit::new().run(ast);
    assign_expr_id::AssignExprId::new().run(ast)
}

pub fn run_before_typechecking_passes(tctx: &TypCtx, ast: ast::Program) -> ast::Program {
    let ast = func_to_struct_literal::PromoteCallToStructLiteral::new(tctx).run(ast);
    ast
}