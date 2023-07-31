use classy_syntax::ast;

use crate::{session::Session, typecheck::type_context::TypCtx};

pub mod assign_ast_ids;
pub mod func_to_struct_literal;
pub mod gather_runtime_functions;
pub mod implicit_forall;
pub mod move_const_init;
pub mod order_functions;
pub mod promote_local_types;
pub mod verify_lvalues;

pub trait AstPass {
    fn run(&mut self, ast: ast::Program, session: &Session) -> ast::Program;
}

pub fn run_befor_type_context_passes(ast: ast::Program, session: &Session) -> ast::Program {
    let ast = verify_lvalues::VerifyLvalues.run(ast, session);
    let ast = promote_local_types::PromoteAnonTypes::new().run(ast, session);
    let ast = implicit_forall::ImplicitForall::new().run(ast, session);
    let ast = move_const_init::MoveConstInit::new().run(ast, session);
    let ast = assign_ast_ids::AssignAstIds::new().run(ast, session);
    ast
}

pub fn run_before_typechecking_passes(
    tctx: &TypCtx,
    ast: ast::Program,
    session: &Session,
) -> ast::Program {
    let ast = func_to_struct_literal::PromoteCallToStructLiteral::new(tctx).run(ast, session);
    ast
}
