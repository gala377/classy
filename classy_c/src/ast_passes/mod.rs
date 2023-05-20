use crate::syntax::ast;

pub mod func_to_struct_literal;
pub mod promote_local_types;
pub mod verify_lvalues;

pub trait AstPass {
    fn run(&mut self, ast: ast::Program) -> ast::Program;
}
