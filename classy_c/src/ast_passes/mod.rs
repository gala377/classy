use crate::syntax::ast;

pub mod func_to_struct_literal;
pub mod verify_lvalues;
pub mod promote_local_types;

pub trait AstPass {
    fn run(&mut self, ast: ast::Program) -> ast::Program;
}
