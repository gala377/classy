use crate::syntax::ast;

pub mod func_to_struct_literal;

pub trait AstPass {
    fn run(&mut self, ast: ast::Program) -> ast::Program;
}
