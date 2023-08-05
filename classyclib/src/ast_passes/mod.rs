use classy_syntax::ast;

use crate::{knowledge::Database, session::Session, typecheck::type_context::TypCtx};

pub mod assign_ast_ids;
pub mod expand_namespace;
pub mod func_to_struct_literal;
pub mod gather_runtime_functions;
pub mod implicit_forall;
pub mod move_const_init;
pub mod promote_local_types;
pub mod resolve_names;
pub mod verify_lvalues;

pub trait AstPass {
    fn run(&mut self, ast: ast::SourceFile, session: &Session) -> ast::SourceFile;
}

pub fn fully_expand_names(
    ast: ast::SourceFile,
    session: &Session,
    database: &Database,
) -> ast::SourceFile {
    let ast = resolve_names::NameResolver::new(database).run(ast, session);
    ast
}

pub fn run_after_parsing_passes(ast: ast::SourceFile, session: &Session) -> ast::SourceFile {
    let ast = verify_lvalues::VerifyLvalues.run(ast, session);
    let ast = promote_local_types::PromoteAnonTypes::new().run(ast, session);
    let ast = implicit_forall::ImplicitForall::new().run(ast, session);
    // TODO: Expand imports
    let ast = assign_ast_ids::AssignAstIds::new().run(ast, session);
    let ast = expand_namespace::ExpandNamespace::new().run(ast, session);
    ast
}

pub fn run_befor_type_context_passes(ast: ast::SourceFile, session: &Session) -> ast::SourceFile {
    let ast = verify_lvalues::VerifyLvalues.run(ast, session);
    let ast = promote_local_types::PromoteAnonTypes::new().run(ast, session);
    let ast = implicit_forall::ImplicitForall::new().run(ast, session);
    let ast = move_const_init::MoveConstInit::new().run(ast, session);
    let ast = assign_ast_ids::AssignAstIds::new().run(ast, session);
    ast
}

pub fn run_before_typechecking_passes(
    tctx: &TypCtx,
    ast: ast::SourceFile,
    session: &Session,
) -> ast::SourceFile {
    let ast = func_to_struct_literal::PromoteCallToStructLiteral::new(tctx).run(ast, session);
    ast
}
