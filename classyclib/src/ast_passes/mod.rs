use classy_syntax::ast;

use crate::{session::Session, typecheck::type_context::TypCtx, v2::knowledge::Database};

pub mod assign_ast_ids;
pub mod expand_namespace;
pub mod func_to_struct_literal;
pub mod func_to_struct_literal_db;
pub mod gather_runtime_functions;
pub mod implicit_forall;
pub mod import_prelude;
pub mod move_const_init;
pub mod promote_local_types;
pub mod resolve_names;
pub mod resolve_struct_literal_names;
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
    //let ast = import_prelude::ImportPrelude.run(ast, session);
    let ast = verify_lvalues::VerifyLvalues.run(ast, session);
    let ast = promote_local_types::PromoteAnonTypes::new().run(ast, session);
    let ast = implicit_forall::ImplicitForall::new().run(ast, session);
    let ast = assign_ast_ids::AssignAstIds::new().run(ast, session);

    expand_namespace::ExpandNamespace::new().run(ast, session)
}

pub fn run_after_db_creation_passes(
    ast: ast::SourceFile,
    _db: &Database,
    _session: &Session,
) -> ast::SourceFile {
    // let ast = func_to_struct_literal_db::PromoteCallToStructLiteral::new(db).
    // run(ast, session); let ast = resolve_names::NameResolver::new(db).
    // run(ast, session);
    ast
}

pub fn run_befor_type_context_passes(ast: ast::SourceFile, session: &Session) -> ast::SourceFile {
    let ast = verify_lvalues::VerifyLvalues.run(ast, session);
    let ast = promote_local_types::PromoteAnonTypes::new().run(ast, session);
    let ast = implicit_forall::ImplicitForall::new().run(ast, session);
    let ast = move_const_init::MoveConstInit::new().run(ast, session);

    assign_ast_ids::AssignAstIds::new().run(ast, session)
}

pub fn run_before_typechecking_passes(
    tctx: &TypCtx,
    ast: ast::SourceFile,
    session: &Session,
) -> ast::SourceFile {
    let ast = func_to_struct_literal::PromoteCallToStructLiteral::new(tctx).run(ast, session);
    ast
}
