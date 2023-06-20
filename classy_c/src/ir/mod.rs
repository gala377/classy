use crate::{
    syntax::ast,
    typecheck::{inference::TypeEnv, type_context::TypCtx},
};

pub mod instr;

pub struct Emitter {
    tctx: TypCtx,
    type_env: TypeEnv,
    program: ast::Program,
}

impl Emitter {
    pub fn new(tctx: TypCtx, type_env: TypeEnv, program: ast::Program) -> Self {
        Self {
            tctx,
            type_env,
            program,
        }
    }
}
