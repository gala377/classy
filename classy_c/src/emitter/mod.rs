pub mod ast;
pub mod ir;
pub mod backpatch;

use crate::{ir::emitter::IrFunction, code::Code, typecheck::type_context::TypCtx};

pub fn compile_ir_function(func: &IrFunction, typ_ctx: &TypCtx) -> Code {
    let mut emitter = ir::FunctionEmitter::new(typ_ctx);
    let mut code = emitter.emit_fn(func);
    let labels = emitter.labels().clone();
    let mut backpatcher = backpatch::Backpatcher::new(labels);
    code.instructions = backpatcher.backpatch_code(code.instructions);
    code
}