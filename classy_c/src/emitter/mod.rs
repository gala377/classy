pub mod backpatch;
pub mod ir;

use crate::{
    code::{constant_pool::ConstantPool, Code},
    ir::emitter::IrFunction,
    typecheck::type_context::TypCtx,
};

pub fn compile_ir_function(
    func: &IrFunction,
    typ_ctx: &TypCtx,
    constant_pool: &mut ConstantPool,
) -> Code {
    let mut emitter = ir::FunctionEmitter::new(typ_ctx, constant_pool);
    let mut code = emitter.emit_fn(func);
    let labels = emitter.labels().clone();
    let mut backpatcher = backpatch::Backpatcher::new(labels);
    code.instructions = backpatcher.backpatch_code(code.instructions);
    code
}
