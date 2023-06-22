pub mod emitter;
pub mod instr;

pub use emitter::FunctionEmitter as Emitter;
pub use instr::{Address, Block, Instruction, Label, Op};
