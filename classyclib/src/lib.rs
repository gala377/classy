#![feature(box_patterns)]

pub mod ast_passes;
pub mod code;
pub mod discard_values;
pub mod emitter;
pub mod id_provider;
pub mod ir;
pub mod knowledge;
pub mod package;
pub mod scope;
pub mod session;
pub mod typecheck;
