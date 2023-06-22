use std::{mem::size_of, collections::HashMap, hash::Hash};

use crate::{
    code::{constant_pool, Code, OpCode},
    ir::{self, emitter::IrFunction, instr::IsRef, Label},
};

const LABEL_BACKPATCH: u64 = std::u64::MAX;

pub struct FunctionEmitter {
    current_line: usize,
    stack_map: Vec<bool>,

    /// Every arg is available at the stack (from the bottom)
    /// at stack[arg]
    args_len: usize,
    /// Every temporary is accesible at the stack
    /// (from the bottom) at stack[args + temp]
    locals_stack_depth: usize,
    /// Maps label to the line number its pointing to.
    /// This is later used for backpatching.
    label_lines: HashMap<Label, usize>,
}

impl FunctionEmitter {
    pub fn new() -> Self {
        Self {
            current_line: 0,
            stack_map: Vec::new(),
            args_len: 0,
            locals_stack_depth: 0,
            label_lines: HashMap::new(),
        }
    }

    fn prepare_locals(&mut self, func: &IrFunction) {
        // setup arguments
        let args = &func.args;
        self.args_len = args.len();

        for (_, is_ref) in args {
            match is_ref {
                IsRef::Ref => self.stack_map_add_ref(),
                IsRef::NoRef => self.stack_map_add_value(),
            }
        }

        // setup locals
        fn add_temp(v: &mut Vec<(usize, IsRef)>, addr: &ir::Address) {
            if let ir::Address::Temporary(i, f) = addr {
                v.push((*i, f.clone()));
            }
        }

        let tmps: &mut Vec<(usize, IsRef)> = &mut Vec::new();
        for instr in &func.body {
            match instr {
                ir::Instruction::BinOpAssign(a1, a2, _, a3) => {
                    add_temp(tmps, a1);
                    add_temp(tmps, a2);
                    add_temp(tmps, a3);
                }
                ir::Instruction::UnOpAssing(a1, _, a2) => {
                    add_temp(tmps, a1);
                    add_temp(tmps, a2);
                }
                ir::Instruction::CopyAssign(a1, a2) => {
                    add_temp(tmps, a1);
                    add_temp(tmps, a2);
                }
                ir::Instruction::IndexCopy { res, base, offset } => {
                    add_temp(tmps, res);
                    add_temp(tmps, base);
                    add_temp(tmps, offset);
                }
                ir::Instruction::IndexSet {
                    base,
                    offset,
                    value,
                } => {
                    add_temp(tmps, value);
                    add_temp(tmps, base);
                    add_temp(tmps, offset);
                }
                ir::Instruction::GoTo(_) => {}
                ir::Instruction::If { cond, .. } => add_temp(tmps, cond),
                ir::Instruction::IfFalse { cond, .. } => add_temp(tmps, cond),
                ir::Instruction::Param(_) => {}
                ir::Instruction::Call { res, func, .. } => {
                    add_temp(tmps, res);
                    add_temp(tmps, func);
                }
                ir::Instruction::Label(_) => {}
                ir::Instruction::Alloc { res, .. } => {
                    add_temp(tmps, res);
                }
                ir::Instruction::AllocArray { res, .. } => {
                    add_temp(tmps, res);
                }
                ir::Instruction::Return(v) => {
                    add_temp(tmps, v);
                }
            }
        }

        tmps.dedup_by(|(x1, _), (x2, _)| x1 == x2);
        tmps.sort_by(|(x1, _), (x2, _)| x1.cmp(x2));

        self.locals_stack_depth = tmps.len();
        for (_, is_ref) in tmps {
            match is_ref {
                IsRef::Ref => self.stack_map_add_ref(),
                IsRef::NoRef => self.stack_map_add_value(),
            }
        }
    }

    pub fn emit_fn(&mut self, func: &IrFunction) -> Code {
        let body = &func.body;
        self.prepare_locals(func);

        let mut code = Code::new();
        let mut index = 0;
        while index < body.len() {
            let op = body[index].clone();
            match op {
                ir::Instruction::BinOpAssign(_, _, _, _) => todo!(),
                ir::Instruction::UnOpAssing(_, _, _) => todo!(),
                ir::Instruction::CopyAssign(_, _) => todo!(),
                ir::Instruction::IndexCopy { res, base, offset } => todo!(),
                ir::Instruction::IndexSet {
                    base,
                    offset,
                    value,
                } => todo!(),
                ir::Instruction::GoTo(l) => {
                    match self.label_lines.get(&l) {
                        Some(line) => {
                            let offset = self.current_line - line;
                            self.emit_instr(&mut code, OpCode::JumpBack);
                            self.emit_word(&mut code, offset as u64);
                        },
                        None => {
                            self.emit_instr(&mut code, OpCode::JumpFront);
                            self.emit_word(&mut code, LABEL_BACKPATCH);
                        },
                    }
                },
                ir::Instruction::If { cond, goto } => todo!(),
                ir::Instruction::IfFalse { cond, goto } => todo!(),
                ir::Instruction::Param(_) => todo!(),
                ir::Instruction::Call { res, func, argc } => match argc {
                    0 => todo!(),
                    1 => todo!(),
                    n => todo!(),
                },
                ir::Instruction::Label(_) => todo!(),
                ir::Instruction::Alloc { res, size, typ } => todo!(),
                ir::Instruction::AllocArray { .. } => todo!(),
                ir::Instruction::Return(v) => {
                    self.push_address(&mut code, v);
                    self.emit_instr(&mut code, OpCode::Return);
                }
            }
            index += 1;
        }
        code
    }

    fn push_address(&mut self, code: &mut Code, addr: ir::Address) {
        match addr {
            ir::Address::Temporary(i, _) => {
                self.emit_instr(code, OpCode::StackCopyBottom);
                self.emit_word(code, (i + self.args_len) as u64);
                self.stack_map_add(self.stack_map[i + self.args_len]);
            }
            ir::Address::Name(name) => {
                let id = code
                    .constant_pool
                    .add_entry(constant_pool::TypedEntry::String(name.clone()));
                self.emit_instr(code, OpCode::LookUpGlobal);
                self.emit_word(code, id as u64);
                // Global values are global so they are added to roots either way.
                // There is no point scanning them again.
                self.stack_map_add_value();
            }
            ir::Address::ConstantInt(val) => {
                // TODO:
                // This and constant float should use something
                // like pushfloat and pushint so we can encode
                // integer directly in the source code.
                let id = code.constant_pool.add_entry(val.into());
                self.emit_instr(code, OpCode::ConstLoadInteger);
                self.emit_word(code, id as u64);
                self.stack_map_add_value();
            }
            ir::Address::ConstantFloat(val) => {
                let id = code.constant_pool.add_entry(val.into());
                self.emit_instr(code, OpCode::ConstLoadFloat);
                self.emit_word(code, id as u64);
                self.stack_map_add_value();
            }
            ir::Address::ConstantBool(val) => {
                self.emit_instr(
                    code,
                    if val {
                        OpCode::PushTrue
                    } else {
                        OpCode::PushFalse
                    },
                );
            }
            ir::Address::ConstantString(val) => {
                let id = code
                    .constant_pool
                    .add_entry(constant_pool::TypedEntry::String(val.clone()));
                self.emit_instr(code, OpCode::ConstLoadString);
                self.emit_word(code, id as u64);
                // The string is static so it's going to be scanned anyway.
                self.stack_map_add_value();
            }
            ir::Address::ConstantUnit => {
                self.emit_instr(code, OpCode::PushUnit);
                self.stack_map_add_value();
            }
            ir::Address::Parameter(i) => {
                self.emit_instr(code, OpCode::StackCopyBottom);
                self.emit_word(code, i as u64);
                self.stack_map_add(self.stack_map[i]);
            }
        }
    }

    fn emit_instr(&mut self, code: &mut Code, op: OpCode) {
        code.instructions.push(op as u8);
        self.current_line += 1;
    }

    fn emit_word(&mut self, code: &mut Code, val: u64) {
        // saves as a low endian representaiton
        code.instructions.extend_from_slice(&val.to_le_bytes());
        self.current_line += size_of::<u64>();
    }

    fn stack_map_add_ref(&mut self) {
        self.stack_map.push(true);
    }

    fn stack_map_add_value(&mut self) {
        self.stack_map.push(false);
    }

    fn stack_map_add(&mut self, is_ref: bool) {
        self.stack_map.push(is_ref)
    }

    fn stack_map_pop(&mut self) {
        self.stack_map.pop();
    }
}
