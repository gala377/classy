use std::{collections::HashMap, mem::size_of};

use crate::{
    code::{
        constant_pool::{self, ConstantPool},
        Code, GcStackMapEntry, OpCode,
    },
    ir::{self, emitter::IrFunction, instr::IsRef, Label},
    typecheck::type_context::TypCtx,
};

pub const LABEL_BACKPATCH_MASK: u64 = 0xFF00000000000000;

pub struct FunctionEmitter<'ctx, 'pool> {
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

    type_ctx: &'ctx TypCtx,
    constant_pool: &'pool mut ConstantPool,
}

impl<'ctx, 'pool> FunctionEmitter<'ctx, 'pool> {
    pub fn new(type_ctx: &'ctx TypCtx, constant_pool: &'pool mut ConstantPool) -> Self {
        Self {
            current_line: 0,
            stack_map: Vec::new(),
            args_len: 0,
            locals_stack_depth: 0,
            label_lines: HashMap::new(),
            type_ctx,
            constant_pool,
        }
    }

    pub fn labels(&self) -> &HashMap<Label, usize> {
        &self.label_lines
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
        self.emit_instr(&mut code, OpCode::StackAlloc);
        // No need to allocate space for function args as those are allocated
        // from by the caller
        self.emit_word(&mut code, (self.locals_stack_depth) as u64);
        let mut index = 0;
        while index < body.len() {
            let op = body[index].clone();
            // TODO: remove later
            let debug_op = op.clone();
            match op {
                ir::Instruction::BinOpAssign(_, _, _, _) => todo!(),
                ir::Instruction::UnOpAssing(_, _, _) => todo!(),
                ir::Instruction::CopyAssign(a1, a2) => {
                    self.push_address(&mut code, a2);
                    self.set_address(&mut code, a1)
                        .map_err(|e| format!("line {index}, {debug_op:?} => {e}"))
                        .unwrap();
                }
                ir::Instruction::IndexCopy { res, base, offset } => {
                    self.push_address(&mut code, base);
                    // There is no need to pop or push anything to the gc map
                    // as the set_address will pop the gc map already.
                    // and no allocation can happen between the push and pop.
                    self.emit_instr(&mut code, OpCode::PushOffsetDeref);
                    self.emit_word(
                        &mut code,
                        match offset {
                            ir::Address::ConstantInt(val) => val as u64,
                            _ => todo!(),
                        },
                    );
                    self.set_address(&mut code, res)
                        .map_err(|e| format!("line {index}, {debug_op:?} => {e}"))
                        .unwrap();
                }
                ir::Instruction::IndexSet {
                    base,
                    offset,
                    value,
                } => {
                    self.push_address(&mut code, base);
                    self.push_address(&mut code, value);
                    self.emit_instr(&mut code, OpCode::SetOffset);
                    self.emit_word(
                        &mut code,
                        match offset {
                            ir::Address::ConstantInt(val) => val as u64,
                            _ => todo!(),
                        },
                    );
                    self.stack_map_pop();
                    self.stack_map_pop();
                }
                ir::Instruction::GoTo(l) => match self.label_lines.get(&l) {
                    Some(line) => {
                        let offset = self.current_line - line;
                        self.emit_instr(&mut code, OpCode::JumpBack);
                        self.emit_word(&mut code, offset as u64);
                    }
                    None => {
                        self.emit_instr(&mut code, OpCode::JumpFront);
                        // Later in backpatching we need to check FLAG & VALUE == FLAG
                        // to know if this requires backpatching
                        self.emit_word(&mut code, LABEL_BACKPATCH_MASK | l.0 as u64);
                    }
                },
                ir::Instruction::If { .. } => {
                    todo!()
                }
                ir::Instruction::IfFalse { cond, goto } => {
                    self.push_address(&mut code, cond);
                    match self.label_lines.get(&goto) {
                        Some(line) => {
                            let offset = self.current_line - line + size_of::<u64>() + 1;
                            self.emit_instr(&mut code, OpCode::JumpBackIfFalse);
                            self.emit_word(&mut code, offset as u64);
                        }
                        None => {
                            self.emit_instr(&mut code, OpCode::JumpFrontIfFalse);
                            // Later in backpatching we need to check FLAG & VALUE == FLAG
                            // to know if this requires backpatching
                            self.emit_word(&mut code, LABEL_BACKPATCH_MASK | goto.0 as u64);
                        }
                    }
                    // no more bool on the top of the stack
                    self.stack_map_pop();
                }
                ir::Instruction::Param(val) => {
                    let mut params = vec![val];
                    index += 1;
                    while let ir::Instruction::Param(val) = &body[index] {
                        params.push(val.clone());
                        index += 1;
                    }
                    let ir::Instruction::Call { res, func, argc } = &body[index] else {
                        panic!("Expected call instruction");
                    };
                    assert_eq!(params.len(), *argc);
                    match argc {
                        1 => {
                            self.push_address(&mut code, params[0].clone());
                            self.push_address(&mut code, func.clone());
                            // We export stack map after we push the function so the code
                            // needs to remember that in case of the gc it needs to push
                            // the values popped again onto the stack in the reverse order
                            // of popping them
                            self.export_stack_map(&mut code);
                            self.emit_instr(&mut code, OpCode::Call1);
                            // we have a stack of [param, func]
                            // then we have a stack of [res]
                            // and then set address pops this and pops the stack map
                            // so we need to pop stack_map once to preserve the stack shape
                            self.stack_map_pop();
                            self.set_address(&mut code, res.clone())
                                .map_err(|e| format!("line {index}, {debug_op:?} => {e}"))
                                .unwrap();
                        }
                        n => {
                            for param in &params {
                                self.push_address(&mut code, param.clone());
                            }
                            self.push_address(&mut code, func.clone());
                            // We export stack map after we push the function so the code
                            // needs to remember that in case of the gc it needs to push
                            // the values popped again onto the stack in the reverse order
                            // of popping them
                            self.export_stack_map(&mut code);
                            self.emit_instr(&mut code, OpCode::CallN);
                            self.emit_word(&mut code, *n as u64);
                            for _ in &params {
                                self.stack_map_pop();
                            }
                            self.set_address(&mut code, res.clone())
                                .map_err(|e| format!("line {index}, {debug_op:?} => {e}"))
                                .unwrap();
                        }
                    }
                }
                ir::Instruction::Call { res, func, argc } => match argc {
                    0 => {
                        self.push_address(&mut code, func);
                        // We export stack map after we push the function so the code
                        // needs to remember that in case of the gc it needs to push
                        // the values popped again onto the stack
                        self.export_stack_map(&mut code);
                        self.emit_instr(&mut code, OpCode::Call0);
                        self.set_address(&mut code, res)
                            .map_err(|e| format!("line {index}, {debug_op:?} => {e}"))
                            .unwrap();
                    }
                    _ => panic!("Other should not be possible"),
                },
                ir::Instruction::Label(i) => {
                    self.label_lines.insert(Label(i), self.current_line);
                }
                ir::Instruction::Alloc { res, typ, .. } => {
                    self.export_stack_map(&mut code);
                    self.emit_instr(&mut code, OpCode::AllocHeap);
                    let name = self.type_ctx.get_name(typ).unwrap();
                    let id = self.constant_pool.add_entry(name.into());
                    self.emit_word(&mut code, id as u64);
                    self.stack_map_add_ref();
                    self.set_address(&mut code, res)
                        .map_err(|e| format!("line {index}, {debug_op:?} => {e}"))
                        .unwrap();
                }
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
                let id = self
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
                let id = self.constant_pool.add_entry(val.into());
                self.emit_instr(code, OpCode::ConstLoadInteger);
                self.emit_word(code, id as u64);
                self.stack_map_add_value();
            }
            ir::Address::ConstantFloat(val) => {
                let id = self.constant_pool.add_entry(val.into());
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
                self.stack_map_add_value();
            }
            ir::Address::ConstantString(val) => {
                let id = self
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

    fn set_address(&mut self, code: &mut Code, addr: ir::Address) -> Result<(), String> {
        match addr {
            ir::Address::Temporary(i, _) => {
                self.emit_instr(code, OpCode::StackAssign);
                self.emit_word(code, (i + self.args_len) as u64);
                self.stack_map_pop();
                Ok(())
            }
            ir::Address::Parameter(i) => {
                self.emit_instr(code, OpCode::StackAssign);
                self.emit_word(code, i as u64);
                self.stack_map_pop();
                Ok(())
            }
            ir::Address::Name(_) => todo!("Global values not assignable yet"),
            addr => Err(format!("{addr:?} is not an assignable address")),
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

    fn export_stack_map(&self, code: &mut Code) {
        let entry = GcStackMapEntry {
            line: self.current_line,
            references: self.stack_map.iter().collect(),
        };
        println!("Exporting stack map {:?}", entry);
        code.stack_map.push(entry);
    }
}
