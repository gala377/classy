use std::collections::HashMap;

use crate::{code::OpCode, ir::Label};

pub struct Backpatcher {
    labels: HashMap<Label, usize>,
}

impl Backpatcher {
    pub fn new(labels: HashMap<Label, usize>) -> Self {
        Self { labels }
    }

    pub fn backpatch_code(&mut self, mut instrs: Vec<u8>) -> Vec<u8> {
        let mut index = 0;
        while index < instrs.len() {
            let instr = OpCode::from(instrs[index]);
            match instr {
                OpCode::JumpFront | OpCode::JumpFrontIfFalse => {
                    index += 1;
                    let label_val = usize::from_le_bytes(
                        instrs[index..index + std::mem::size_of::<u64>()]
                            .try_into()
                            .unwrap(),
                    );
                    let label = Label(label_val);

                    let label_line = self.labels[&label];
                    let offset = label_line - index - 1;
                    instrs[index..index + std::mem::size_of::<u64>()]
                        .copy_from_slice(&offset.to_le_bytes());
                    index += std::mem::size_of::<u64>();
                }
                i => index += i.argument_size(),
            }
            index += 1;
        }
        instrs
    }
}
