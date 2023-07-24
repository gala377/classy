use std::collections::HashMap;

use crate::{code::OpCode, ir::Label};

use super::ir::LABEL_BACKPATCH_MASK;

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
                    println!("BACKPATCHING, CURRENT LINE IS {}", index);
                    index += 1;
                    let label_val = u64::from_le_bytes(
                        instrs[index..index + std::mem::size_of::<u64>()]
                            .try_into()
                            .unwrap(),
                    );
                    // TODO:
                    // technicaly not usuful as only jumps forward require backpatching
                    if (LABEL_BACKPATCH_MASK & label_val) > 0 {
                        // requires backpatching
                        let label = Label((label_val & (!LABEL_BACKPATCH_MASK)) as usize);
                        let label_line = match self.labels.get(&label) {
                            Some(line) => *line,
                            None => panic!("Label {:?} not found", label),
                        };
                        println!("Found label {:?} at line {}", label, label_line);
                        let offset = label_line - (index - 1);
                        println!(
                            "The jumpo from {} to {} is {}",
                            index - 1,
                            label_line,
                            offset
                        );
                        instrs[index..index + std::mem::size_of::<u64>()]
                            .copy_from_slice(&offset.to_le_bytes());
                    }
                    index += std::mem::size_of::<u64>();
                }
                i => index += 1 + i.argument_size(),
            }
        }
        instrs
    }
}
