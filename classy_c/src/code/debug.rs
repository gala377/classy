use colored::Colorize;

use super::{constant_pool, OpCode};

pub fn debug_print_code(instrs: &Vec<u8>, cp: &constant_pool::ConstantPool) {
    const WORD_SIZE: usize = std::mem::size_of::<u64>();

    fn read_word(index: &mut usize, instr: &[u8]) -> usize {
        assert_eq!(instr.len(), WORD_SIZE);
        let mut bytes: [u8; WORD_SIZE] = [0; WORD_SIZE];
        bytes[..WORD_SIZE].copy_from_slice(&instr[..WORD_SIZE]);
        *index += WORD_SIZE;
        usize::from_le_bytes(bytes)
    }

    let mut index = 0;

    macro_rules! read_word {
        () => {{
            let indices = index;
            read_word(&mut index, &instrs[indices..indices + WORD_SIZE])
        }};
    }

    while index < instrs.len() {
        let saved_index = index;
        let instr = OpCode::from(instrs[index]);
        let repr: (&str, Vec<String>) = match instr {
            OpCode::AddInteger => {
                index += 1;
                ("addi", vec![])
            }
            OpCode::EqInteger => {
                index += 1;
                ("eqi", vec![])
            }
            OpCode::AddFloat => {
                index += 1;
                ("addf", vec![])
            }
            OpCode::ConstLoadInteger => {
                index += 1;
                let i = read_word!();
                let value = cp.get::<isize>(i).unwrap();
                ("loadi", vec![value.to_string()])
            }
            OpCode::ConstLoadFloat => {
                index += 1;
                let i = read_word!();
                let value = cp.get::<f64>(i).unwrap();
                ("loadf", vec![value.to_string()])
            }
            OpCode::ConstLoadString => {
                index += 1;
                let i = read_word!();
                let value = cp.get::<String>(i).unwrap();
                ("loads", vec![value])
            }
            OpCode::LookUpGlobal => {
                index += 1;
                let i = read_word!();
                let value = cp.get::<String>(i).unwrap();
                ("global", vec![value])
            }
            OpCode::Return => {
                index += 1;
                ("ret", vec![])
            }
            OpCode::Call0 => {
                index += 1;
                ("call0", vec![])
            }
            OpCode::Call1 => {
                index += 1;
                ("call1", vec![])
            }
            OpCode::CallN => {
                index += 1;
                let i = read_word!();
                ("calln", vec![i.to_string()])
            }
            OpCode::Pop => {
                index += 1;
                ("pop", vec![])
            }
            OpCode::JumpFront => {
                index += 1;
                let i = read_word!();
                ("jump_front", vec![i.to_string()])
            }
            OpCode::JumpBack => {
                index += 1;
                let i = read_word!();
                ("jump_back", vec![i.to_string()])
            }
            OpCode::JumpFrontIfFalse => {
                index += 1;
                let i = read_word!();
                ("cjump_front", vec![i.to_string()])
            }
            OpCode::JumpBackIfFalse => {
                index += 1;
                let i = read_word!();
                ("cjump_back", vec![i.to_string()])
            }

            OpCode::AllocHeap => {
                index += 1;
                let i = read_word!();
                let value = cp.get::<String>(i).unwrap();
                ("alloc", vec![value])
            }
            OpCode::StackCopyBottom => {
                index += 1;
                let i = read_word!();
                ("copy_bottom", vec![i.to_string()])
            }
            OpCode::StackAlloc => {
                index += 1;
                let i = read_word!();
                ("alloc_stack", vec![i.to_string()])
            }
            OpCode::StackAssign => {
                index += 1;
                let i = read_word!();
                ("assign", vec![i.to_string()])
            }
            OpCode::PushTrue => {
                index += 1;
                ("push_true", vec![])
            }
            OpCode::PushFalse => {
                index += 1;
                ("push_false", vec![])
            }
            OpCode::PushUnit => {
                index += 1;
                ("push_unit", vec![])
            }
            OpCode::PushOffsetDeref => {
                index += 1;
                let i = read_word!();
                ("push_offset_deref", vec![i.to_string()])
            }
            OpCode::PushOffsetDerefNegative => {
                index += 1;
                let i = read_word!();
                ("push_offset_deref_neg", vec![i.to_string()])
            }
            OpCode::SetOffsetNegative => {
                index += 1;
                let i = read_word!();
                ("set_offset_neg", vec![i.to_string()])
            }
            OpCode::SetOffset => {
                index += 1;
                let i = read_word!();
                ("set_offset", vec![i.to_string()])
            }
            OpCode::LastMarker => panic!("This instruction should have never been emitted"),
            OpCode::RuntimeCall => {
                index += 1;
                let i = read_word!();
                let value = cp.get::<String>(i).unwrap();
                ("runtime_call", vec![value])
            }
            OpCode::CallNative1 => {
                index += 1;
                ("call_native1", vec![])
            }
            OpCode::CallNative => {
                index += 1;
                let arg = read_word!();
                ("call_native", vec![arg.to_string()])
            }
            OpCode::AllocArray => {
                index += 1;
                let size = read_word!();
                let align = read_word!();
                let is_ref = read_word!();
                (
                    "alloc_array",
                    vec![size.to_string(), align.to_string(), is_ref.to_string()],
                )
            }
        };
        println!(
            "{}| {:20} | {}",
            format!("{:04}", saved_index).dimmed(),
            repr.0.bold(),
            repr.1.join(", ").red()
        );
    }
}
