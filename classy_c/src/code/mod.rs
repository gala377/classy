use bitvec::vec::BitVec;

pub mod constant_pool;
pub mod interner;

use constant_pool::ConstantPool;

/// Type erased value.
/// For now we require all values to be at least
/// word sized. Pointers, intergers, floats.
/// Booleans and chars could be extended otherwise
/// we would have to adjust stack to make sure
/// values have proper alignment.
type Word = usize;

/// Bytecode instruction.
///
/// Some instructions can have arguments that follow
/// them directly in the bytecode. (These could be expressed
/// as arguments in the enum variants however, to keep the
/// bytecode size low, this enum only contains the operations's type)
#[derive(Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    AddInteger,
    AddFloat,
    ConstLoadInteger,
    ConstLoadFloat,
    ConstLoadString,
    LookUpGlobal,
    Return,
    Call1,
    Pop,
    LastMarker,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        let last = OpCode::LastMarker as u8;
        if value >= last {
            panic!("Unknown opcode {value}");
        }
        // SAFETY: We checked that the value falls within the opcode range
        unsafe { std::mem::transmute(value) }
    }
}

impl OpCode {
    pub fn argument_size(&self) -> usize {
        match self {
            Self::ConstLoadFloat => 1,
            Self::ConstLoadInteger => 1,
            Self::ConstLoadString => std::mem::size_of::<u64>(),
            Self::LookUpGlobal => std::mem::size_of::<u64>(),
            _ => 0,
        }
    }
}

/// Entry for a gc stack map.
/// Represents stack information about cells holding references
/// to scan.
pub struct GcStackMapEntry {
    // Line starting at which this map entry starts.
    line: usize,
    // True for the stack slot wher the reference lives.
    references: BitVec,
}

impl GcStackMapEntry {
    pub fn line(&self) -> usize {
        self.line
    }
    pub fn reference(&self) -> &BitVec {
        &self.references
    }
}

/// Represents stack information about which cells hold references
/// for each bytecode instruction.
pub type GcStackMap = Vec<GcStackMapEntry>;

/// Bytecode to execute by the virtual machine.
pub struct Code {
    pub instructions: Vec<u8>,
    pub constant_pool: ConstantPool,
    pub stack_map: GcStackMap,
}

impl Code {
    pub fn new() -> Code {
        Code {
            instructions: Vec::new(),
            constant_pool: ConstantPool::new(),
            stack_map: GcStackMap::new(),
        }
    }
}
