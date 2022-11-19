use bitvec::vec::BitVec;

pub mod constant_pool;

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
    Return,
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
