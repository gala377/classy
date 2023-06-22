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
    /// Add two values from the top of the stack 
    /// and push the result on top of the stack.
    AddInteger,
    AddFloat,
    /// Load constant with the index following this instruction
    /// as a word. (Meaning push it on top of the stack) 
    /// 
    /// Args:
    ///  - index: Word
    ConstLoadInteger,
    ConstLoadFloat,
    ConstLoadString,
    /// Lookup value of a global variable with the name
    /// being a symbol pointed by the following word.
    /// 
    /// Args:
    ///  - class_name: Symbol
    LookUpGlobal,
    /// Return from the function with the value on top of the stack.
    Return,
    /// Call function with no arguments. The function is on top of the stack.
    Call0,
    /// Call function with one argument that is on top of the stack. And the function is below it.
    Call1,
    /// Call function on the top of the stack with "N"
    /// arguments where "n" is stored as a single byte after the
    /// instruction. "N" arguments are on the stack.
    /// 
    /// Args:
    /// - number_of_args: Word
    CallN,
    /// Pop value from the top of the stack
    Pop,

    /// Increase the instruction pointer by the amount given as a word
    /// following this instruction
    /// 
    /// Args:
    /// - offset: Word
    JumpFront,
    /// Decrement the instruction pointer by the amout given as a word
    /// following this instruction
    /// 
    /// Args:
    /// - offset: Word
    JumpBack,
    /// Jump front but only if the top of the stack is false
    /// 
    /// Args:
    /// - offset: Word
    JumpFrontIfFalse,
    /// Jump back but only if the top of the stack is false
    /// 
    /// Args:
    /// - offset: Word
    JumpBackIfFlse,


    /// Allocate an instance of a class that has a name followed
    /// as an symbol index to a constant pool followed by this instruction
    /// 
    /// Args:
    ///  - class_name: Symbol
    AllocHeap,  

    /// Copy a value from the bottom of the stack that is indexed by the word
    /// following this instruction and push it to the top of the stack
    /// 
    /// Args:
    ///   - offset: Word  
    StackCopyBottom,

    /// Allocate space on a stack for a number of words
    /// that is specified as a word following this instruction.
    /// 
    /// Args:
    /// - offset: Word
    StackAlloc,
    /// do stack[bottom + offset] = pop 
    /// where the offset is followed as word after this instruction.
    /// 
    /// Args:
    ///  - offset: Word
    StackAssign,

    /// Push true or false on top of the stack
    PushTrue,
    PushFalse,
    /// Push pointer to unit on the top of the stack
    PushUnit,

    /// Take the pointer from the top of the stack offset it and derefernce it.
    /// Put this value on top of the stack. The offset is following this instruction
    /// as a word.
    /// 
    /// Args: 
    ///  - offset: Word
    PushOffsetDeref,

    /// Used as the marker for the last instruction
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
