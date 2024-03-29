use bitvec::vec::BitVec;

pub mod constant_pool;
pub mod debug;
pub mod interner;

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
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum OpCode {
    /// Add two values from the top of the stack
    /// and push the result on top of the stack.
    AddInteger,
    AddFloat,

    /// Check if 2 integers on top of the stack are equal.
    EqInteger,
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
    /// Call function with one argument that is on top of the stack. And the
    /// function is below it.
    Call1,
    /// Call function on the top of the stack with "N"
    /// arguments where "n" is stored as a word the
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
    JumpBackIfFalse,

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
    /// - num_of_words: Word
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
    /// Put this value on top of the stack. The offset is following this
    /// instruction as a word.
    ///
    /// Args:
    ///  - offset: Word
    PushOffsetDeref,
    /// Same as push offset deref but the offset is read as negative.
    PushOffsetDerefNegative,
    /// Pop value from the top of the stack and also pop an address.
    /// Then store the value at the address shifted by the offset.
    ///
    /// Args:
    ///  - offset: Word
    SetOffset,
    /// Same as set offset but the offset is read as negative.
    SetOffsetNegative,
    /// Hach used to call a function defined in the vm by its symbol
    CallNative1,
    CallNative,
    /// Call a runtime function with a name given by the symbol following.
    /// Args:
    ///   - name: Symbol
    RuntimeCall,

    /// Allocate an array on the the heap
    /// The arguments following are the size, align of the element and if the
    /// element is a reference. The top of the stack holds the number of
    /// elements. Args:
    ///  - size: Word
    ///  - align: Word
    ///  - is_ref: Word
    AllocArray,

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
        const WORD: usize = std::mem::size_of::<u64>();
        match self {
            OpCode::EqInteger => 0,
            OpCode::AddInteger => 0,
            OpCode::AddFloat => 0,
            OpCode::ConstLoadInteger => WORD,
            OpCode::ConstLoadFloat => WORD,
            OpCode::ConstLoadString => WORD,
            OpCode::LookUpGlobal => WORD,
            OpCode::Return => 0,
            OpCode::Call0 => 0,
            OpCode::Call1 => 0,
            OpCode::CallN => WORD,
            OpCode::Pop => 0,
            OpCode::JumpFront => WORD,
            OpCode::JumpBack => WORD,
            OpCode::JumpFrontIfFalse => WORD,
            OpCode::JumpBackIfFalse => WORD,
            OpCode::AllocHeap => WORD,
            OpCode::StackCopyBottom => WORD,
            OpCode::StackAlloc => WORD,
            OpCode::StackAssign => WORD,
            OpCode::PushTrue => 0,
            OpCode::PushFalse => 0,
            OpCode::PushUnit => 0,
            OpCode::PushOffsetDeref => WORD,
            OpCode::PushOffsetDerefNegative => WORD,
            OpCode::SetOffset => WORD,
            OpCode::SetOffsetNegative => WORD,
            OpCode::CallNative1 => WORD,
            OpCode::RuntimeCall => WORD,
            OpCode::AllocArray => WORD * 3,
            OpCode::CallNative => WORD,
            OpCode::LastMarker => panic!(),
        }
    }
}

/// Entry for a gc stack map.
/// Represents stack information about cells holding references
/// to scan.
#[derive(Clone, Debug)]
pub struct GcStackMapEntry {
    // Line starting at which this map entry starts.
    pub line: usize,
    // True for the stack slot wher the reference lives.
    pub references: BitVec,
}

impl GcStackMapEntry {
    pub fn empty() -> Self {
        Self {
            line: 0,
            references: BitVec::new(),
        }
    }

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
#[derive(Clone)]
pub struct Code {
    pub instructions: Vec<u8>,
    pub stack_map: GcStackMap,
}

impl Default for Code {
    fn default() -> Self {
        Self::new()
    }
}

impl Code {
    pub fn new() -> Code {
        Code {
            instructions: Vec::new(),
            stack_map: GcStackMap::new(),
        }
    }
}
