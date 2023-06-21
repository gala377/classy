use std::fmt::Debug;

use crate::typecheck::type_context::TypeId;

#[derive(Clone, Debug)]
pub struct Label(pub usize);

#[derive(Clone)]
pub enum Address {
    Temporary(usize),
    Name(String),
    ConstantInt(isize),
    ConstantFloat(f64),
    ConstantBool(bool),
    ConstantString(String),
    ConstantUnit,
    // Function parameter at index
    Parameter(usize),
}

impl Debug for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Temporary(arg0) => write!(f, "t_{}", arg0),
            Self::Name(arg0) => write!(f, "{}", arg0),
            Self::ConstantInt(arg0) => write!(f, "{}", arg0),
            Self::ConstantFloat(arg0) => write!(f, "{}", arg0),
            Self::ConstantBool(arg0) => write!(f, "{}", arg0),
            Self::ConstantString(arg0) => write!(f, "\"{}\"", arg0),
            Self::ConstantUnit => write!(f, "()"),
            Self::Parameter(arg0) => write!(f, "arg {}", arg0),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Op {
    // Int operations
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    // Float operations
    FAdd,
    FSub,
    FMul,
    FDiv,
    // Bool operations
    Eq,
    Neq,
    GrEq,
    Gr,
    LeEq,
    Le,
    Not,

    // References
    Deref,
    Ref,
}

pub struct Code {
    pub blocks: Vec<Block>,
    pub stable_block_ordering: Vec<Label>,
}

pub type Block = Vec<Instruction>;

#[derive(Clone)]
pub enum Instruction {
    // Assignments
    BinOpAssign(Address, Address, Op, Address),
    UnOpAssing(Address, Op, Address),
    CopyAssign(Address, Address),
    IndexCopy {
        res: Address,
        base: Address,
        offset: Address,
    },
    IndexSet {
        base: Address,
        offset: Address,
        value: Address,
    },

    // jumps
    GoTo(Label),
    If {
        cond: Address,
        goto: Label,
    },
    IfFalse {
        cond: Address,
        goto: Label,
    },

    // func call
    Param(Address),
    Call {
        res: Address,
        func: Address,
        argc: usize,
    },
    // Label to later jump to
    Label(usize),

    // Allocate new object of class
    Alloc {
        res: Address,
        size: usize,
        typ: TypeId,
    },
    AllocArray {
        res: Address,
        elem_size: usize,
        count: Address,
        typ: TypeId,
    },
    // Return from a function
    Return(Address),
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BinOpAssign(arg0, arg1, arg2, arg3) => f
                .debug_tuple("BinOpAssign")
                .field(arg0)
                .field(arg1)
                .field(arg2)
                .field(arg3)
                .finish(),
            Self::UnOpAssing(arg0, arg1, arg2) => f
                .debug_tuple("UnOpAssing")
                .field(arg0)
                .field(arg1)
                .field(arg2)
                .finish(),
            Self::CopyAssign(arg0, arg1) => write!(f, "{arg0:?} = {arg1:?}"),
            Self::IndexCopy { res, base, offset } => f
                .debug_struct("IndexCopy")
                .field("res", res)
                .field("base", base)
                .field("offset", offset)
                .finish(),
            Self::IndexSet {
                base,
                offset,
                value,
            } => write!(f, "{base:?}[{offset:?}] = {value:?}"),
            Self::GoTo(arg0) => write!(f, "goto {arg0:?}"),
            Self::If { cond, goto } => write!(f, "if {cond:?} goto {goto:?}"),
            Self::IfFalse { cond, goto } => write!(f, "ifFalse {cond:?} goto {goto:?}"),
            Self::Param(arg0) => write!(f, "param {arg0:?}"),
            Self::Call { res, func, argc } => {
                write!(f, "{res:?} = call {func:?} {argc}")
            }
            Self::Label(arg0) => write!(f, "label {arg0}"),
            Self::Alloc { res, size, typ } => write!(f, "{res:?} = alloc[{size}] of type {typ}"),
            Self::AllocArray {
                res,
                elem_size,
                count,
                typ,
            } => f
                .debug_struct("AllocArray")
                .field("res", res)
                .field("elem_size", elem_size)
                .field("count", count)
                .field("typ", typ)
                .finish(),
            Self::Return(arg0) => write!(f, "return {arg0:?}"),
        }
    }
}
