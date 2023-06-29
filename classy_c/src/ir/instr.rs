use std::fmt::Debug;

use colored::*;

use crate::typecheck::{type_context::TypeId, r#type::Type};

#[derive(Clone, Debug)]
pub enum IsRef {
    Ref,
    NoRef,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Label(pub usize);

impl Debug for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string().bold().yellow())
    }
}

#[derive(Clone)]
pub enum Address {
    Temporary(usize, IsRef),
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
            Self::Temporary(arg0, is_ref) => write!(f, "{}", {
                let s = format!("t_{arg0}");
                if let IsRef::Ref = is_ref {
                    s.bright_cyan()
                } else {
                    s.normal()
                }
            }),
            Self::Name(arg0) => write!(f, "{arg0}"),
            Self::ConstantInt(arg0) => write!(f, "{}", arg0.to_string().bright_magenta()),
            Self::ConstantFloat(arg0) => write!(f, "{}", arg0.to_string().bright_magenta()),
            Self::ConstantBool(arg0) => write!(f, "{}", arg0.to_string().bright_magenta()),
            Self::ConstantString(arg0) => write!(f, "{}", format!("\"{arg0}\"").bright_magenta()),
            Self::ConstantUnit => write!(f, "{}", "()".bright_magenta()),
            Self::Parameter(arg0) => write!(f, "{} {}", "arg".bold().green(), arg0),
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
        typ: Type,
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
            Self::IndexCopy { res, base, offset } => write!(f, "{res:?} = {base:?}[{offset:?}]"),

            Self::IndexSet {
                base,
                offset,
                value,
            } => write!(f, "{base:?}[{offset:?}] = {value:?}"),
            Self::GoTo(arg0) => write!(f, "{} {arg0:?}", "goto".bold().green()),
            Self::If { cond, goto } => write!(
                f,
                "{} {cond:?} {} {goto:?}",
                "if".bold().green(),
                "goto".bold().green()
            ),
            Self::IfFalse { cond, goto } => write!(
                f,
                "{} {cond:?} {} {goto:?}",
                "ifFalse".bold().green(),
                "goto".bold().green()
            ),
            Self::Param(arg0) => write!(f, "{} {arg0:?}", "param".bold().green()),
            Self::Call { res, func, argc } => {
                write!(f, "{res:?} = {} {func:?} {argc}", "call".bold().green())
            }
            Self::Label(arg0) => write!(
                f,
                "{} {}",
                "label".bold().green(),
                arg0.to_string().bold().yellow()
            ),
            Self::Alloc { res, size, typ } => write!(
                f,
                "{res:?} = {}[{size}] {} {typ}",
                "alloc".bold().green(),
                "type".bold().green()
            ),
            Self::AllocArray {
                res,
                elem_size,
                count,
                typ,
            } => write!(f, "{res:?} = {}[{count:?}; {elem_size}; {typ:?}]", "array".bold().green()),
            Self::Return(arg0) => write!(f, "{} {arg0:?}", "return".bold().green()),
        }
    }
}
