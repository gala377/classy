use std::fmt::Debug;

use colored::*;

use crate::typecheck::type_context::TypeId;

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

#[derive(Clone)]
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
    IntEq,
    FloatEq,
    StringEq,
    BoolEq,
    Neq,
    GrEq,
    Gr,
    LeEq,
    Le,
    Not,

    // References
    Deref,
    Ref,
    // Retrieve the header data
    HeaderData,
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
    // Allocate a new ADT case
    AllocCase {
        res: Address,
        size: usize,
        case: usize,
        typ: TypeId,
    },
    AllocArray {
        res: Address,
        elem_size: usize,
        elem_align: usize,
        is_elem_ref: bool,
        count: Address,
    },
    // Return from a function
    Return(Address),
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BinOpAssign(arg0, arg1, arg2, arg3) =>
                write!(f, "{arg0:?} = {arg1:?} {arg2:?} {arg3:?}"),
            Self::UnOpAssing(arg0, arg1, arg2) =>
                write!(f, "{arg0:?} = {arg1:?} {arg2:?}"),
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
                elem_align,
                is_elem_ref,
                count,
            } => write!(
                f,
                "{res:?} = {}[count={count:?}; is_ref={is_elem_ref}, elem_size={elem_size}; elem_align={elem_align}]",
                "array".bold().green()
            ),
            Self::AllocCase { res, size, case, typ } => write!(
                f,
                "{res:?} = {}[{size}] {} {case} {} {typ}",
                "alloc_case".bold().green(),
                "case".bold().green(),
                "type".bold().green()),
            Self::Return(arg0) => write!(f, "{} {arg0:?}", "return".bold().green()),
        }
    }
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "{}", "+".bold().white()),
            Self::Sub => write!(f, "{}", "-".bold().white()),
            Self::Mul => write!(f, "{}", "*".bold().white()),
            Self::Div => write!(f, "{}", "/".bold().white()),
            Self::Mod => write!(f, "{}", "%".bold().white()),
            Self::Neg => write!(f, "{}", "-".bold().white()),
            Self::FAdd => write!(f, "+."),
            Self::FSub => write!(f, "-."),
            Self::FMul => write!(f, "*."),
            Self::FDiv => write!(f, "/."),
            Self::BoolEq | Self::FloatEq | Self::StringEq | Self::IntEq => {
                write!(f, "{}", "==".bold().white())
            }
            Self::Neq => write!(f, "!="),
            Self::GrEq => write!(f, ">="),
            Self::Gr => write!(f, ">"),
            Self::LeEq => write!(f, "<="),
            Self::Le => write!(f, "<"),
            Self::Not => write!(f, "!"),
            Self::Deref => write!(f, "*"),
            Self::Ref => write!(f, "&"),
            Self::HeaderData => write!(f, "{}", "header_data".bold().white()),
        }
    }
}
