use std::{collections::HashMap, fmt::Debug};

use crate::{
    syntax::ast,
    typecheck::{
        inference::TypeEnv,
        r#type::Type,
        type_context::{TypCtx, TypeId},
    },
};

#[derive(Clone, Debug)]
pub struct Label(usize);

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
    NoAddress,
}

impl Debug for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Temporary(arg0) => write!(f, "t_{}", arg0),
            Self::Name(arg0) => write!(f, "{}", arg0),
            Self::ConstantInt(arg0) => write!(f, "@{}", arg0),
            Self::ConstantFloat(arg0) => write!(f, "@{}", arg0),
            Self::ConstantBool(arg0) => write!(f, "@{}", arg0),
            Self::ConstantString(arg0) => write!(f, "\"{}\"", arg0),
            Self::ConstantUnit => write!(f, "()"),
            Self::Parameter(arg0) => write!(f, "arg {}", arg0),
            Self::NoAddress => write!(f, "--"),
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

#[derive(Clone )]
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
            Self::BinOpAssign(arg0, arg1, arg2, arg3) => f.debug_tuple("BinOpAssign").field(arg0).field(arg1).field(arg2).field(arg3).finish(),
            Self::UnOpAssing(arg0, arg1, arg2) => f.debug_tuple("UnOpAssing").field(arg0).field(arg1).field(arg2).finish(),
            Self::CopyAssign(arg0, arg1) => write!(f, "{:?} = {:?}", arg0, arg1),
            Self::IndexCopy { res, base, offset } => f.debug_struct("IndexCopy").field("res", res).field("base", base).field("offset", offset).finish(),
            Self::IndexSet { base, offset, value } => f.debug_struct("IndexSet").field("base", base).field("offset", offset).field("value", value).finish(),
            Self::GoTo(arg0) => write!(f, "goto {:?}", arg0),
            Self::If { cond, goto } => write!(f, "if {:?} goto {:?}", cond, goto),
            Self::IfFalse { cond, goto } => write!(f, "ifFalse {:?} goto {:?}", cond, goto),
            Self::Param(arg0) => write!(f, "param {:?}", arg0),
            Self::Call { res, func, argc } => {
                write!(f, "{:?} = call {:?} {}", res, func, argc)
            },
            Self::Label(arg0) => write!(f, "label {}", arg0),
            Self::Alloc { res, size, typ } => f.debug_struct("Alloc").field("res", res).field("size", size).field("typ", typ).finish(),
            Self::AllocArray { res, elem_size, count, typ } => f.debug_struct("AllocArray").field("res", res).field("elem_size", elem_size).field("count", count).field("typ", typ).finish(),
            Self::Return(arg0) => write!(f, "return {:?}", arg0),
        }
    }
}

type Scope = HashMap<String, Address>;

pub struct FunctionEmitter<'ctx, 'env> {
    next_label: usize,
    next_temporary: usize,
    current_block: Block,
    tctx: &'ctx TypCtx,
    env: &'env TypeEnv,
    scopes: Vec<Scope>,
    args: Vec<String>,
}

impl<'ctx, 'env> FunctionEmitter<'ctx, 'env> {
    pub fn new(ctx: &'ctx TypCtx, env: &'env TypeEnv) -> Self {
        Self {
            next_label: 0,
            next_temporary: 0,
            current_block: Vec::new(),
            tctx: ctx,
            env,
            scopes: Vec::new(),
            args: Vec::new(),
        }
    }

    pub fn emit_fn(mut self, func: &ast::FunctionDefinition) -> Block {
        self.args = func.parameters.clone();
        self.new_scope();
 
        let res = self.emit_expr(&func.body);
        self.current_block.push(Instruction::Return(res));

        self.current_block
    }

    fn emit_expr(&mut self, ast::Expr { id, kind: expr }: &ast::Expr) -> Address {
        match expr {
            ast::ExprKind::Unit => Address::ConstantUnit,
            ast::ExprKind::Sequence(exprs) => {
                let res = self.new_temporary();
                for expr in exprs {
                    let e_res = self.emit_expr(expr);
                    self.current_block
                        .push(Instruction::CopyAssign(res.clone(), e_res));
                }
                res
            }
            ast::ExprKind::Assignment { lval, rval } => {
                let lval = self.emit_expr(lval);
                let rval = self.emit_expr(rval);
                self.current_block
                    .push(Instruction::CopyAssign(lval.clone(), rval));
                lval
            }
            ast::ExprKind::IntConst(v) => Address::ConstantInt(*v),
            ast::ExprKind::StringConst(v) => Address::ConstantString(v.clone()),
            ast::ExprKind::FloatConst(v) => Address::ConstantFloat(*v),
            ast::ExprKind::BoolConst(v) => Address::ConstantBool(*v),
            ast::ExprKind::Name(n) => {
                if let Some(addr) = self.find_in_scopes(n) {
                    return addr;
                }
                if let Some(pos) = self.args.iter().position(|arg| arg == n) {
                    return Address::Parameter(pos);
                }
                Address::Name(n.clone())
            }
            ast::ExprKind::FunctionCall { func, args, kwargs } => {
                assert!(kwargs.is_empty(), "Kwargs are not supported yet");
                let func = self.emit_expr(func);
                let args = args
                    .iter()
                    .map(|arg| self.emit_expr(arg))
                    .collect::<Vec<_>>();
                for arg in &args {
                    self.current_block.push(Instruction::Param(arg.clone()));
                }
                let res = self.new_temporary();
                self.current_block.push(Instruction::Call {
                    res: res.clone(),
                    func,
                    argc: args.len(),
                });
                res
            }
            ast::ExprKind::Access { val, field } => {
                let val = self.emit_expr(val);
                let Type::Struct { fields, .. } = self.env.get(id).unwrap() else {
                    panic!("Should be a struct");
                };
                let offset = fields.iter().position(|(name, _)| name == field).unwrap();
                let res = self.new_temporary();
                self.current_block.push(Instruction::IndexCopy {
                    res: res.clone(),
                    base: val,
                    offset: Address::ConstantInt(offset as isize),
                });
                res
            }
            ast::ExprKind::Tuple(_) => todo!(),
            ast::ExprKind::Lambda { .. } => todo!(),
            ast::ExprKind::TypedExpr { expr, .. } => self.emit_expr(expr),
            ast::ExprKind::StructLiteral { strct, values } => {
                // TODO:
                // We should believe into scope analysis but I am pretty sure I did not
                // do it and this might create a struct when we dont want to because we reassigned
                // the name of the type to something else
                let name = strct.0.last().unwrap().clone();
                let Type::Struct { fields, .. } = self.env.get(id).unwrap() else {
                    panic!("Should be a struct");
                };
                todo!()
                // this one is complicated as heck, we need to alloc and then init every field with proper offset
            },
            ast::ExprKind::While { cond, body } => {
                let cond_label = self.new_label();
                let exit_label = self.new_label();
                self.current_block.push(Instruction::Label(cond_label.0));
                let cond = self.emit_expr(cond);
                self.current_block.push(Instruction::IfFalse {
                    cond,
                    goto: exit_label.clone(),
                });
                self.new_scope();
                self.emit_expr(body);
                self.current_block.push(Instruction::GoTo(cond_label));
                self.close_scope();
                self.current_block.push(Instruction::Label(exit_label.0));
                Address::NoAddress
            }
            ast::ExprKind::Return(e) => {
                let res = self.emit_expr(e);
                self.current_block.push(Instruction::Return(res));
                Address::NoAddress
            }
            ast::ExprKind::If {
                cond,
                body,
                else_body,
            } => {
                let res = self.new_temporary();
                let else_label = self.new_label();
                let exit_label = self.new_label();
                let cond = self.emit_expr(cond);
                self.current_block.push(Instruction::IfFalse {
                    cond,
                    goto: else_label.clone(),
                });
                self.new_scope();
                let if_res = self.emit_expr(body);
                self.current_block.push(Instruction::CopyAssign(
                    res.clone(),
                    if else_body.is_none() {
                        Address::ConstantUnit
                    } else {
                        if_res
                    },
                ));
                self.current_block
                    .push(Instruction::GoTo(exit_label.clone()));
                self.close_scope();
                self.current_block.push(Instruction::Label(else_label.0));
                if let Some(else_body) = else_body {
                    self.new_scope();
                    let else_res = self.emit_expr(else_body);
                    self.current_block
                        .push(Instruction::CopyAssign(res.clone(), else_res));
                    self.close_scope();
                }
                self.current_block.push(Instruction::Label(exit_label.0));
                res
            }
            ast::ExprKind::Let { name, init, .. } => {
                let init = self.emit_expr(init);
                let res = self.new_temporary();
                self.current_block
                    .push(Instruction::CopyAssign(res.clone(), init));
                self.add_to_scope(name, res.clone());
                res
            }
            ast::ExprKind::AnonType { .. } => {
                panic!("should not exist at this point")
            }
        }
    }

    fn new_label(&mut self) -> Label {
        let label = Label(self.next_label);
        self.next_label += 1;
        label
    }

    fn new_temporary(&mut self) -> Address {
        let temp = Address::Temporary(self.next_temporary);
        self.next_temporary += 1;
        temp
    }

    fn new_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn close_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_to_scope(&mut self, name: &str, address: Address) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), address);
    }

    fn find_in_scopes(&self, name: &str) -> Option<Address> {
        for scope in self.scopes.iter().rev() {
            if let Some(address) = scope.get(name) {
                return Some(address.clone());
            }
        }
        None
    }
}
