use std::collections::HashMap;

use crate::{
    syntax::ast,
    typecheck::{inference::TypeEnv, r#type::Type, type_context::TypCtx},
};

use super::instr::{Address, Block, Instruction, IsRef, Label};

type Scope = HashMap<String, Address>;

enum Lval {
    Address(Address),
    Indexed(Address, usize),
}

pub struct IrFunction {
    pub name: String,
    pub args: Vec<(String, IsRef)>,
    pub body: Block,
}

pub struct SourceEmitter {
    functions: HashMap<String, IrFunction>,
}

impl SourceEmitter {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    pub fn compile(
        mut self,
        program: ast::Program,
        tctx: &mut TypCtx,
        tenv: &TypeEnv,
    ) -> HashMap<String, IrFunction> {
        for decl in program.items {
            if let ast::TopLevelItem::FunctionDefinition(def) = decl {
                let emmiter = FunctionEmitter::new(tctx, tenv);
                let instrcs = emmiter.emit_fn(&def);
                self.functions.insert(def.name, instrcs);
            }
        }
        self.functions
    }
}

pub struct FunctionEmitter<'ctx, 'env> {
    next_label: usize,
    next_temporary: usize,
    current_block: Block,
    tctx: &'ctx TypCtx,
    env: &'env TypeEnv,
    scopes: Vec<Scope>,
    args: Vec<(String, IsRef)>,
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

    pub fn emit_fn(mut self, func: &ast::FunctionDefinition) -> IrFunction {
        let func_name = func.name.clone();
        let func_t_id = self.tctx.variables.get(&func_name).unwrap();
        let Type::Function { args, .. } = self.tctx.definitions.get(func_t_id).unwrap() else {
            panic!("Should be a function type");
        };
        let this_args = func
            .parameters
            .iter()
            .zip(args.iter())
            .map(|(param, arg_t)| {
                let is_ref = match arg_t.is_ref() {
                    None => panic!("Should not be any of these types"),
                    Some(true) => IsRef::Ref,
                    Some(false) => IsRef::NoRef,
                };
                (param.clone(), is_ref)
            })
            .collect::<Vec<_>>();
        self.args = this_args;
        self.new_scope();

        let res = self.emit_expr(&func.body);
        self.current_block.push(Instruction::Return(res));

        IrFunction {
            name: func_name,
            args: self.args,
            body: self.current_block,
        }
    }

    fn emit_expr(&mut self, ast::Expr { id, kind: expr }: &ast::Expr) -> Address {
        match expr {
            ast::ExprKind::Unit => Address::ConstantUnit,
            ast::ExprKind::Sequence(exprs) => {
                let mut res = None;
                for (i, expr) in exprs.iter().enumerate() {
                    let e_res = self.emit_expr(expr);
                    if i == exprs.len() - 1 {
                        let res_addr = self.new_temporary(self.is_ref(&e_res));
                        self.current_block
                            .push(Instruction::CopyAssign(res_addr.clone(), e_res));
                        res = Some(res_addr);
                    }
                }
                res.unwrap()
            }
            ast::ExprKind::Assignment { lval, rval } => {
                // TODO: I am not sure if this will
                // wotk with complex assignments like
                // a.b[10].c.d[20] = 1
                // but we would need to check it when the interpreter is ready
                let lval = self.emit_lval(lval);
                let rval = self.emit_expr(rval);
                match lval {
                    Lval::Address(addr) => {
                        self.current_block
                            .push(Instruction::CopyAssign(addr.clone(), rval));
                        addr
                    }
                    Lval::Indexed(addr, index) => {
                        self.current_block.push(Instruction::IndexSet {
                            base: addr.clone(),
                            offset: Address::ConstantInt(index as isize),
                            value: rval,
                        });
                        Address::ConstantUnit
                    }
                }
            }
            ast::ExprKind::IntConst(v) => Address::ConstantInt(*v),
            ast::ExprKind::StringConst(v) => Address::ConstantString(v.clone()),
            ast::ExprKind::FloatConst(v) => Address::ConstantFloat(*v),
            ast::ExprKind::BoolConst(v) => Address::ConstantBool(*v),
            ast::ExprKind::Name(n) => {
                if let Some(addr) = self.find_in_scopes(n) {
                    return addr;
                }
                if let Some(pos) = self.args.iter().position(|(arg, _)| arg == n) {
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
                let res_t = self.env.get(id).unwrap();
                let res = self.new_temporary(match res_t.is_ref() {
                    None => panic!(),
                    Some(true) => IsRef::Ref,
                    Some(false) => IsRef::NoRef,
                });
                self.current_block.push(Instruction::Call {
                    res: res.clone(),
                    func,
                    argc: args.len(),
                });
                res
            }
            ast::ExprKind::Access { val, field } => {
                let lhs_id = val.id;
                let val = self.emit_expr(val);
                let Type::Struct { fields, .. } = self.env.get(&lhs_id).unwrap() else {
                    panic!("Should be a struct");
                };
                let (offset, field_t) = fields
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == field)
                    .map(|(i, (_, t))| (i, t))
                    .unwrap();
                let res = self.new_temporary(match field_t.is_ref() {
                    None => panic!(),
                    Some(true) => IsRef::Ref,
                    Some(false) => IsRef::NoRef,
                });
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
                let Type::Struct { def, fields } = self.tctx.get_type(&name).unwrap() else {
                    panic!("should be a struct")
                };
                let tid = self.tctx.def_id_to_typ_id(def);
                let struct_address = self.new_temporary(IsRef::Ref);
                self.current_block.push(Instruction::Alloc {
                    res: struct_address.clone(),
                    size: fields.len(),
                    typ: tid,
                });
                let offset_and_value = fields.iter().enumerate().map(|(i, (name, _))| {
                    let expr = values.get(name).unwrap().clone();
                    (i, expr)
                });
                for (offset, expr) in offset_and_value {
                    let expr_addr = self.emit_expr(&expr);
                    self.current_block.push(Instruction::IndexSet {
                        base: struct_address.clone(),
                        offset: Address::ConstantInt(offset as isize),
                        value: expr_addr,
                    })
                }
                struct_address
            }
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
                Address::ConstantUnit
            }
            ast::ExprKind::Return(e) => {
                let res = self.emit_expr(e);
                self.current_block.push(Instruction::Return(res));
                Address::ConstantUnit
            }
            ast::ExprKind::If {
                cond,
                body,
                else_body,
            } => {
                let res_t = self.env.get(id).unwrap();
                let res = self.new_temporary(match res_t.is_ref() {
                    None => panic!(),
                    Some(true) => IsRef::Ref,
                    Some(false) => IsRef::NoRef,
                });
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
                let init_t = self.env.get(&init.id).unwrap();
                let is_ref = match init_t.is_ref() {
                    None => panic!("unexpeced type that cannot be told if its ref"),
                    Some(true) => IsRef::Ref,
                    Some(false) => IsRef::NoRef,
                };
                let init = self.emit_expr(init);
                let res = self.new_temporary(is_ref);
                self.current_block
                    .push(Instruction::CopyAssign(res.clone(), init));
                self.add_to_scope(name, res.clone());
                res
            }
            ast::ExprKind::AnonType { .. } => {
                panic!("should not exist at this point")
            }
            _ => panic!("Not implemented")
        }
    }

    fn new_label(&mut self) -> Label {
        let label = Label(self.next_label);
        self.next_label += 1;
        label
    }

    fn new_temporary(&mut self, is_ref: IsRef) -> Address {
        let temp = Address::Temporary(self.next_temporary, is_ref);
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

    fn is_ref(&self, addr: &Address) -> IsRef {
        match addr {
            Address::Temporary(_, is_ref) => is_ref.clone(),
            Address::Name(_) => {
                // TODO:
                // Probably not every name is a ref, like, they have to be a global
                // name so by that we could assume they have to be refs but that is
                // probably a wrong assumption and the best way to check this
                // would be to lookup a name.
                // We should have a one scope structure that we can somehow trawerse
                // and add values to.
                IsRef::Ref
            }
            Address::ConstantInt(_) => IsRef::NoRef,
            Address::ConstantFloat(_) => IsRef::NoRef,
            Address::ConstantBool(_) => IsRef::NoRef,
            Address::ConstantString(_) => IsRef::Ref,
            Address::ConstantUnit => IsRef::NoRef,
            Address::Parameter(i) => self.args[*i].1.clone(),
        }
    }

    fn emit_lval(&mut self, expr: &ast::Expr) -> Lval {
        match &expr.kind {
            ast::ExprKind::Name(name) => {
                if let Some(addr) = self.find_in_scopes(name) {
                    return Lval::Address(addr);
                }
                if let Some(pos) = self.args.iter().position(|(arg, _)| arg == name) {
                    return Lval::Address(Address::Parameter(pos));
                }
                Lval::Address(Address::Name(name.clone()))
            }
            ast::ExprKind::Access { val, field } => {
                let lhs_id = val.id;
                let val = self.emit_expr(val);
                let Type::Struct { fields, .. } = self.env.get(&lhs_id).unwrap() else {
                    panic!("Should be a struct");
                };
                let offset = fields.iter().position(|(name, _)| name == field).unwrap();
                Lval::Indexed(val, offset)
            }
            _ => panic!("Not lval"),
        }
    }
}