use std::collections::HashMap;

use classy_syntax::ast;

use crate::typecheck::{
    constrait_solver::instance,
    inference::TypeEnv,
    r#type::Type,
    type_context::{DefId, TypCtx},
};

use super::instr::{Address, Block, Instruction, IsRef, Label, Op};

type Scope = HashMap<String, Address>;

enum Lval {
    Address(Address),
    Indexed(Address, usize),
    IndexedDynamic(Address, Address),
}

pub struct IrFunction {
    pub name: String,
    pub args: Vec<(String, IsRef)>,
    pub body: Block,
}

#[derive(Default)]
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
            if let ast::TopLevelItem {
                id: _,
                kind: ast::TopLevelItemKind::FunctionDefinition(def),
            } = decl
            {
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

    fn get_function_args(&self, t: Type) -> Vec<Type> {
        match t {
            Type::Function { args, .. } => args.clone(),
            Type::Scheme { typ, .. } => self.get_function_args(*typ.clone()),
            Type::App { typ, args } => {
                self.get_function_args(instance(self.tctx, args.clone(), *typ.clone()))
            }
            Type::Alias(for_type) => self.get_function_args(self.tctx.resolve_alias(for_type)),
            _ => panic!("Should be a function"),
        }
    }

    fn get_struct_type(&self, name: &str) -> (DefId, Vec<(String, Type)>) {
        fn get_struct_type_impl(tctx: &TypCtx, t: Type) -> (DefId, Vec<(String, Type)>) {
            match t {
                Type::Struct { def, fields } => (def, fields.clone()),
                Type::Scheme { typ, .. } => get_struct_type_impl(tctx, *typ.clone()),
                Type::App { typ, args } => {
                    get_struct_type_impl(tctx, instance(tctx, args.clone(), *typ.clone()))
                }
                Type::Alias(for_type) => get_struct_type_impl(tctx, tctx.resolve_alias(for_type)),
                _ => panic!("expected a struct type"),
            }
        }
        let t = self.tctx.get_type(name).unwrap();
        get_struct_type_impl(self.tctx, t.clone())
    }

    fn get_struct_fields(&self, t: &Type) -> Vec<(String, Type)> {
        fn implementation(tctx: &TypCtx, t: Type) -> Vec<(String, Type)> {
            match t {
                Type::Struct { fields, .. } => fields.clone(),
                Type::Scheme { typ, .. } => implementation(tctx, *typ.clone()),
                Type::App { typ, args } => implementation(tctx, instance(tctx, args.clone(), *typ)),
                Type::Alias(for_type) => implementation(tctx, tctx.resolve_alias(for_type)),
                _ => panic!("expected a struct type"),
            }
        }
        implementation(self.tctx, t.clone())
    }

    fn get_adt_type(&self, name: &str) -> (DefId, Vec<(String, Type)>) {
        fn get_adt_type_impl(tctx: &TypCtx, t: Type) -> (DefId, Vec<(String, Type)>) {
            match t {
                Type::ADT { def, constructors } => (def, constructors.clone()),
                Type::Scheme { typ, .. } => get_adt_type_impl(tctx, *typ.clone()),
                Type::App { typ, args } => {
                    get_adt_type_impl(tctx, instance(tctx, args.clone(), *typ.clone()))
                }
                Type::Alias(for_type) => get_adt_type_impl(tctx, tctx.resolve_alias(for_type)),
                _ => panic!("expected a struct type"),
            }
        }
        let t = self.tctx.get_type(name).unwrap();
        get_adt_type_impl(self.tctx, t.clone())
    }

    pub fn emit_fn(mut self, func: &ast::FunctionDefinition) -> IrFunction {
        let func_name = func.name.clone();
        let func_t_id = self.tctx.variables.get(&func_name).unwrap();
        let func_t = self.tctx.definitions.get(func_t_id).unwrap();
        let args = self.get_function_args(func_t.clone());
        let this_args = func
            .parameters
            .iter()
            .zip(args.iter())
            .map(|(param, arg_t)| {
                let arg_t = match arg_t {
                    Type::Alias(for_type) => self.tctx.resolve_alias(*for_type),
                    t => t.clone(),
                };
                let is_ref = match arg_t.is_ref() {
                    None => panic!("Should not be any of these types {arg_t:?}"),
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
                // work with complex assignments like
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
                            base: addr,
                            offset: Address::ConstantInt(index as isize),
                            value: rval,
                        });
                        Address::ConstantUnit
                    }
                    Lval::IndexedDynamic(addr, index) => {
                        self.current_block.push(Instruction::IndexSet {
                            base: addr,
                            offset: index,
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
                let fields = self.get_struct_fields(self.env.get(&lhs_id).unwrap());
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
            ast::ExprKind::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|expr| self.emit_expr(expr))
                    .collect::<Vec<_>>();
                let is_ref: Vec<_> = types
                    .iter()
                    .map(|x| match self.is_ref(x) {
                        IsRef::NoRef => false,
                        IsRef::Ref => true,
                    })
                    .collect();
                let res = self.new_temporary(IsRef::Ref);
                self.current_block.push(Instruction::AllocTuple {
                    res: res.clone(),
                    size: types.len(),
                    refmap: is_ref,
                });
                for (i, expr) in types.iter().enumerate() {
                    self.current_block.push(Instruction::IndexSet {
                        base: res.clone(),
                        offset: Address::ConstantInt(i as isize),
                        value: expr.clone(),
                    });
                }
                res
            }
            ast::ExprKind::Lambda { .. } => todo!(),
            ast::ExprKind::TypedExpr { expr, .. } => self.emit_expr(expr),
            ast::ExprKind::StructLiteral { strct, values } => {
                // TODO:
                // We should believe into scope analysis but I am pretty sure I did not
                // do it and this might create a struct when we dont want to because we
                // reassigned the name of the type to something else
                let name = strct.0.last().unwrap().clone();
                let (def, fields) = self.get_struct_type(&name);
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
            ast::ExprKind::AdtUnitConstructor { typ, constructor } => {
                let (def, cons) = self.get_adt_type(typ);
                let tid = self.tctx.def_id_to_typ_id(def);
                let adt_address = self.new_temporary(IsRef::Ref);
                let Some((cpos, (_, ctyp))) = cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == constructor)
                else {
                    panic!("Constructor {constructor} not found");
                };
                assert!(ctyp == &Type::Unit, "invariant");
                self.current_block.push(Instruction::AllocCase {
                    res: adt_address.clone(),
                    size: 0,
                    case: cpos,
                    typ: tid,
                });
                adt_address
            }
            ast::ExprKind::AdtTupleConstructor {
                typ,
                constructor,
                args,
            } => {
                let (def, cons) = self.get_adt_type(typ);
                let tid = self.tctx.def_id_to_typ_id(def);
                let adt_address = self.new_temporary(IsRef::Ref);
                let Some((cpos, (_, ctyp))) = cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == constructor)
                else {
                    panic!("Constructor {constructor} not found");
                };
                assert!(matches!(ctyp, Type::Tuple(_)), "invariant");
                self.current_block.push(Instruction::AllocCase {
                    res: adt_address.clone(),
                    size: args.len(),
                    case: cpos,
                    typ: tid,
                });
                for (offset, expr) in args.iter().enumerate() {
                    let expr_addr = self.emit_expr(expr);
                    self.current_block.push(Instruction::IndexSet {
                        base: adt_address.clone(),
                        offset: Address::ConstantInt(offset as isize),
                        value: expr_addr,
                    })
                }
                adt_address
            }
            ast::ExprKind::AdtStructConstructor {
                typ,
                constructor,
                fields,
            } => {
                let (def, cons) = self.get_adt_type(typ);
                let tid = self.tctx.def_id_to_typ_id(def);
                let adt_address = self.new_temporary(IsRef::Ref);
                let Some((cpos, (_, ctyp))) = cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == constructor)
                else {
                    panic!("Constructor {constructor} not found");
                };
                let Type::Struct {
                    fields: fields_t, ..
                } = ctyp
                else {
                    panic!("invariant")
                };
                assert!(fields_t.len() == fields.len(), "invariant");
                let fields_expr: HashMap<_, _> = fields.iter().cloned().collect();
                self.current_block.push(Instruction::AllocCase {
                    res: adt_address.clone(),
                    size: fields.len(),
                    case: cpos,
                    typ: tid,
                });
                for (offset, (name, _)) in fields_t.iter().enumerate() {
                    let expr = &fields_expr[name];
                    let expr_addr = self.emit_expr(expr);
                    self.current_block.push(Instruction::IndexSet {
                        base: adt_address.clone(),
                        offset: Address::ConstantInt(offset as isize),
                        value: expr_addr,
                    })
                }
                adt_address
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
            ast::ExprKind::IndexAccess { lhs, index } => {
                let lhs_id = lhs.id;
                let val = self.emit_expr(lhs);
                let Type::Array(inner_t) = self.env.get(&lhs_id).unwrap() else {
                    panic!(
                        "Should be an array got {:?}",
                        self.env.get(&lhs_id).unwrap()
                    );
                };
                let index = self.emit_expr(index);
                let res = self.new_temporary(match inner_t.is_ref() {
                    None => panic!(),
                    Some(true) => IsRef::Ref,
                    Some(false) => IsRef::NoRef,
                });
                self.current_block.push(Instruction::IndexCopy {
                    res: res.clone(),
                    base: val,
                    offset: index,
                });
                res
            }
            ast::ExprKind::ArrayLiteral { size, init, .. } => {
                let Type::Array(inner_t) = self.env.get(id).unwrap() else {
                    panic!("Should be an array");
                };
                let res = self.new_temporary(IsRef::Ref);
                let size = self.emit_expr(size);
                self.current_block.push(Instruction::AllocArray {
                    res: res.clone(),
                    is_elem_ref: inner_t.is_ref().unwrap(),
                    elem_size: inner_t.byte_size(),
                    elem_align: inner_t.align(),
                    count: size,
                });
                for (i, expr) in init.iter().enumerate() {
                    let expr_addr = self.emit_expr(expr);
                    self.current_block.push(Instruction::IndexSet {
                        base: res.clone(),
                        offset: Address::ConstantInt(i as isize),
                        value: expr_addr,
                    })
                }
                res
            }
            ast::ExprKind::AnonType { .. } => {
                panic!("should not exist at this point")
            }
            ast::ExprKind::Match { expr, cases } => {
                let t = self.env.get(&expr.id).unwrap();
                let res = self.new_temporary(match t.is_ref() {
                    None => panic!(),
                    Some(true) => IsRef::Ref,
                    Some(false) => IsRef::NoRef,
                });
                let expr = self.emit_expr(expr);
                let labels = cases.iter().map(|_| self.new_label()).collect::<Vec<_>>();
                let exit_label = self.new_label();
                for (case, label) in cases.iter().zip(labels.iter()) {
                    self.emit_pattern(expr.clone(), &case.0, label.clone());
                    let body = self.emit_expr(&case.1);
                    self.current_block
                        .push(Instruction::CopyAssign(res.clone(), body));
                    self.current_block
                        .push(Instruction::GoTo(exit_label.clone()));
                    self.current_block.push(Instruction::Label(label.0));
                }
                self.current_block.push(Instruction::Label(exit_label.0));
                res
            }
            x => todo!("{x:?}"),
        }
    }

    fn emit_pattern(&mut self, to_match: Address, pattern: &ast::Pattern, next_label: Label) {
        let id = pattern.id;
        match &pattern.kind {
            ast::PatternKind::Name(n) => {
                if n.chars().take(1).all(|c| c.is_uppercase()) {
                    let t = self.env.get(&id).unwrap();
                    let constructor = self.get_constructors(t);
                    let (cpos, ctyp) = constructor
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| name == n)
                        .map(|(i, (_, t))| (i, t))
                        .unwrap();
                    assert!(matches!(ctyp, Type::Unit), "invariant");
                    let val_case = self.new_temporary(IsRef::NoRef);
                    self.current_block.push(Instruction::UnOpAssing(
                        val_case.clone(),
                        Op::HeaderData,
                        to_match.clone(),
                    ));
                    let cond = self.new_temporary(IsRef::NoRef);
                    self.current_block.push(Instruction::BinOpAssign(
                        cond.clone(),
                        val_case.clone(),
                        Op::IntEq,
                        Address::ConstantInt(cpos as isize),
                    ));
                    self.current_block.push(Instruction::IfFalse {
                        cond: cond.clone(),
                        goto: next_label.clone(),
                    });
                    return;
                }
                self.add_to_scope(n, to_match);
            }
            ast::PatternKind::Tuple(patterns) => {
                for (i, p) in patterns.iter().enumerate() {
                    let field_t = self.env.get(&p.id).unwrap();
                    let val = self.new_temporary(match field_t.is_ref() {
                        None => panic!(),
                        Some(true) => IsRef::Ref,
                        Some(false) => IsRef::NoRef,
                    });
                    self.current_block.push(Instruction::IndexCopy {
                        res: val.clone(),
                        base: to_match.clone(),
                        offset: Address::ConstantInt(i as isize),
                    });
                    self.emit_pattern(val, p, next_label.clone());
                }
            }
            ast::PatternKind::Array(patterns) => {
                let arr_len = self.new_temporary(IsRef::NoRef);
                self.current_block.push(Instruction::UnOpAssing(
                    arr_len.clone(),
                    Op::HeaderData,
                    to_match.clone(),
                ));
                let len_cond = self.new_temporary(IsRef::NoRef);
                self.current_block.push(Instruction::BinOpAssign(
                    len_cond.clone(),
                    arr_len.clone(),
                    Op::IntEq,
                    Address::ConstantInt(patterns.len() as isize),
                ));
                self.current_block.push(Instruction::IfFalse {
                    cond: len_cond.clone(),
                    goto: next_label.clone(),
                });
                for (i, p) in patterns.iter().enumerate() {
                    let field_t = self.env.get(&p.id).unwrap();
                    let val = self.new_temporary(match field_t.is_ref() {
                        None => panic!(),
                        Some(true) => IsRef::Ref,
                        Some(false) => IsRef::NoRef,
                    });
                    self.current_block.push(Instruction::IndexCopy {
                        res: val.clone(),
                        base: to_match.clone(),
                        offset: Address::ConstantInt(i as isize),
                    });
                    self.emit_pattern(val, p, next_label.clone());
                }
            }
            ast::PatternKind::Struct { strct, fields } => {
                let t = self.env.get(&id).unwrap();
                let struct_fields = match t {
                    Type::Struct { fields, .. } => fields.clone(),
                    Type::ADT { constructors, .. } => {
                        let (cpos, ctyp) = constructors
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| name == strct)
                            .map(|(i, (_, t))| (i, t))
                            .unwrap();
                        let val_case = self.new_temporary(IsRef::NoRef);
                        self.current_block.push(Instruction::UnOpAssing(
                            val_case.clone(),
                            Op::HeaderData,
                            to_match.clone(),
                        ));
                        let cond = self.new_temporary(IsRef::NoRef);
                        self.current_block.push(Instruction::BinOpAssign(
                            cond.clone(),
                            val_case.clone(),
                            Op::IntEq,
                            Address::ConstantInt(cpos as isize),
                        ));
                        self.current_block.push(Instruction::IfFalse {
                            cond: cond.clone(),
                            goto: next_label.clone(),
                        });
                        let Type::Struct { fields, .. } = ctyp else {
                            panic!("invarian {ctyp:?}")
                        };
                        fields.clone()
                    }
                    t => panic!("Expected struct or adt, got {t:?}"),
                };
                for (i, (field_name, field_type)) in struct_fields.iter().enumerate() {
                    let field_type = if let Type::Alias(for_t) = field_type {
                        self.tctx.resolve_alias(*for_t)
                    } else {
                        field_type.clone()
                    };
                    let val = self.new_temporary(match field_type.is_ref() {
                        None => panic!(),
                        Some(true) => IsRef::Ref,
                        Some(false) => IsRef::NoRef,
                    });
                    self.current_block.push(Instruction::IndexCopy {
                        res: val.clone(),
                        base: to_match.clone(),
                        offset: Address::ConstantInt(i as isize),
                    });
                    let field_pattern = fields.get(field_name).unwrap();
                    self.emit_pattern(val, field_pattern, next_label.clone());
                }
            }
            ast::PatternKind::TupleStruct { strct, fields } => {
                let constructors = self.get_constructors(self.env.get(&id).unwrap());
                let (cpos, ctyp) = constructors
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == strct)
                    .map(|(i, (_, t))| (i, t))
                    .unwrap();
                let val_case = self.new_temporary(IsRef::NoRef);
                self.current_block.push(Instruction::UnOpAssing(
                    val_case.clone(),
                    Op::HeaderData,
                    to_match.clone(),
                ));
                let cond = self.new_temporary(IsRef::NoRef);
                self.current_block.push(Instruction::BinOpAssign(
                    cond.clone(),
                    val_case.clone(),
                    Op::IntEq,
                    Address::ConstantInt(cpos as isize),
                ));
                self.current_block.push(Instruction::IfFalse {
                    cond: cond.clone(),
                    goto: next_label.clone(),
                });
                let Type::Tuple(fields_t) = ctyp else {
                    panic!("invarian {ctyp:?}")
                };
                for (i, field_type) in fields_t.iter().enumerate() {
                    let field_type = if let Type::Alias(for_t) = field_type {
                        self.tctx.resolve_alias(*for_t)
                    } else {
                        field_type.clone()
                    };
                    let val = self.new_temporary(match field_type.is_ref() {
                        None => panic!(),
                        Some(true) => IsRef::Ref,
                        Some(false) => IsRef::NoRef,
                    });
                    self.current_block.push(Instruction::IndexCopy {
                        res: val.clone(),
                        base: to_match.clone(),
                        offset: Address::ConstantInt(i as isize),
                    });
                    self.emit_pattern(val, &fields[i], next_label.clone());
                }
            }
            ast::PatternKind::AnonStruct { fields } => {
                let Type::Struct {
                    fields: struct_fields,
                    ..
                } = self.env.get(&id).unwrap()
                else {
                    panic!("Should be a struct");
                };
                for (i, (field_name, field_type)) in struct_fields.iter().enumerate() {
                    let field_type = if let Type::Alias(for_t) = field_type {
                        self.tctx.resolve_alias(*for_t)
                    } else {
                        field_type.clone()
                    };
                    let val = self.new_temporary(match field_type.is_ref() {
                        None => panic!(),
                        Some(true) => IsRef::Ref,
                        Some(false) => IsRef::NoRef,
                    });
                    self.current_block.push(Instruction::IndexCopy {
                        res: val.clone(),
                        base: to_match.clone(),
                        offset: Address::ConstantInt(i as isize),
                    });
                    let field_pattern = fields.get(field_name).unwrap();
                    self.emit_pattern(val, field_pattern, next_label.clone());
                }
            }
            ast::PatternKind::TypeSpecifier(_, p) => {
                // todo: I think this is correct as p should have a type assigned
                // at this point
                self.emit_pattern(to_match, p, next_label)
            }
            // TODO: I think that unit type always matches as there is only one possible
            // value for it so there is not point in checking it
            ast::PatternKind::Unit => {}
            // The same for wildcard, it always matches so there is nothing to do for us
            ast::PatternKind::Wildcard => {}
            ast::PatternKind::String(s) => {
                let cond = self.new_temporary(IsRef::NoRef);
                self.current_block.push(Instruction::BinOpAssign(
                    cond.clone(),
                    to_match,
                    Op::StringEq,
                    Address::ConstantString(s.clone()),
                ));
                self.current_block.push(Instruction::IfFalse {
                    cond: cond.clone(),
                    goto: next_label,
                });
            }
            ast::PatternKind::Int(i) => {
                let cond = self.new_temporary(IsRef::NoRef);
                self.current_block.push(Instruction::BinOpAssign(
                    cond.clone(),
                    to_match,
                    Op::IntEq,
                    Address::ConstantInt(*i),
                ));
                self.current_block.push(Instruction::IfFalse {
                    cond: cond.clone(),
                    goto: next_label,
                });
            }
            ast::PatternKind::Bool(b) => {
                let cond = self.new_temporary(IsRef::NoRef);
                self.current_block.push(Instruction::BinOpAssign(
                    cond.clone(),
                    to_match,
                    Op::BoolEq,
                    Address::ConstantBool(*b),
                ));
                self.current_block.push(Instruction::IfFalse {
                    cond: cond.clone(),
                    goto: next_label,
                });
            }
            ast::PatternKind::Rest(_) => todo!(),
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

    fn get_constructors(&self, t: &Type) -> Vec<(String, Type)> {
        match t {
            Type::ADT { constructors, .. } => constructors.clone(),
            Type::Alias(for_t) => self.get_constructors(&self.tctx.resolve_alias(*for_t)),
            Type::Scheme { typ, .. } => self.get_constructors(typ),
            Type::App { typ, .. } => self.get_constructors(typ),
            _ => panic!("Expected adt got {t:?}"),
        }
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
                let fields = self.get_struct_fields(self.env.get(&lhs_id).unwrap());
                let offset = fields.iter().position(|(name, _)| name == field).unwrap();
                Lval::Indexed(val, offset)
            }
            ast::ExprKind::IndexAccess { lhs, index } => {
                let lhs_id = lhs.id;
                let lhs = self.emit_expr(lhs);
                let Type::Array(_) = self.env.get(&lhs_id).unwrap() else {
                    panic!("Should be an array");
                };
                let offset = self.emit_expr(index);
                Lval::IndexedDynamic(lhs, offset)
            }
            _ => panic!("Not lval"),
        }
    }
}
