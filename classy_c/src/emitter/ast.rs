use crate::{
    code::{constant_pool, Code, OpCode},
    syntax::ast,
};

pub struct AstEmmiter;

impl AstEmmiter {
    pub fn compile(program: ast::Program) -> Code {
        let emmiter = Self;
        let main = emmiter.find_main(&program);
        emmiter.emit_function(main)
    }
}

impl AstEmmiter {
    fn find_main<'a>(&self, program: &'a ast::Program) -> &'a ast::FunctionDefinition {
        for item in &program.items {
            match item {
                ast::TopLevelItem::FunctionDefinition(def) if def.name == "main" => return def,
                _ => {}
            }
        }
        panic!("Not main function found")
    }

    fn emit_function(&self, func: &ast::FunctionDefinition) -> Code {
        let mut c = Code::new();
        self.emit_expr(&mut c, &func.body);
        return c;
    }

    fn emit_expr(&self, code: &mut Code, expr: &ast::Expr) {
        match &expr.kind {
            ast::ExprKind::Unit => todo!(),
            ast::ExprKind::Sequence(seq) => {
                for expr in seq {
                    self.emit_expr(code, expr);
                    self.emit_instr(code, OpCode::Pop);
                }
            }
            ast::ExprKind::Assignment { .. } => todo!(),
            ast::ExprKind::IntConst(_) => todo!(),
            ast::ExprKind::StringConst(val) => {
                let id = code
                    .constant_pool
                    .add_entry(constant_pool::TypedEntry::String(val.clone()));
                self.emit_instr(code, OpCode::ConstLoadString);
                self.emit_word(code, id as u64);
            }
            ast::ExprKind::FloatConst(_) => todo!(),
            ast::ExprKind::Name(name) => {
                // todo: this is just plain wrong but we want something working
                // Actually this might be okay if we just somehow intern the strings?
                // we need better symbol names.
                let id = code
                    .constant_pool
                    .add_entry(constant_pool::TypedEntry::String(name.clone()));
                self.emit_instr(code, OpCode::LookUpGlobal);
                self.emit_word(code, id as u64);
            }
            ast::ExprKind::FunctionCall {
                func,
                args,
                kwargs: _kwargs,
            } => {
                if args.len() != 1 {
                    todo!()
                }
                self.emit_expr(code, func);
                self.emit_expr(code, &args[0]);
                self.emit_instr(code, OpCode::Call1);
            }
            ast::ExprKind::Access { .. } => todo!(),
            ast::ExprKind::Tuple(_) => todo!(),
            ast::ExprKind::Lambda { .. } => todo!(),
            ast::ExprKind::TypedExpr { .. } => todo!(),
            ast::ExprKind::StructLiteral { .. } => todo!(),
            ast::ExprKind::While { .. } => todo!(),
            ast::ExprKind::Return(expr) => {
                self.emit_expr(code, expr);
                self.emit_instr(code, OpCode::Return);
            }
            ast::ExprKind::If { .. } => todo!(),
            ast::ExprKind::Let { .. } => todo!(),
            ast::ExprKind::BoolConst(_) => todo!(),
            ast::ExprKind::AnonType { .. } => todo!(),
        }
    }

    fn emit_instr(&self, code: &mut Code, op: OpCode) {
        code.instructions.push(op as u8)
    }

    fn emit_word(&self, code: &mut Code, val: u64) {
        // saves as a low endian representaiton
        code.instructions.extend_from_slice(&val.to_le_bytes())
    }
}
