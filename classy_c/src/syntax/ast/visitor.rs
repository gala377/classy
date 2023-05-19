use std::collections::HashMap;

use crate::syntax::ast;

pub trait Visitor<'ast>: Sized {
    fn visit(&mut self, node: &'ast ast::Program) {
        walk_program(self, node)
    }

    fn visit_top_level_item(&mut self, node: &'ast ast::TopLevelItem) {
        walk_top_level_item(self, node)
    }

    fn visit_fn_def(&mut self, _node: &'ast ast::FunctionDefinition) {
        todo!()
    }
    fn visit_type_def(&mut self, _node: &'ast ast::TypeDefinition) {
        todo!()
    }

    fn visit_expr(&mut self, _node: &'ast ast::Expr) {}

    fn visit_typ(&mut self, _node: &'ast ast::Typ) {}
    fn visit_name(&mut self, _node: &'ast str) {}
    fn visit_int_const(&mut self, _node: isize) {}
    fn visit_string_const(&mut self, _node: &'ast str) {}
    fn visit_float_const(&mut self, _node: f64) {}
    fn visit_sequence(&mut self, seq: &'ast [ast::Expr]) {
        walk_seq_expr(self, seq)
    }
    fn visit_unit(&mut self) {}
    fn visit_function_call(&mut self, _func: &'ast ast::Expr, _args: &'ast [ast::Expr]) {}
    fn visit_access(&mut self, _val: &'ast ast::Expr, _field: &'ast str) {}
    fn visit_tuple(&mut self, _fields: &'ast [ast::Expr]) {}
    fn visit_lambda(&mut self, _params: &'ast [ast::TypedName], _body: &'ast ast::Expr) {}
    fn visit_while(&mut self, _cond: &'ast ast::Expr, _body: &'ast ast::Expr) {}
    fn visit_return(&mut self, _expr: &'ast ast::Expr) {}
    fn visit_if(
        &mut self,
        _cond: &'ast ast::Expr,
        _body: &'ast ast::Expr,
        _else_body: Option<&'ast ast::Expr>,
    ) {
    }
    fn visit_let(&mut self, _name: &'ast str, _typ: &'ast ast::Typ, _init: &'ast ast::Expr) {}
    fn visit_assignment(&mut self, _lval: &'ast ast::Expr, _rval: &'ast ast::Expr) {}
    fn visit_typed_expr(&mut self, _expr: &'ast ast::Expr, _typ: &'ast ast::Typ) {}
    fn visit_struct_literal(
        &mut self,
        _strct: &'ast ast::Path,
        _values: &'ast HashMap<String, ast::Expr>,
    ) {
    }
}

pub fn walk_program<'ast, V: Visitor<'ast>>(v: &mut V, node: &'ast ast::Program) {
    for item in &node.items {
        v.visit_top_level_item(item)
    }
}

pub fn walk_top_level_item<'ast, V: Visitor<'ast>>(v: &mut V, node: &'ast ast::TopLevelItem) {
    match node {
        ast::TopLevelItem::FunctionDefinition(fn_def) => v.visit_fn_def(fn_def),
        ast::TopLevelItem::TypeDefinition(t_def) => v.visit_type_def(t_def),
    }
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(v: &mut V, node: &'ast ast::Expr) {
    match node {
        ast::Expr::Unit => {}
        ast::Expr::Sequence(seq) => {
            v.visit_sequence(seq);
        }
        ast::Expr::Assignment { lval, rval } => {
            v.visit_expr(lval);
            v.visit_expr(rval);
        }
        ast::Expr::IntConst(val) => {
            v.visit_int_const(*val);
        }
        ast::Expr::StringConst(val) => {
            v.visit_string_const(val);
        }
        ast::Expr::FloatConst(val) => {
            v.visit_float_const(*val);
        }
        ast::Expr::Name(name) => v.visit_name(name),
        ast::Expr::FunctionCall { func, args } => {
            v.visit_function_call(func, args);
        }
        ast::Expr::Access { val, field } => {
            v.visit_access(val, field);
        }
        ast::Expr::Tuple(val) => {
            v.visit_tuple(val);
        }
        ast::Expr::Lambda { parameters, body } => {
            v.visit_lambda(parameters, body);
        }
        ast::Expr::TypedExpr { expr, typ } => {
            v.visit_typed_expr(expr, typ);
        }
        ast::Expr::StructLiteral { strct, values } => {
            v.visit_struct_literal(strct, values);
        }
        ast::Expr::While { cond, body } => {
            v.visit_while(cond, body);
        }
        ast::Expr::Return(val) => {
            v.visit_return(val);
        }
        ast::Expr::If {
            cond,
            body,
            else_body,
        } => {
            let else_body = else_body.as_ref().map(|e| e.as_ref());
            v.visit_if(cond, body, else_body);
        }
        ast::Expr::Let { name, typ, init } => {
            v.visit_let(name, typ, init);
        }
    }
}

pub fn walk_seq_expr<'ast, V: Visitor<'ast>>(v: &mut V, seq: &'ast [ast::Expr]) {
    for expr in seq {
        v.visit_expr(expr);
    }
}
