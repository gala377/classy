use std::collections::HashMap;

use crate::syntax::ast;

pub trait Visitor<'ast>: Sized {
    fn visit(&mut self, node: &'ast ast::Program) {
        walk_program(self, node)
    }

    fn visit_top_level_item(&mut self, node: &'ast ast::TopLevelItem) {
        walk_top_level_item(self, node)
    }

    fn visit_fn_def(&mut self, def: &'ast ast::FunctionDefinition) {
        walk_function_def(self, def)
    }
    
    fn visit_type_def(&mut self, _node: &'ast ast::TypeDefinition) {}

    fn visit_expr(&mut self, node: &'ast ast::Expr) {
        walk_expr(self, node)
    }

    fn visit_sequence(&mut self, seq: &'ast [ast::Expr]) {
        walk_seq_expr(self, seq)
    }
    
    fn visit_function_call(
        &mut self,
        func: &'ast ast::Expr,
        args: &'ast [ast::Expr],
        kwargs: &'ast HashMap<String, ast::Expr>,
    ) {
        walk_function_call(self, func, args, kwargs)
    }
    
    fn visit_access(&mut self, val: &'ast ast::Expr, field: &'ast str) {
        walk_access(self, val, field)
    }
    
    fn visit_tuple(&mut self, fields: &'ast [ast::Expr]) {
        walk_tuple(self, fields)
    }
    
    fn visit_lambda(&mut self, params: &'ast [ast::TypedName], body: &'ast ast::Expr) {
        walk_lambda(self, params, body)
    }
    
    fn visit_while(&mut self, cond: &'ast ast::Expr, body: &'ast ast::Expr) {
        walk_while(self, cond, body)
    }
    
    fn visit_return(&mut self, expr: &'ast ast::Expr) {
        walk_expr(self, expr)
    }
    
    fn visit_if(
        &mut self,
        cond: &'ast ast::Expr,
        body: &'ast ast::Expr,
        else_body: Option<&'ast ast::Expr>,
    ) {
        walk_if(self, cond, body, else_body)
    }
    
    fn visit_let(&mut self, name: &'ast str, typ: &'ast ast::Typ, init: &'ast ast::Expr) {
        walk_let(self, name, typ, init)
    }
    
    fn visit_assignment(&mut self, lval: &'ast ast::Expr, rval: &'ast ast::Expr) {
        walk_assignment(self, lval, rval)
    }
    
    fn visit_typed_expr(&mut self, expr: &'ast ast::Expr, typ: &'ast ast::Typ) {
        walk_typed_expression(self, expr, typ)
    }

    fn visit_struct_literal(
        &mut self,
        strct: &'ast ast::Path,
        values: &'ast HashMap<String, ast::Expr>,
    ) {
        walk_struct_literal(self, strct, values);
    }


    // leaf nodes
    fn visit_typ(&mut self, _node: &'ast ast::Typ) {}
    fn visit_name(&mut self, _node: &'ast str) {}
    fn visit_int_const(&mut self, _node: isize) {}
    fn visit_bool_const(&mut self, _val: bool) {}
    fn visit_string_const(&mut self, _node: &'ast str) {}
    fn visit_float_const(&mut self, _node: f64) {}
    fn visit_unit(&mut self) {}
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
        ast::Expr::Unit => {
            v.visit_unit();
        }
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
        ast::Expr::FunctionCall { func, args, kwargs } => {
            v.visit_function_call(func, args, kwargs);
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
        ast::Expr::BoolConst(val) => v.visit_bool_const(*val),
    }
}

pub fn walk_seq_expr<'ast, V: Visitor<'ast>>(v: &mut V, seq: &'ast [ast::Expr]) {
    for expr in seq {
        v.visit_expr(expr);
    }
}

pub fn walk_function_call<'ast, V: Visitor<'ast>>(
    v: &mut V,
    func: &'ast ast::Expr,
    args: &'ast [ast::Expr],
    kwargs: &'ast HashMap<String, ast::Expr>,
) {
    v.visit_expr(func);
    for arg in args {
        v.visit_expr(arg);
    }
    for arg in kwargs.values() {
        v.visit_expr(arg);
    }
}

pub fn walk_let<'ast, V: Visitor<'ast>>(
    v: &mut V,
    name: &'ast str,
    typ: &'ast ast::Typ,
    init: &'ast ast::Expr,
) {
    v.visit_name(name);
    v.visit_typ(typ);
    v.visit_expr(init)
}
pub fn walk_struct_literal<'ast, V: Visitor<'ast>>(
    v: &mut V,
    strct: &'ast ast::Path,
    values: &'ast HashMap<String, ast::Expr>,
) {
    // TODO: change when we handle proper paths
    v.visit_name(&strct.0[0]);
    for val in values.values() {
        v.visit_expr(val);
    }
}

pub fn walk_return<'ast, V: Visitor<'ast>>(v: &mut V, expr: &'ast ast::Expr) {
    v.visit_expr(expr);
}

pub fn walk_while<'ast, V: Visitor<'ast>>(v: &mut V, cond: &'ast ast::Expr, body: &'ast ast::Expr) {
    v.visit_expr(cond);
    v.visit_expr(body);
}

pub fn walk_if<'ast, V: Visitor<'ast>>(
    v: &mut V,
    cond: &'ast ast::Expr,
    body: &'ast ast::Expr,
    else_body: Option<&'ast ast::Expr>,
) {
    v.visit_expr(cond);
    v.visit_expr(body);
    if let Some(else_body) = else_body {
        v.visit_expr(else_body);
    }
}

pub fn walk_access<'ast, V: Visitor<'ast>>(v: &mut V, val: &'ast ast::Expr, field: &'ast str) {
    v.visit_expr(val);
    v.visit_name(field);
}

pub fn walk_typed_expression<'ast, V: Visitor<'ast>>(
    v: &mut V,
    expr: &'ast ast::Expr,
    typ: &'ast ast::Typ,
) {
    v.visit_expr(expr);
    v.visit_typ(typ);
}

pub fn walk_assignment<'ast, V: Visitor<'ast>>(
    v: &mut V,
    lval: &'ast ast::Expr,
    rval: &'ast ast::Expr,
) {
    v.visit_expr(lval);
    v.visit_expr(rval);
}

pub fn walk_tuple<'ast, V: Visitor<'ast>>(v: &mut V, fields: &'ast [ast::Expr]) {
    for field in fields {
        v.visit_expr(field);
    }
}

pub fn walk_lambda<'ast, V: Visitor<'ast>>(
    v: &mut V,
    params: &'ast [ast::TypedName],
    body: &'ast ast::Expr,
) {
    for param in params {
        v.visit_name(&param.name);
        v.visit_typ(&param.typ);
    }
    v.visit_expr(body);
}

pub fn walk_function_def<'ast, V: Visitor<'ast>>(
    v: &mut V,
    def: &'ast ast::FunctionDefinition,
) {
    v.visit_name(&def.name);
    v.visit_typ(&def.typ);
    for param in &def.parameters {
        v.visit_name(&param);
    }
    v.visit_expr(&def.body);
}