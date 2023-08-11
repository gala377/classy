use std::collections::HashMap;

use crate::ast;

pub trait Visitor<'ast>: Sized {
    fn visit(&mut self, node: &'ast ast::SourceFile) {
        walk_program(self, node)
    }

    fn visit_identifier(&mut self, _node: &'ast str) {}

    fn visit_top_level_item(&mut self, node: &'ast ast::TopLevelItem) {
        walk_top_level_item(self, node)
    }

    fn visit_top_level_item_kind(&mut self, node: &'ast ast::TopLevelItemKind) {
        walk_top_level_item_kind(self, node)
    }

    fn visit_fn_def(&mut self, def: &'ast ast::FunctionDefinition) {
        walk_function_def(self, def)
    }

    fn visit_const_definition(&mut self, def: &'ast ast::ConstDefinition) {
        walk_const_definition(self, def)
    }

    fn visit_local_function_def(&mut self, def: &'ast ast::FunctionDefinition) {
        walk_local_function_def(self, def)
    }

    fn visit_methods_block(&mut self, meth: &'ast ast::MethodsBlock<ast::FunctionDefinition>) {
        walk_methods_block(self, meth)
    }

    fn visit_type_def(&mut self, _node: &'ast ast::TypeDefinition) {}

    fn visit_expr(&mut self, node: &'ast ast::Expr) {
        walk_expr(self, node)
    }

    fn visit_expr_kind(&mut self, node: &'ast ast::ExprKind) {
        walk_expr_kind(self, node)
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

    fn visit_method_call(
        &mut self,
        receiver: &'ast ast::Expr,
        method: &'ast str,
        args: &'ast [ast::Expr],
        kwargs: &'ast HashMap<String, ast::Expr>,
    ) {
        walk_method_call(self, receiver, method, args, kwargs)
    }

    fn visit_access(&mut self, val: &'ast ast::Expr, field: &'ast str) {
        walk_access(self, val, field)
    }

    fn visit_tuple(&mut self, fields: &'ast [ast::Expr]) {
        walk_tuple(self, fields)
    }

    fn visit_lambda(&mut self, params: &'ast [ast::TypedIdentifier], body: &'ast ast::Expr) {
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

    fn visit_let_rec(&mut self, definitions: &'ast [ast::FunctionDefinition]) {
        walk_let_rec(self, definitions)
    }

    fn visit_assignment(&mut self, lval: &'ast ast::Expr, rval: &'ast ast::Expr) {
        walk_assignment(self, lval, rval)
    }

    fn visit_typed_expr(&mut self, expr: &'ast ast::Expr, typ: &'ast ast::Typ) {
        walk_typed_expression(self, expr, typ)
    }

    fn visit_struct_literal(
        &mut self,
        strct: &'ast ast::Name,
        values: &'ast HashMap<String, ast::Expr>,
    ) {
        walk_struct_literal(self, strct, values);
    }

    fn visit_adt_struct_constructor(
        &mut self,
        typ: &'ast ast::Name,
        case: &'ast str,
        values: &'ast [(String, ast::Expr)],
    ) {
        walk_adt_struct_constructor(self, typ, case, values);
    }

    fn visit_adt_tuple_constructor(
        &mut self,
        typ: &'ast ast::Name,
        case: &'ast str,
        values: &'ast [ast::Expr],
    ) {
        walk_adt_tuple_constructor(self, typ, case, values);
    }

    fn visit_adt_unit_constructor(&mut self, typ: &'ast ast::Name, case: &'ast str) {
        walk_adt_unit_constructor(self, typ, case);
    }

    fn visit_anon_type(&mut self, fields: &'ast [(String, ast::Expr)]) {
        walk_anon_type(self, fields);
    }

    fn visit_array(&mut self, size: &'ast ast::Expr, typ: &'ast ast::Typ, init: &'ast [ast::Expr]) {
        walk_array(self, size, typ, init)
    }

    fn visit_index_access(&mut self, lhs: &'ast ast::Expr, index: &'ast ast::Expr) {
        walk_index_access(self, lhs, index)
    }

    fn visit_match(
        &mut self,
        expr: &'ast ast::Expr,
        cases: &'ast [(ast::Pattern, ast::Expr, Option<Box<ast::Expr>>)],
    ) {
        walk_match(self, expr, cases)
    }

    fn visit_pattern(&mut self, pattern: &'ast ast::Pattern) {
        walk_pattern(self, pattern)
    }

    fn visit_pattern_kind(&mut self, pattern: &'ast ast::PatternKind) {
        walk_pattern_kind(self, pattern)
    }

    fn visit_tuple_pattern(&mut self, fields: &'ast [ast::Pattern]) {
        walk_tuple_pattern(self, fields)
    }
    fn visit_array_pattern(&mut self, fields: &'ast [ast::Pattern]) {
        walk_array_pattern(self, fields)
    }
    fn visit_struct_pattern(
        &mut self,
        strct: &'ast ast::Name,
        fields: &'ast HashMap<String, ast::Pattern>,
    ) {
        walk_struct_pattern(self, strct, fields)
    }
    fn visit_tuple_struct_pattern(&mut self, strct: &'ast ast::Name, fields: &'ast [ast::Pattern]) {
        walk_tuple_struct_pattern(self, strct, fields)
    }

    fn visit_type_specific_pattern(&mut self, name: &'ast ast::Name, pattern: &'ast ast::Pattern) {
        walk_type_specific_pattern(self, name, pattern)
    }

    fn visit_anon_struct_pattern(&mut self, fields: &'ast HashMap<String, ast::Pattern>) {
        walk_anon_struct_pattern(self, fields)
    }

    fn visit_unit_pattern(&mut self) {}
    fn visit_rest_pattern(&mut self, _name: &'ast str) {}
    fn visit_wildcard_pattern(&mut self) {}
    fn visit_name_pattern(&mut self, _name: &'ast str) {}
    fn visit_int_pattern(&mut self, _val: isize) {}
    fn visit_bool_pattern(&mut self, _val: bool) {}
    fn visit_string_pattern(&mut self, _val: &'ast str) {}

    // leaf nodes
    fn visit_typ(&mut self, _node: &'ast ast::Typ) {}
    fn visit_name(&mut self, _node: &'ast ast::Name) {}
    fn visit_int_const(&mut self, _node: isize) {}
    fn visit_bool_const(&mut self, _val: bool) {}
    fn visit_string_const(&mut self, _node: &'ast str) {}
    fn visit_float_const(&mut self, _node: f64) {}
    fn visit_unit(&mut self) {}

    fn visit_method(&mut self, node: &'ast ast::FunctionDefinition) {
        walk_method(self, node)
    }

    fn visit_instance_definition(
        &mut self,
        ast::InstanceDefinition {
            name,
            free_variables,
            bounds,
            instanced_class,
            body,
        }: &'ast ast::InstanceDefinition,
    ) {
        walk_instance_definition(
            self,
            name.as_ref(),
            free_variables,
            bounds,
            instanced_class,
            body,
        )
    }

    fn visit_instance_definition_item(&mut self, item: &'ast ast::InstanceDefinitionItem) {
        walk_instance_definition_item(self, item)
    }

    fn visit_class_definition(
        &mut self,
        ast::ClassDefinition {
            name,
            bounds,
            args,
            body,
        }: &'ast ast::ClassDefinition,
    ) {
        walk_class_definition(self, name, args, bounds, body)
    }

    fn visit_class_definition_item(&mut self, item: &'ast ast::ClassDefinitionItem) {
        walk_class_definition_item(self, item)
    }

    fn visit_class_function_decl(&mut self, decl: &'ast ast::FuncDecl) {
        walk_class_function_decl(self, decl)
    }

    fn visit_class_methods_block(&mut self, block: &'ast ast::MethodsBlock<ast::FuncDecl>) {
        walk_class_methods_block(self, block)
    }

    fn visit_class_methods_block_method(&mut self, method: &'ast ast::FuncDecl) {
        walk_class_methods_block_method(self, method)
    }

    fn visit_type_bound(&mut self, head: &'ast ast::Name, args: &'ast [ast::Typ]) {
        walk_type_bound(self, head, args)
    }
}

pub fn walk_program<'ast, V: Visitor<'ast>>(v: &mut V, node: &'ast ast::SourceFile) {
    for item in &node.items {
        v.visit_top_level_item(item)
    }
}

pub fn walk_top_level_item_kind<'ast, V: Visitor<'ast>>(
    v: &mut V,
    node: &'ast ast::TopLevelItemKind,
) {
    match node {
        ast::TopLevelItemKind::FunctionDefinition(fn_def) => v.visit_fn_def(fn_def),
        ast::TopLevelItemKind::TypeDefinition(t_def) => v.visit_type_def(t_def),
        ast::TopLevelItemKind::MethodsBlock(meth) => v.visit_methods_block(meth),
        ast::TopLevelItemKind::ConstDefinition(def) => v.visit_const_definition(def),
        t => todo!("{t:?}"),
    }
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(v: &mut V, node: &'ast ast::Expr) {
    v.visit_expr_kind(&node.kind)
}

pub fn walk_expr_kind<'ast, V: Visitor<'ast>>(v: &mut V, node: &'ast ast::ExprKind) {
    match node {
        ast::ExprKind::Unit => {
            v.visit_unit();
        }
        ast::ExprKind::Sequence(seq) => {
            v.visit_sequence(seq);
        }
        ast::ExprKind::Assignment { lval, rval } => {
            v.visit_expr(lval);
            v.visit_expr(rval);
        }
        ast::ExprKind::IntConst(val) => {
            v.visit_int_const(*val);
        }
        ast::ExprKind::StringConst(val) => {
            v.visit_string_const(val);
        }
        ast::ExprKind::FloatConst(val) => {
            v.visit_float_const(*val);
        }
        ast::ExprKind::Name(name) => v.visit_name(name),
        ast::ExprKind::FunctionCall { func, args, kwargs } => {
            v.visit_function_call(func, args, kwargs);
        }
        ast::ExprKind::Access { val, field } => {
            v.visit_access(val, field);
        }
        ast::ExprKind::Tuple(val) => {
            v.visit_tuple(val);
        }
        ast::ExprKind::Lambda { parameters, body } => {
            v.visit_lambda(parameters, body);
        }
        ast::ExprKind::TypedExpr { expr, typ } => {
            v.visit_typed_expr(expr, typ);
        }
        ast::ExprKind::StructLiteral { strct, values } => {
            v.visit_struct_literal(strct, values);
        }

        ast::ExprKind::While { cond, body } => {
            v.visit_while(cond, body);
        }
        ast::ExprKind::Return(val) => {
            v.visit_return(val);
        }
        ast::ExprKind::If {
            cond,
            body,
            else_body,
        } => {
            let else_body = else_body.as_ref().map(|e| e.as_ref());
            v.visit_if(cond, body, else_body);
        }
        ast::ExprKind::Let { name, typ, init } => {
            v.visit_let(name, typ, init);
        }
        ast::ExprKind::BoolConst(val) => v.visit_bool_const(*val),
        ast::ExprKind::AnonType { fields } => v.visit_anon_type(fields),
        ast::ExprKind::ArrayLiteral { typ, size, init } => v.visit_array(size, typ, init),
        ast::ExprKind::IndexAccess { lhs, index } => v.visit_index_access(lhs, index),
        ast::ExprKind::Match { expr, cases } => v.visit_match(expr, cases),
        ast::ExprKind::AdtStructConstructor {
            typ,
            constructor,
            fields,
        } => v.visit_adt_struct_constructor(typ, constructor, fields),
        ast::ExprKind::AdtTupleConstructor {
            typ,
            constructor,
            args,
        } => v.visit_adt_tuple_constructor(typ, constructor, args),
        ast::ExprKind::AdtUnitConstructor { typ, constructor } => {
            v.visit_adt_unit_constructor(typ, constructor)
        }
        ast::ExprKind::MethodCall {
            receiver,
            method,
            args,
            kwargs,
        } => v.visit_method_call(receiver, method, args, kwargs),
        ast::ExprKind::LetRec { definitions } => v.visit_let_rec(definitions),
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
    _name: &'ast str,
    typ: &'ast ast::Typ,
    init: &'ast ast::Expr,
) {
    v.visit_typ(typ);
    v.visit_expr(init)
}
pub fn walk_struct_literal<'ast, V: Visitor<'ast>>(
    v: &mut V,
    strct: &'ast ast::Name,
    values: &'ast HashMap<String, ast::Expr>,
) {
    // TODO: change when we handle proper paths
    v.visit_name(strct);
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

pub fn walk_access<'ast, V: Visitor<'ast>>(v: &mut V, val: &'ast ast::Expr, _field: &'ast str) {
    v.visit_expr(val);
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
    params: &'ast [ast::TypedIdentifier],
    body: &'ast ast::Expr,
) {
    for param in params {
        v.visit_typ(&param.typ);
    }
    v.visit_expr(body);
}

pub fn walk_function_def<'ast, V: Visitor<'ast>>(v: &mut V, def: &'ast ast::FunctionDefinition) {
    v.visit_typ(&def.typ);
    v.visit_expr(&def.body);
}

pub fn walk_method<'ast, V: Visitor<'ast>>(v: &mut V, def: &'ast ast::FunctionDefinition) {
    v.visit_typ(&def.typ);
    v.visit_expr(&def.body);
}

pub fn walk_anon_type<'ast, V: Visitor<'ast>>(v: &mut V, fields: &'ast [(String, ast::Expr)]) {
    for (_name, expr) in fields {
        v.visit_expr(expr);
    }
}

pub fn walk_array<'ast, V: Visitor<'ast>>(
    v: &mut V,
    size: &'ast ast::Expr,
    typ: &'ast ast::Typ,
    init: &'ast [ast::Expr],
) {
    v.visit_typ(typ);
    v.visit_expr(size);
    for expr in init {
        v.visit_expr(expr);
    }
}

pub fn walk_index_access<'ast, V: Visitor<'ast>>(
    v: &mut V,
    lhs: &'ast ast::Expr,
    index: &'ast ast::Expr,
) {
    v.visit_expr(lhs);
    v.visit_expr(index);
}

pub fn walk_match<'ast, V: Visitor<'ast>>(
    v: &mut V,
    expr: &'ast ast::Expr,
    cases: &'ast [(ast::Pattern, ast::Expr, Option<Box<ast::Expr>>)],
) {
    v.visit_expr(expr);
    for (pattern, expr, guard) in cases {
        v.visit_pattern(pattern);
        v.visit_expr(expr);
        if let Some(guard) = guard {
            v.visit_expr(guard);
        }
    }
}

pub fn walk_pattern_kind<'ast, V: Visitor<'ast>>(v: &mut V, pattern: &'ast ast::PatternKind) {
    match pattern {
        ast::PatternKind::Var(n) => v.visit_name_pattern(n),
        ast::PatternKind::Tuple(fs) => v.visit_tuple_pattern(fs),
        ast::PatternKind::Struct { strct, fields } => v.visit_struct_pattern(strct, fields),
        ast::PatternKind::TupleStruct { strct, fields } => {
            v.visit_tuple_struct_pattern(strct, fields)
        }
        ast::PatternKind::Array(fs) => v.visit_array_pattern(fs),
        ast::PatternKind::Wildcard => v.visit_wildcard_pattern(),
        ast::PatternKind::Unit => v.visit_unit_pattern(),
        ast::PatternKind::String(s) => v.visit_string_pattern(s),
        ast::PatternKind::Int(i) => v.visit_int_pattern(*i),
        ast::PatternKind::Bool(b) => v.visit_bool_pattern(*b),
        ast::PatternKind::Rest(n) => v.visit_rest_pattern(n),
        ast::PatternKind::TypeSpecifier(name, pattern) => {
            v.visit_type_specific_pattern(name, pattern)
        }
        ast::PatternKind::AnonStruct { fields } => v.visit_anon_struct_pattern(fields),
    }
}

pub fn walk_tuple_pattern<'ast, V: Visitor<'ast>>(v: &mut V, fields: &'ast [ast::Pattern]) {
    for field in fields {
        v.visit_pattern(field);
    }
}

pub fn walk_array_pattern<'ast, V: Visitor<'ast>>(v: &mut V, fields: &'ast [ast::Pattern]) {
    for field in fields {
        v.visit_pattern(field);
    }
}

pub fn walk_struct_pattern<'ast, V: Visitor<'ast>>(
    v: &mut V,
    strct: &'ast ast::Name,
    fields: &'ast HashMap<String, ast::Pattern>,
) {
    v.visit_name(strct);
    for field in fields.values() {
        v.visit_pattern(field);
    }
}

pub fn walk_tuple_struct_pattern<'ast, V: Visitor<'ast>>(
    v: &mut V,
    strct: &'ast ast::Name,
    fields: &'ast [ast::Pattern],
) {
    v.visit_name(strct);
    for field in fields {
        v.visit_pattern(field);
    }
}

pub fn walk_type_specific_pattern<'ast, V: Visitor<'ast>>(
    v: &mut V,
    name: &'ast ast::Name,
    pattern: &'ast ast::Pattern,
) {
    v.visit_name(name);
    v.visit_pattern(pattern);
}

pub fn walk_adt_struct_constructor<'ast, V: Visitor<'ast>>(
    v: &mut V,
    typ: &'ast ast::Name,
    _case: &'ast str,
    values: &'ast [(String, ast::Expr)],
) {
    v.visit_name(typ);
    for (_name, expr) in values {
        v.visit_expr(expr);
    }
}

pub fn walk_adt_tuple_constructor<'ast, V: Visitor<'ast>>(
    v: &mut V,
    typ: &'ast ast::Name,
    case: &'ast str,
    values: &'ast [ast::Expr],
) {
    v.visit_name(typ);
    v.visit_identifier(case);
    for expr in values {
        v.visit_expr(expr);
    }
}

pub fn walk_adt_unit_constructor<'ast, V: Visitor<'ast>>(
    v: &mut V,
    typ: &'ast ast::Name,
    case: &'ast str,
) {
    v.visit_name(typ);
    v.visit_identifier(case);
}

pub fn walk_anon_struct_pattern<'ast, V: Visitor<'ast>>(
    v: &mut V,
    fields: &'ast HashMap<String, ast::Pattern>,
) {
    for (_name, pattern) in fields {
        v.visit_pattern(pattern);
    }
}

pub fn walk_pattern<'ast, V: Visitor<'ast>>(v: &mut V, pattern: &'ast ast::Pattern) {
    v.visit_pattern_kind(&pattern.kind)
}

fn walk_methods_block<'ast, V: Visitor<'ast>>(
    v: &mut V,
    meth: &'ast ast::MethodsBlock<ast::FunctionDefinition>,
) {
    v.visit_typ(&meth.typ);
    for method in &meth.methods {
        v.visit_method(method);
    }
}

pub fn walk_method_call<'ast, V: Visitor<'ast>>(
    v: &mut V,
    receiver: &'ast ast::Expr,
    method: &'ast str,
    args: &'ast [ast::Expr],
    kwargs: &'ast HashMap<String, ast::Expr>,
) {
    v.visit_expr(receiver);
    v.visit_identifier(method);
    for arg in args {
        v.visit_expr(arg);
    }
    for arg in kwargs.values() {
        v.visit_expr(arg);
    }
}

pub fn walk_const_definition<'ast, V: Visitor<'ast>>(v: &mut V, def: &'ast ast::ConstDefinition) {
    v.visit_identifier(&def.name);
    v.visit_typ(&def.typ);
    v.visit_expr(&def.init);
}

pub fn walk_local_function_def<'ast, V: Visitor<'ast>>(
    v: &mut V,
    def: &'ast ast::FunctionDefinition,
) {
    v.visit_identifier(&def.name);
    v.visit_typ(&def.typ);
    for name in &def.parameters {
        v.visit_identifier(&name);
    }
    v.visit_expr(&def.body);
}

pub fn walk_let_rec<'ast, V: Visitor<'ast>>(
    v: &mut V,
    definitions: &'ast [ast::FunctionDefinition],
) {
    for def in definitions {
        v.visit_local_function_def(def);
    }
}

pub fn walk_top_level_item<'ast, V: Visitor<'ast>>(v: &mut V, node: &'ast ast::TopLevelItem) {
    v.visit_top_level_item_kind(&node.kind)
}

fn walk_instance_definition<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    name: Option<&'ast String>,
    _free_variables: &'ast [String],
    bounds: &'ast [ast::TypeBound],
    ast::TypeBound {
        head: def_name,
        args: def_args,
    }: &'ast ast::TypeBound,
    body: &'ast [ast::InstanceDefinitionItem],
) {
    name.map(|n| visitor.visit_identifier(n));
    for ast::TypeBound { head, args } in bounds {
        visitor.visit_type_bound(head, args);
    }
    visitor.visit_type_bound(def_name, def_args);
    for item in body {
        visitor.visit_instance_definition_item(item);
    }
}

fn walk_instance_definition_item<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    item: &'ast ast::InstanceDefinitionItem,
) {
    match item {
        ast::InstanceDefinitionItem::FunctionDefinition(def) => visitor.visit_fn_def(def),
        ast::InstanceDefinitionItem::MethodsBlock(methods) => visitor.visit_methods_block(methods),
    }
}

fn walk_class_definition<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    name: &'ast str,
    _args: &'ast [String],
    bounds: &'ast [ast::TypeBound],
    body: &'ast [ast::ClassDefinitionItem],
) {
    visitor.visit_identifier(name);
    for ast::TypeBound { head, args } in bounds {
        visitor.visit_type_bound(head, args);
    }
    for item in body {
        visitor.visit_class_definition_item(item);
    }
}

fn walk_class_definition_item<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    item: &'ast ast::ClassDefinitionItem,
) {
    match item {
        ast::ClassDefinitionItem::Function(def) => visitor.visit_class_function_decl(def),
        ast::ClassDefinitionItem::MethodBlock(methods) => {
            visitor.visit_class_methods_block(methods)
        }
    }
}

fn walk_class_function_decl<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    ast::FuncDecl { name, typ }: &'ast ast::FuncDecl,
) {
    visitor.visit_identifier(name);
    visitor.visit_typ(typ);
}

fn walk_class_methods_block<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    ast::MethodsBlock { name, typ, methods }: &'ast ast::MethodsBlock<ast::FuncDecl>,
) {
    for name in name {
        visitor.visit_identifier(name);
    }
    visitor.visit_typ(typ);
    for meth in methods {
        visitor.visit_class_methods_block_method(meth);
    }
}

fn walk_class_methods_block_method<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    ast::FuncDecl { name, typ }: &'ast ast::FuncDecl,
) {
    visitor.visit_identifier(name);
    visitor.visit_typ(typ);
}

fn walk_type_bound<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    head: &'ast ast::Name,
    args: &'ast [ast::Typ],
) {
    visitor.visit_name(head);
    for arg in args {
        visitor.visit_typ(arg);
    }
}
