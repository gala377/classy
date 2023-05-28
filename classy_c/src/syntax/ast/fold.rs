use std::collections::HashMap;

use crate::syntax::ast::{
    Expr, FunctionDefinition, Path, Program, TopLevelItem, Typ, TypeDefinition, TypedName,
};

use super::{DefinedType, TypeVariable, ExprKind};

/// TODO: Not all travelsal methods are implemented yet.
pub trait Folder: Sized {
    fn fold_program(&mut self, program: Program) -> Program {
        fold_program(self, program)
    }

    fn fold_top_level_item(&mut self, item: TopLevelItem) -> TopLevelItem {
        fold_top_level_item(self, item)
    }

    fn fold_function_definition(&mut self, def: FunctionDefinition) -> FunctionDefinition {
        fold_function_definition(self, def)
    }

    fn fold_type_definition(&mut self, def: TypeDefinition) -> TypeDefinition {
        fold_type_definition(self, def)
    }

    fn fold_defined_type(&mut self, kind: DefinedType) -> DefinedType {
        kind
    }

    fn fold_expr(&mut self, expr: Expr) -> Expr {
        fold_expr(self, expr)
    }

    fn fold_typ(&mut self, typ: Typ) -> Typ {
        typ
    }

    fn fold_name(&mut self, name: String) -> String {
        name
    }

    fn fold_int_const(&mut self, val: isize) -> isize {
        val
    }

    fn fold_string_const(&mut self, val: String) -> String {
        val
    }

    fn fold_float_const(&mut self, val: f64) -> f64 {
        val
    }

    fn fold_sequence(&mut self, seq: Vec<Expr>) -> Vec<Expr> {
        fold_sequence(self, seq)
    }

    fn fold_unit(&mut self) {}

    fn fold_function_call(
        &mut self,
        func: Expr,
        args: Vec<Expr>,
        kwargs: HashMap<String, Expr>,
    ) -> ExprKind {
        fold_function_call(self, func, args, kwargs)
    }

    fn fold_access(&mut self, val: Expr, field: String) -> ExprKind {
        fold_access(self, val, field)
    }

    fn fold_tuple(&mut self, fields: Vec<Expr>) -> ExprKind {
        fold_tuple(self, fields)
    }

    fn fold_lambda(&mut self, params: Vec<TypedName>, body: Expr) -> ExprKind {
        fold_lambda(self, params, body)
    }

    fn fold_while(&mut self, cond: Expr, body: Expr) -> ExprKind {
        fold_while(self, cond, body)
    }

    fn fold_return(&mut self, expr: Expr) -> ExprKind {
        fold_return(self, expr)
    }

    fn fold_if(&mut self, cond: Expr, body: Expr, else_body: Option<Expr>) -> ExprKind {
        fold_if(self, cond, body, else_body)
    }

    fn fold_let(&mut self, name: String, typ: Typ, init: Expr) -> ExprKind {
        fold_let(self, name, typ, init)
    }

    fn fold_assignment(&mut self, lval: Expr, rval: Expr) -> ExprKind {
        fold_assignment(self, lval, rval)
    }

    fn fold_typed_expr(&mut self, expr: Expr, typ: Typ) -> ExprKind {
        fold_typed_expression(self, expr, typ)
    }

    fn fold_struct_literal(&mut self, strct: Path, values: HashMap<String, Expr>) -> ExprKind {
        fold_struct_literal(self, strct, values)
    }

    fn fold_bool_const(&mut self, val: bool) -> bool {
        val
    }

    fn fold_type_name(&mut self, name: String) -> String {
        name
    }

    fn fold_type_param(&mut self, param: String) -> String {
        param
    }

    fn fold_function_params(&mut self, params: Vec<String>) -> Vec<String> {
        params
    }

    fn fold_type_variables(&mut self, type_variables: Vec<TypeVariable>) -> Vec<TypeVariable> {
        type_variables
    }

    fn fold_function_args(&mut self, args: Vec<Expr>) -> Vec<Expr> {
        fold_function_args(self, args)
    }

    fn fold_anon_type(&mut self, fields: Vec<(String, Expr)>) -> ExprKind {
        fold_anon_type(self, fields)
    }
}

pub fn fold_program<F: Folder>(folder: &mut F, program: Program) -> Program {
    let mut new_items = Vec::new();
    for item in program.items {
        new_items.push(folder.fold_top_level_item(item));
    }
    Program { items: new_items }
}

pub fn fold_top_level_item<F: Folder>(folder: &mut F, item: TopLevelItem) -> TopLevelItem {
    match item {
        TopLevelItem::FunctionDefinition(def) => {
            TopLevelItem::FunctionDefinition(folder.fold_function_definition(def))
        }
        TopLevelItem::TypeDefinition(def) => {
            TopLevelItem::TypeDefinition(folder.fold_type_definition(def))
        }
    }
}

pub fn fold_function_definition<F: Folder>(
    folder: &mut F,
    def: FunctionDefinition,
) -> FunctionDefinition {
    FunctionDefinition {
        name: folder.fold_name(def.name),
        parameters: folder.fold_function_params(def.parameters),
        body: folder.fold_expr(def.body),
        typ: folder.fold_typ(def.typ),
    }
}

pub fn fold_type_definition<F: Folder>(folder: &mut F, def: TypeDefinition) -> TypeDefinition {
    TypeDefinition {
        name: folder.fold_type_name(def.name),
        definition: folder.fold_defined_type(def.definition),
        type_variables: folder.fold_type_variables(def.type_variables),
        span: def.span,
    }
}

pub fn fold_expr(folder: &mut impl Folder, expr: Expr) -> Expr {
    Expr {
        kind: fold_expr_kind(folder, expr.kind),
        id: expr.id,
    }
}

pub fn fold_expr_kind(folder: &mut impl Folder, expr: ExprKind) -> ExprKind {
    match expr {
        ExprKind::Unit => {
            folder.fold_unit();
            ExprKind::Unit
        }
        ExprKind::IntConst(val) => ExprKind::IntConst(folder.fold_int_const(val)),
        ExprKind::StringConst(val) => ExprKind::StringConst(folder.fold_string_const(val)),
        ExprKind::FloatConst(val) => ExprKind::FloatConst(folder.fold_float_const(val)),
        ExprKind::Name(name) => ExprKind::Name(folder.fold_name(name)),
        ExprKind::Sequence(seq) => ExprKind::Sequence(folder.fold_sequence(seq)),
        ExprKind::FunctionCall { func, args, kwargs } => folder.fold_function_call(*func, args, kwargs),
        ExprKind::Access { val, field } => folder.fold_access(*val, field),
        ExprKind::Tuple(fields) => folder.fold_tuple(fields),
        ExprKind::Lambda { parameters, body } => folder.fold_lambda(parameters, *body),
        ExprKind::While { cond, body } => folder.fold_while(*cond, *body),
        ExprKind::Return(expr) => folder.fold_return(*expr),
        ExprKind::If {
            cond,
            body,
            else_body,
        } => folder.fold_if(*cond, *body, else_body.map(|e| *e)),
        ExprKind::Let { name, typ, init } => folder.fold_let(name, typ, *init),
        ExprKind::Assignment { lval, rval } => folder.fold_assignment(*lval, *rval),
        ExprKind::TypedExpr { expr, typ } => folder.fold_typed_expr(*expr, typ),
        ExprKind::StructLiteral { strct, values } => folder.fold_struct_literal(strct, values),
        ExprKind::BoolConst(val) => ExprKind::BoolConst(folder.fold_bool_const(val)),
        ExprKind::AnonType { fields } => folder.fold_anon_type(fields),
    }
}

pub fn fold_sequence(folder: &mut impl Folder, seq: Vec<Expr>) -> Vec<Expr> {
    let mut new_seq = Vec::new();
    for expr in seq {
        new_seq.push(folder.fold_expr(expr));
    }
    new_seq
}

pub fn fold_if(folder: &mut impl Folder, cond: Expr, body: Expr, else_body: Option<Expr>) -> ExprKind {
    ExprKind::If {
        cond: Box::new(folder.fold_expr(cond)),
        body: Box::new(folder.fold_expr(body)),
        else_body: else_body.map(|e| Box::new(folder.fold_expr(e))),
    }
}

pub fn fold_while(folder: &mut impl Folder, cond: Expr, body: Expr) -> ExprKind {
    ExprKind::While {
        cond: Box::new(folder.fold_expr(cond)),
        body: Box::new(folder.fold_expr(body)),
    }
}

pub fn fold_assignment(folder: &mut impl Folder, lval: Expr, rval: Expr) -> ExprKind {
    ExprKind::Assignment {
        lval: Box::new(folder.fold_expr(lval)),
        rval: Box::new(folder.fold_expr(rval)),
    }
}

pub fn fold_access(folder: &mut impl Folder, val: Expr, field: String) -> ExprKind {
    ExprKind::Access {
        val: Box::new(folder.fold_expr(val)),
        field,
    }
}

pub fn fold_function_call(
    folder: &mut impl Folder,
    func: Expr,
    args: Vec<Expr>,
    kwargs: HashMap<String, Expr>,
) -> ExprKind {
    ExprKind::FunctionCall {
        func: Box::new(folder.fold_expr(func)),
        args: folder.fold_function_args(args),
        kwargs: kwargs
            .into_iter()
            .map(|(k, v)| (k, folder.fold_expr(v)))
            .collect(),
    }
}

pub fn fold_tuple(folder: &mut impl Folder, fields: Vec<Expr>) -> ExprKind {
    let mut new_vals = Vec::new();
    for val in fields {
        new_vals.push(folder.fold_expr(val));
    }
    ExprKind::Tuple(new_vals)
}

pub fn fold_lambda(folder: &mut impl Folder, params: Vec<TypedName>, body: Expr) -> ExprKind {
    ExprKind::Lambda {
        parameters: params,
        body: Box::new(folder.fold_expr(body)),
    }
}

pub fn fold_return(folder: &mut impl Folder, expr: Expr) -> ExprKind {
    ExprKind::Return(Box::new(folder.fold_expr(expr)))
}

pub fn fold_let(folder: &mut impl Folder, name: String, typ: Typ, init: Expr) -> ExprKind {
    ExprKind::Let {
        name,
        typ: folder.fold_typ(typ),
        init: Box::new(folder.fold_expr(init)),
    }
}

pub fn fold_typed_expression(folder: &mut impl Folder, expr: Expr, typ: Typ) -> ExprKind {
    ExprKind::TypedExpr {
        expr: Box::new(folder.fold_expr(expr)),
        typ: folder.fold_typ(typ),
    }
}

pub fn fold_struct_literal(
    folder: &mut impl Folder,
    strct: Path,
    values: HashMap<String, Expr>,
) -> ExprKind {
    ExprKind::StructLiteral {
        strct,
        values: values
            .into_iter()
            .map(|(k, v)| (k, folder.fold_expr(v)))
            .collect(),
    }
}

pub fn fold_function_args(folder: &mut impl Folder, args: Vec<Expr>) -> Vec<Expr> {
    let mut new_args = Vec::new();
    for arg in args {
        new_args.push(folder.fold_expr(arg));
    }
    new_args
}

pub fn fold_anon_type(folder: &mut impl Folder, fields: Vec<(String, Expr)>) -> ExprKind {
    let mut new_fields = Vec::new();
    for (name, val) in fields {
        new_fields.push((name, folder.fold_expr(val)));
    }
    ExprKind::AnonType { fields: new_fields }
}
