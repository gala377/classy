use std::collections::HashMap;

use super::{Expr, ExprKind, Name, Pattern, PatternKind, Typ, TypedIdentifier};

pub trait AstExprTransformer: Sized {
    type Expr;
    type ExprKind;
    type Pattern;
    type PatternKind;

    fn transform_expr(&mut self, expr: Expr) -> Self::Expr;
    fn transform_expr_kind(&mut self, expr: ExprKind) -> Self::ExprKind {
        transform_expr_kind::<Self>(self, expr)
    }
    fn transform_unit(&mut self) -> Self::ExprKind;
    fn transform_int_const(&mut self, val: isize) -> Self::ExprKind;
    fn transform_string_const(&mut self, val: String) -> Self::ExprKind;
    fn transform_float_const(&mut self, val: f64) -> Self::ExprKind;
    fn transform_name(&mut self, name: Name) -> Self::ExprKind;
    fn transform_sequence(&mut self, seq: Vec<Expr>) -> Self::ExprKind;
    fn transform_assignment(&mut self, lval: Expr, rval: Expr) -> Self::ExprKind;
    fn transform_function_call(
        &mut self,
        func: Expr,
        args: Vec<Expr>,
        kwargs: HashMap<String, Expr>,
    ) -> Self::ExprKind;
    fn transform_access(&mut self, val: Expr, field: String) -> Self::ExprKind;
    fn transform_tuple(&mut self, fields: Vec<Expr>) -> Self::ExprKind;
    fn transform_lambda(&mut self, params: Vec<TypedIdentifier>, body: Expr) -> Self::ExprKind;
    fn transform_while(&mut self, cond: Expr, body: Expr) -> Self::ExprKind;
    fn transform_return(&mut self, expr: Expr) -> Self::ExprKind;
    fn transform_if(&mut self, cond: Expr, body: Expr, else_body: Option<Expr>) -> Self::ExprKind;
    fn transform_let(&mut self, name: String, typ: Typ, init: Expr) -> Self::ExprKind;
    fn transform_typed_expr(&mut self, expr: Expr, typ: Typ) -> Self::ExprKind;
    fn transform_struct_literal(
        &mut self,
        strct: Name,
        values: HashMap<String, Expr>,
    ) -> Self::ExprKind;
    fn transform_bool_const(&mut self, val: bool) -> Self::ExprKind;
    fn transform_array_literal(&mut self, typ: Typ, size: Expr, init: Vec<Expr>) -> Self::ExprKind;
    fn transform_index_access(&mut self, lhs: Expr, index: Expr) -> Self::ExprKind;
    fn transform_method_call(
        &mut self,
        receiver: Expr,
        method: String,
        args: Vec<Expr>,
        kwargs: HashMap<String, Expr>,
    ) -> Self::ExprKind;

    fn transform_adt_struct_constructor(
        &mut self,
        name: Name,
        case: String,
        fields: Vec<(String, Expr)>,
    ) -> Self::ExprKind;
    fn transform_adt_tuple_constructor(
        &mut self,
        name: Name,
        case: String,
        fields: Vec<Expr>,
    ) -> Self::ExprKind;
    fn transform_adt_unit_constructor(&mut self, name: Name, case: String) -> Self::ExprKind;
    fn transform_match(
        &mut self,
        expr: Expr,
        cases: Vec<(Pattern, Expr, Option<Box<Expr>>)>,
    ) -> Self::ExprKind;

    fn transform_pattern(&mut self, pat: Pattern) -> Self::Pattern;
    fn transform_pattern_kind(&mut self, pat: PatternKind) -> Self::PatternKind {
        transform_pattern_kind::<Self>(self, pat)
    }
    fn transform_var_pattern(&mut self, name: String) -> Self::PatternKind;
    fn transform_tuple_pattern(&mut self, fields: Vec<Pattern>) -> Self::PatternKind;
    fn transform_array_pattern(&mut self, fields: Vec<Pattern>) -> Self::PatternKind;
    fn transform_struct_pattern(
        &mut self,
        strct: Name,
        fields: HashMap<String, Pattern>,
    ) -> Self::PatternKind;
    fn transform_tuple_struct_pattern(
        &mut self,
        strct: Name,
        fields: Vec<Pattern>,
    ) -> Self::PatternKind;
    fn transform_rest_pattern(&mut self, name: String) -> Self::PatternKind;
    fn transform_type_specified_pattern(&mut self, name: Name, pat: Pattern) -> Self::PatternKind;
    fn transform_anon_struct_pattern(
        &mut self,
        fields: HashMap<String, Pattern>,
    ) -> Self::PatternKind;
    fn transform_wildcard_pattern(&mut self) -> Self::PatternKind;
    fn transform_unit_pattern(&mut self) -> Self::PatternKind;
    fn transform_string_pattern(&mut self, val: String) -> Self::PatternKind;
    fn transform_int_pattern(&mut self, val: isize) -> Self::PatternKind;
    fn transform_bool_pattern(&mut self, val: bool) -> Self::PatternKind;
}

pub fn transform_expr_kind<T: AstExprTransformer>(
    transformer: &mut T,
    kind: ExprKind,
) -> T::ExprKind {
    match kind {
        ExprKind::Unit => transformer.transform_unit(),
        ExprKind::Sequence(inner) => transformer.transform_sequence(inner),
        ExprKind::Assignment { lval, rval } => transformer.transform_assignment(*lval, *rval),
        ExprKind::IntConst(val) => transformer.transform_int_const(val),
        ExprKind::StringConst(val) => transformer.transform_string_const(val),
        ExprKind::FloatConst(val) => transformer.transform_float_const(val),
        ExprKind::BoolConst(val) => transformer.transform_bool_const(val),
        ExprKind::Name(val) => transformer.transform_name(val),
        ExprKind::FunctionCall { func, args, kwargs } => {
            transformer.transform_function_call(*func, args, kwargs)
        }
        ExprKind::Access { val, field } => transformer.transform_access(*val, field),
        ExprKind::Tuple(inner) => transformer.transform_tuple(inner),
        ExprKind::Lambda { parameters, body } => transformer.transform_lambda(parameters, *body),
        ExprKind::TypedExpr { expr, typ } => transformer.transform_typed_expr(*expr, typ),
        ExprKind::StructLiteral { strct, values } => {
            transformer.transform_struct_literal(strct, values)
        }
        ExprKind::AdtTupleConstructor {
            typ,
            constructor,
            args,
        } => transformer.transform_adt_tuple_constructor(typ, constructor, args),
        ExprKind::AdtStructConstructor {
            typ,
            constructor,
            fields,
        } => transformer.transform_adt_struct_constructor(typ, constructor, fields),
        ExprKind::AdtUnitConstructor { typ, constructor } => {
            transformer.transform_adt_unit_constructor(typ, constructor)
        }
        ExprKind::While { cond, body } => transformer.transform_while(*cond, *body),
        ExprKind::Return(val) => transformer.transform_return(*val),
        ExprKind::If {
            cond,
            body,
            else_body,
        } => transformer.transform_if(*cond, *body, else_body.map(|e| *e)),
        ExprKind::Let { name, typ, init } => transformer.transform_let(name, typ, *init),
        ExprKind::LetRec { .. } => todo!(),
        ExprKind::AnonType { .. } => todo!(),
        ExprKind::ArrayLiteral { typ, size, init } => {
            transformer.transform_array_literal(typ, *size, init)
        }
        ExprKind::IndexAccess { lhs, index } => transformer.transform_index_access(*lhs, *index),
        ExprKind::Match { expr, cases } => transformer.transform_match(*expr, cases),
        ExprKind::MethodCall {
            receiver,
            method,
            args,
            kwargs,
        } => transformer.transform_method_call(*receiver, method, args, kwargs),
    }
}

fn transform_pattern_kind<T: AstExprTransformer>(
    transformer: &mut T,
    kind: PatternKind,
) -> T::PatternKind {
    match kind {
        PatternKind::Var(val) => transformer.transform_var_pattern(val),
        PatternKind::Tuple(inner) => transformer.transform_tuple_pattern(inner),
        PatternKind::Struct { strct, fields } => {
            transformer.transform_struct_pattern(strct, fields)
        }
        PatternKind::TupleStruct { strct, fields } => {
            transformer.transform_tuple_struct_pattern(strct, fields)
        }
        PatternKind::AnonStruct { fields } => transformer.transform_anon_struct_pattern(fields),
        PatternKind::Array(inner) => transformer.transform_array_pattern(inner),
        PatternKind::Wildcard => transformer.transform_wildcard_pattern(),
        PatternKind::Unit => transformer.transform_unit_pattern(),
        PatternKind::String(val) => transformer.transform_string_pattern(val),
        PatternKind::Int(val) => transformer.transform_int_pattern(val),
        PatternKind::Bool(val) => transformer.transform_bool_pattern(val),
        PatternKind::Rest(name) => transformer.transform_rest_pattern(name),
        PatternKind::TypeSpecifier(expr, typ) => {
            transformer.transform_type_specified_pattern(expr, *typ)
        }
    }
}
