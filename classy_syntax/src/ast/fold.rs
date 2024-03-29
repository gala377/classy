use std::collections::HashMap;

use crate::ast::{
    DefinedType, Discriminant, DiscriminantKind, Expr, ExprKind, FunctionDefinition, Method, Name,
    Pattern, PatternKind, SourceFile, TopLevelItemKind, Typ, TypeDefinition, TypeVariable,
    TypedIdentifier,
};

use super::{
    Alias, ClassDefinition, ClassDefinitionItem, ConstDefinition, FuncDecl, InstanceDefinition,
    InstanceDefinitionItem, MethodsBlock, Record, TopLevelItem, TypeBound, ADT,
};

pub trait Folder: Sized {
    fn fold_program(&mut self, program: SourceFile) -> SourceFile {
        fold_program(self, program)
    }

    fn fold_top_level_item(&mut self, item: TopLevelItem) -> TopLevelItem {
        fold_top_level_item(self, item)
    }

    fn fold_top_level_item_kind(&mut self, item: TopLevelItemKind) -> TopLevelItemKind {
        fold_top_level_item_kind(self, item)
    }

    fn fold_function_definition(&mut self, def: FunctionDefinition) -> FunctionDefinition {
        fold_function_definition(self, def)
    }

    fn fold_method_definition(
        &mut self,
        def: Method<FunctionDefinition>,
    ) -> Method<FunctionDefinition> {
        fold_method_definition(self, def)
    }

    fn fold_const_definition(&mut self, def: ConstDefinition) -> ConstDefinition {
        fold_const_definition(self, def)
    }

    fn fold_methods_block(
        &mut self,
        meths: MethodsBlock<FunctionDefinition>,
    ) -> MethodsBlock<FunctionDefinition> {
        fold_methods_block(self, meths)
    }

    fn fold_type_definition(&mut self, def: TypeDefinition) -> TypeDefinition {
        fold_type_definition(self, def)
    }

    fn fold_defined_type(&mut self, kind: DefinedType) -> DefinedType {
        fold_defined_type(self, kind)
    }

    fn fold_defined_type_alias(&mut self, alias: Alias) -> DefinedType {
        fold_defined_type_alias(self, alias)
    }

    fn fold_defined_type_record(&mut self, record: Record) -> DefinedType {
        fold_defined_type_record(self, record)
    }

    fn fold_defined_type_adt(&mut self, adt: ADT) -> DefinedType {
        fold_defined_type_adt(self, adt)
    }

    fn fold_expr(&mut self, expr: Expr) -> Expr {
        fold_expr(self, expr)
    }

    fn fold_typ(&mut self, typ: Typ) -> Typ {
        fold_type(self, typ)
    }

    fn fold_name(&mut self, name: Name) -> Name {
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

    fn fold_identifier(&mut self, id: String) -> String {
        id
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

    fn fold_lambda(&mut self, params: Vec<TypedIdentifier>, body: Expr) -> ExprKind {
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

    fn fold_struct_literal(&mut self, strct: Name, values: HashMap<String, Expr>) -> ExprKind {
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

    fn fold_methods_call(
        &mut self,
        receiver: Expr,
        method: String,
        args: Vec<Expr>,
        kwargs: HashMap<String, Expr>,
    ) -> ExprKind {
        fold_method_call(self, receiver, method, args, kwargs)
    }

    fn fold_anon_type(&mut self, fields: Vec<(String, Expr)>) -> ExprKind {
        fold_anon_type(self, fields)
    }

    fn fold_attributes(&mut self, attributes: Vec<String>) -> Vec<String> {
        attributes
    }

    fn fold_array(&mut self, size: Expr, typ: Typ, init: Vec<Expr>) -> ExprKind {
        fold_array(self, size, typ, init)
    }

    fn fold_index_access(&mut self, lhs: Expr, index: Expr) -> ExprKind {
        fold_index_access(self, lhs, index)
    }

    fn fold_match(
        &mut self,
        expr: Expr,
        cases: Vec<(Pattern, Expr, Option<Box<Expr>>)>,
    ) -> ExprKind {
        fold_match(self, expr, cases)
    }

    fn fold_pattern(&mut self, pat: Pattern) -> Pattern {
        fold_pattern(self, pat)
    }

    fn fold_pattern_kind(&mut self, pat: PatternKind) -> PatternKind {
        fold_pattern_kind(self, pat)
    }

    fn fold_unit_pattern(&mut self) -> PatternKind {
        PatternKind::Unit
    }

    fn fold_name_pattern(&mut self, name: String) -> PatternKind {
        PatternKind::Var(name)
    }

    fn fold_wildcard_pattern(&mut self) -> PatternKind {
        PatternKind::Wildcard
    }

    fn fold_int_pattern(&mut self, val: isize) -> PatternKind {
        PatternKind::Int(val)
    }

    fn fold_bool_pattern(&mut self, val: bool) -> PatternKind {
        PatternKind::Bool(val)
    }

    fn fold_string_pattern(&mut self, val: String) -> PatternKind {
        PatternKind::String(val)
    }

    fn fold_tuple_pattern(&mut self, fields: Vec<Pattern>) -> PatternKind {
        fold_tuple_pattern(self, fields)
    }

    fn fold_array_pattern(&mut self, fields: Vec<Pattern>) -> PatternKind {
        fold_array_pattern(self, fields)
    }

    fn fold_struct_pattern(
        &mut self,
        strct: Name,
        fields: HashMap<String, Pattern>,
    ) -> PatternKind {
        fold_struct_pattern(self, strct, fields)
    }

    fn fold_tuple_struct_pattern(&mut self, strct: Name, fields: Vec<Pattern>) -> PatternKind {
        fold_tuple_struct_pattern(self, strct, fields)
    }

    fn fold_anon_struct_pattern(&mut self, fields: HashMap<String, Pattern>) -> PatternKind {
        fold_anon_struct_pattern(self, fields)
    }

    fn fold_rest_pattern(&mut self, name: String) -> PatternKind {
        PatternKind::Rest(name)
    }

    fn fold_type_specified_pattern(&mut self, name: Name, typ: Pattern) -> PatternKind {
        fold_type_specified_pattern(self, name, typ)
    }

    fn fold_adt_struct_constructor(
        &mut self,
        name: Name,
        case: String,
        fields: Vec<(String, Expr)>,
    ) -> ExprKind {
        fold_adt_struct_constructor(self, name, case, fields)
    }

    fn fold_adt_tuple_constructor(
        &mut self,
        name: Name,
        case: String,
        fields: Vec<Expr>,
    ) -> ExprKind {
        fold_adt_tuple_constructor(self, name, case, fields)
    }

    fn fold_adt_unit_constructor(&mut self, name: Name, case: String) -> ExprKind {
        fold_adt_unit_constructor(self, name, case)
    }

    fn fold_unit_type(&mut self) -> Typ {
        Typ::Unit
    }

    fn fold_name_type(&mut self, name: Name) -> Typ {
        Typ::Name(name)
    }

    fn fold_to_infere_type(&mut self) -> Typ {
        Typ::ToInfere
    }

    fn fold_poly_type(&mut self, vars: Vec<String>, bounds: Vec<TypeBound>, typ: Typ) -> Typ {
        fold_poly_type(self, vars, bounds, typ)
    }

    fn fold_type_bound(&mut self, head: Name, bounds: Vec<Typ>) -> TypeBound {
        fold_type_bound(self, head, bounds)
    }

    fn fold_application_type(&mut self, callee: Typ, args: Vec<Typ>) -> Typ {
        fold_application_type(self, callee, args)
    }

    fn fold_function_type(&mut self, args: Vec<Typ>, ret: Typ) -> Typ {
        fold_function_type(self, args, ret)
    }

    fn fold_tuple_type(&mut self, fields: Vec<Typ>) -> Typ {
        fold_tuple_type(self, fields)
    }

    fn fold_array_type(&mut self, typ: Typ) -> Typ {
        fold_array_type(self, typ)
    }

    fn fold_let_rec(&mut self, definitions: Vec<FunctionDefinition>) -> ExprKind {
        fold_let_rec(self, definitions)
    }

    fn fold_local_function_def(&mut self, def: FunctionDefinition) -> FunctionDefinition {
        fold_local_function_def(self, def)
    }

    fn fold_discriminant_kind(&mut self, kind: DiscriminantKind) -> DiscriminantKind {
        fold_discriminant_kind(self, kind)
    }

    fn fold_instance_definition(
        &mut self,
        InstanceDefinition {
            name,
            free_variables,
            bounds,
            instanced_class,
            body,
        }: InstanceDefinition,
    ) -> InstanceDefinition {
        fold_instance_definition(self, name, free_variables, bounds, instanced_class, body)
    }

    fn fold_instance_definition_item(
        &mut self,
        item: InstanceDefinitionItem,
    ) -> InstanceDefinitionItem {
        fold_instance_definition_item(self, item)
    }

    fn fold_class_definition(
        &mut self,
        ClassDefinition {
            name,
            bounds,
            args,
            body,
        }: ClassDefinition,
    ) -> ClassDefinition {
        fold_class_definition(self, name, args, bounds, body)
    }

    fn fold_class_definition_item(&mut self, item: ClassDefinitionItem) -> ClassDefinitionItem {
        fold_class_definition_item(self, item)
    }

    fn fold_class_function_decl(&mut self, decl: FuncDecl) -> FuncDecl {
        fold_class_function_decl(self, decl)
    }

    fn fold_class_methods_block(
        &mut self,
        block: MethodsBlock<FuncDecl>,
    ) -> MethodsBlock<FuncDecl> {
        fold_class_methods_block(self, block)
    }

    fn fold_class_methods_block_method(&mut self, method: Method<FuncDecl>) -> Method<FuncDecl> {
        fold_class_methods_block_method(self, method)
    }

    fn fold_name_import(&mut self, name: Name) -> Name {
        name
    }

    fn fold_instance_import(&mut self, name: Name) -> Name {
        name
    }

    fn fold_methods_import(&mut self, name: Name) -> Name {
        name
    }
}

fn fold_adt_unit_constructor(folder: &mut impl Folder, name: Name, case: String) -> ExprKind {
    ExprKind::AdtUnitConstructor {
        typ: folder.fold_name(name),
        constructor: case,
    }
}

pub fn fold_program<F: Folder>(folder: &mut F, program: SourceFile) -> SourceFile {
    let mut new_items = Vec::new();
    for item in program.items {
        new_items.push(folder.fold_top_level_item(item));
    }
    SourceFile {
        namespace: program.namespace,
        items: new_items,
    }
}

pub fn fold_top_level_item_kind<F: Folder>(
    folder: &mut F,
    item: TopLevelItemKind,
) -> TopLevelItemKind {
    match item {
        TopLevelItemKind::FunctionDefinition(def) => {
            TopLevelItemKind::FunctionDefinition(folder.fold_function_definition(def))
        }
        TopLevelItemKind::TypeDefinition(def) => {
            TopLevelItemKind::TypeDefinition(folder.fold_type_definition(def))
        }
        TopLevelItemKind::MethodsBlock(meths) => {
            TopLevelItemKind::MethodsBlock(folder.fold_methods_block(meths))
        }
        TopLevelItemKind::ConstDefinition(def) => {
            TopLevelItemKind::ConstDefinition(folder.fold_const_definition(def))
        }
        TopLevelItemKind::InstanceDefinition(def) => {
            TopLevelItemKind::InstanceDefinition(folder.fold_instance_definition(def))
        }
        TopLevelItemKind::ClassDefinition(def) => {
            TopLevelItemKind::ClassDefinition(folder.fold_class_definition(def))
        }
        TopLevelItemKind::NameImport(name) => {
            TopLevelItemKind::NameImport(folder.fold_name_import(name))
        }
        TopLevelItemKind::MethodsImport(name) => {
            TopLevelItemKind::MethodsImport(folder.fold_methods_import(name))
        }
        TopLevelItemKind::InstanceImport(name) => {
            TopLevelItemKind::InstanceImport(folder.fold_instance_import(name))
        }
    }
}

pub fn fold_function_definition<F: Folder>(
    folder: &mut F,
    def: FunctionDefinition,
) -> FunctionDefinition {
    FunctionDefinition {
        name: def.name,
        parameters: folder.fold_function_params(def.parameters),
        body: folder.fold_expr(def.body),
        typ: folder.fold_typ(def.typ),
        attributes: folder.fold_attributes(def.attributes),
    }
}

pub fn fold_method_definition<F: Folder>(
    folder: &mut F,
    def: Method<FunctionDefinition>,
) -> Method<FunctionDefinition> {
    Method {
        id: def.id,
        item: FunctionDefinition {
            name: def.item.name,
            parameters: folder.fold_function_params(def.item.parameters),
            body: folder.fold_expr(def.item.body),
            typ: folder.fold_typ(def.item.typ),
            attributes: folder.fold_attributes(def.item.attributes),
        },
    }
}

pub fn fold_type_definition<F: Folder>(folder: &mut F, def: TypeDefinition) -> TypeDefinition {
    TypeDefinition {
        name: folder.fold_type_name(def.name),
        definition: folder.fold_defined_type(def.definition),
        type_variables: def.type_variables,
        span: def.span,
        constraints: def.constraints,
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
        ExprKind::FunctionCall { func, args, kwargs } => {
            folder.fold_function_call(*func, args, kwargs)
        }
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
        ExprKind::ArrayLiteral { typ, size, init } => folder.fold_array(*size, typ, init),
        ExprKind::IndexAccess { lhs, index } => folder.fold_index_access(*lhs, *index),
        ExprKind::Match { expr, cases } => folder.fold_match(*expr, cases),
        ExprKind::AdtStructConstructor {
            typ,
            constructor,
            fields,
        } => folder.fold_adt_struct_constructor(typ, constructor, fields),
        ExprKind::AdtTupleConstructor {
            typ,
            constructor,
            args,
        } => folder.fold_adt_tuple_constructor(typ, constructor, args),
        ExprKind::AdtUnitConstructor { typ, constructor } => {
            folder.fold_adt_unit_constructor(typ, constructor)
        }
        ExprKind::MethodCall {
            receiver,
            method,
            args,
            kwargs,
        } => folder.fold_methods_call(*receiver, method, args, kwargs),
        ExprKind::LetRec { definitions } => folder.fold_let_rec(definitions),
    }
}

pub fn fold_sequence(folder: &mut impl Folder, seq: Vec<Expr>) -> Vec<Expr> {
    let mut new_seq = Vec::new();
    for expr in seq {
        new_seq.push(folder.fold_expr(expr));
    }
    new_seq
}

pub fn fold_if(
    folder: &mut impl Folder,
    cond: Expr,
    body: Expr,
    else_body: Option<Expr>,
) -> ExprKind {
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

pub fn fold_lambda(folder: &mut impl Folder, params: Vec<TypedIdentifier>, body: Expr) -> ExprKind {
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
    strct: Name,
    values: HashMap<String, Expr>,
) -> ExprKind {
    ExprKind::StructLiteral {
        strct: folder.fold_name(strct),
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

pub fn fold_array(folder: &mut impl Folder, size: Expr, typ: Typ, init: Vec<Expr>) -> ExprKind {
    let new_size = Box::new(folder.fold_expr(size));
    let new_t = folder.fold_typ(typ);
    let new_init = init.into_iter().map(|e| folder.fold_expr(e)).collect();
    ExprKind::ArrayLiteral {
        typ: new_t,
        size: new_size,
        init: new_init,
    }
}

pub fn fold_index_access(folder: &mut impl Folder, lhs: Expr, index: Expr) -> ExprKind {
    let lhs = Box::new(folder.fold_expr(lhs));
    let index = Box::new(folder.fold_expr(index));
    ExprKind::IndexAccess { lhs, index }
}

pub fn fold_match(
    folder: &mut impl Folder,
    expr: Expr,
    cases: Vec<(Pattern, Expr, Option<Box<Expr>>)>,
) -> ExprKind {
    let expr = Box::new(folder.fold_expr(expr));
    let mut new_cases = Vec::new();
    for (pat, expr, guard) in cases {
        new_cases.push((
            folder.fold_pattern(pat),
            folder.fold_expr(expr),
            guard.map(|e| Box::new(folder.fold_expr(*e))),
        ));
    }
    ExprKind::Match {
        expr,
        cases: new_cases,
    }
}

pub fn fold_tuple_pattern(folder: &mut impl Folder, fields: Vec<Pattern>) -> PatternKind {
    let mut new_fields = Vec::new();
    for field in fields {
        new_fields.push(folder.fold_pattern(field));
    }
    PatternKind::Tuple(new_fields)
}

pub fn fold_array_pattern(folder: &mut impl Folder, fields: Vec<Pattern>) -> PatternKind {
    let mut new_fields = Vec::new();
    for field in fields {
        new_fields.push(folder.fold_pattern(field));
    }
    PatternKind::Array(new_fields)
}

pub fn fold_struct_pattern(
    folder: &mut impl Folder,
    strct: Name,
    fields: HashMap<String, Pattern>,
) -> PatternKind {
    let mut new_fields = Vec::new();
    for (name, field) in fields {
        new_fields.push((name, folder.fold_pattern(field)));
    }
    PatternKind::Struct {
        strct: folder.fold_name(strct),
        fields: new_fields.into_iter().collect(),
    }
}

pub fn fold_tuple_struct_pattern(
    folder: &mut impl Folder,
    strct: Name,
    fields: Vec<Pattern>,
) -> PatternKind {
    let mut new_fields = Vec::new();
    for field in fields {
        new_fields.push(folder.fold_pattern(field));
    }
    PatternKind::TupleStruct {
        strct: folder.fold_name(strct),
        fields: new_fields,
    }
}

pub fn fold_pattern_kind(folder: &mut impl Folder, pat: PatternKind) -> PatternKind {
    match pat {
        PatternKind::Unit => folder.fold_unit_pattern(),
        PatternKind::Var(name) => folder.fold_name_pattern(name),
        PatternKind::Wildcard => folder.fold_wildcard_pattern(),
        PatternKind::Int(val) => folder.fold_int_pattern(val),
        PatternKind::Bool(val) => folder.fold_bool_pattern(val),
        PatternKind::String(val) => folder.fold_string_pattern(val),
        PatternKind::Tuple(fields) => fold_tuple_pattern(folder, fields),
        PatternKind::Array(fields) => fold_array_pattern(folder, fields),
        PatternKind::Struct { strct, fields } => fold_struct_pattern(folder, strct, fields),
        PatternKind::TupleStruct { strct, fields } => {
            fold_tuple_struct_pattern(folder, strct, fields)
        }
        PatternKind::Rest(name) => folder.fold_rest_pattern(name),
        PatternKind::TypeSpecifier(name, pat) => folder.fold_type_specified_pattern(name, *pat),
        PatternKind::AnonStruct { fields } => fold_anon_struct_pattern(folder, fields),
    }
}

pub fn fold_type_specified_pattern(
    folder: &mut impl Folder,
    name: Name,
    pattern: Pattern,
) -> PatternKind {
    PatternKind::TypeSpecifier(
        folder.fold_name(name),
        Box::new(folder.fold_pattern(pattern)),
    )
}

pub fn fold_adt_struct_constructor(
    folder: &mut impl Folder,
    name: Name,
    case: String,
    fields: Vec<(String, Expr)>,
) -> ExprKind {
    let mut new_fields = Vec::new();
    for (name, val) in fields {
        new_fields.push((name, folder.fold_expr(val)));
    }
    ExprKind::AdtStructConstructor {
        typ: folder.fold_name(name),
        constructor: case,
        fields: new_fields,
    }
}

pub fn fold_adt_tuple_constructor(
    folder: &mut impl Folder,
    name: Name,
    case: String,
    fields: Vec<Expr>,
) -> ExprKind {
    let mut new_fields = Vec::new();
    for val in fields {
        new_fields.push(folder.fold_expr(val));
    }
    ExprKind::AdtTupleConstructor {
        typ: folder.fold_name(name),
        constructor: case,
        args: new_fields,
    }
}

pub fn fold_anon_struct_pattern(
    folder: &mut impl Folder,
    fields: HashMap<String, Pattern>,
) -> PatternKind {
    let mut new_fields = HashMap::new();
    for (name, field) in fields {
        new_fields.insert(name, folder.fold_pattern(field));
    }
    PatternKind::AnonStruct { fields: new_fields }
}

pub fn fold_type(folder: &mut impl Folder, typ: Typ) -> Typ {
    match typ {
        Typ::Unit => folder.fold_unit_type(),
        Typ::Name(name) => folder.fold_name_type(name),
        Typ::Function { args, ret } => folder.fold_function_type(args, *ret),
        Typ::Tuple(fields) => folder.fold_tuple_type(fields),
        Typ::Array(inner) => folder.fold_array_type(*inner),
        Typ::ToInfere => folder.fold_to_infere_type(),
        Typ::Poly {
            free_variables,
            bounds,
            typ,
        } => folder.fold_poly_type(free_variables, bounds, *typ),
        Typ::Application { callee, args } => folder.fold_application_type(*callee, args),
    }
}

pub fn fold_array_type(folder: &mut impl Folder, typ: Typ) -> Typ {
    Typ::Array(Box::new(folder.fold_typ(typ)))
}

pub fn fold_function_type(folder: &mut impl Folder, args: Vec<Typ>, ret: Typ) -> Typ {
    Typ::Function {
        args: args.into_iter().map(|t| folder.fold_typ(t)).collect(),
        ret: Box::new(folder.fold_typ(ret)),
    }
}

pub fn fold_poly_type(
    folder: &mut impl Folder,
    vars: Vec<String>,
    bounds: Vec<TypeBound>,
    typ: Typ,
) -> Typ {
    Typ::Poly {
        free_variables: vars,
        bounds: bounds
            .into_iter()
            .map(|TypeBound { head, args }| folder.fold_type_bound(head, args))
            .collect(),
        typ: Box::new(folder.fold_typ(typ)),
    }
}

pub fn fold_application_type(folder: &mut impl Folder, callee: Typ, args: Vec<Typ>) -> Typ {
    Typ::Application {
        callee: Box::new(folder.fold_typ(callee)),
        args: args.into_iter().map(|t| folder.fold_typ(t)).collect(),
    }
}

pub fn fold_tuple_type(folder: &mut impl Folder, fields: Vec<Typ>) -> Typ {
    Typ::Tuple(fields.into_iter().map(|t| folder.fold_typ(t)).collect())
}

pub fn fold_pattern(folder: &mut impl Folder, pat: Pattern) -> Pattern {
    Pattern {
        id: pat.id,
        kind: folder.fold_pattern_kind(pat.kind),
    }
}

pub fn fold_methods_block(
    folder: &mut impl Folder,
    meths: MethodsBlock<FunctionDefinition>,
) -> MethodsBlock<FunctionDefinition> {
    MethodsBlock {
        name: meths.name,
        typ: folder.fold_typ(meths.typ),
        methods: meths
            .methods
            .into_iter()
            .map(|def| folder.fold_method_definition(def))
            .collect(),
    }
}

pub fn fold_method_call(
    folder: &mut impl Folder,
    receiver: Expr,
    method: String,
    args: Vec<Expr>,
    kwargs: HashMap<String, Expr>,
) -> ExprKind {
    ExprKind::MethodCall {
        receiver: Box::new(folder.fold_expr(receiver)),
        method,
        args: folder.fold_function_args(args),
        kwargs: kwargs
            .into_iter()
            .map(|(k, v)| (k, folder.fold_expr(v)))
            .collect(),
    }
}

pub fn fold_const_definition(folder: &mut impl Folder, def: ConstDefinition) -> ConstDefinition {
    ConstDefinition {
        name: def.name,
        typ: folder.fold_typ(def.typ),
        init: folder.fold_expr(def.init),
    }
}

pub fn fold_let_rec(folder: &mut impl Folder, definitions: Vec<FunctionDefinition>) -> ExprKind {
    let mut new_defs = Vec::new();
    for def in definitions {
        new_defs.push(folder.fold_function_definition(def));
    }
    ExprKind::LetRec {
        definitions: new_defs,
    }
}

pub fn fold_local_function_def(
    folder: &mut impl Folder,
    def: FunctionDefinition,
) -> FunctionDefinition {
    FunctionDefinition {
        name: def.name,
        parameters: folder.fold_function_params(def.parameters),
        body: folder.fold_expr(def.body),
        typ: folder.fold_typ(def.typ),
        attributes: folder.fold_attributes(def.attributes),
    }
}

pub fn fold_top_level_item(folder: &mut impl Folder, item: TopLevelItem) -> TopLevelItem {
    TopLevelItem {
        kind: folder.fold_top_level_item_kind(item.kind),
        ..item
    }
}

pub fn fold_defined_type(folder: &mut impl Folder, kind: DefinedType) -> DefinedType {
    match kind {
        DefinedType::Record(fields) => folder.fold_defined_type_record(fields),
        DefinedType::ADT(cases) => folder.fold_defined_type_adt(cases),
        DefinedType::Alias(alias) => folder.fold_defined_type_alias(alias),
    }
}

pub fn fold_defined_type_record(folder: &mut impl Folder, record: Record) -> DefinedType {
    DefinedType::Record(Record {
        fields: record
            .fields
            .into_iter()
            .map(|TypedIdentifier { name, typ }| TypedIdentifier {
                name,
                typ: folder.fold_typ(typ),
            })
            .collect(),
    })
}

pub fn fold_defined_type_adt(folder: &mut impl Folder, adt: ADT) -> DefinedType {
    DefinedType::ADT(ADT {
        discriminants: adt
            .discriminants
            .into_iter()
            .map(
                |Discriminant {
                     constructor,
                     arguments,
                 }| Discriminant {
                    constructor,
                    arguments: folder.fold_discriminant_kind(arguments),
                },
            )
            .collect(),
    })
}

pub fn fold_defined_type_alias(folder: &mut impl Folder, alias: Alias) -> DefinedType {
    DefinedType::Alias(Alias {
        for_type: folder.fold_typ(alias.for_type),
    })
}

pub fn fold_discriminant_kind(
    folder: &mut impl Folder,
    kind: DiscriminantKind,
) -> DiscriminantKind {
    match kind {
        DiscriminantKind::Tuple(fields) => {
            DiscriminantKind::Tuple(fields.into_iter().map(|typ| folder.fold_typ(typ)).collect())
        }
        DiscriminantKind::Record(fields) => DiscriminantKind::Record(
            fields
                .into_iter()
                .map(|(name, typ)| (name, folder.fold_typ(typ)))
                .collect(),
        ),
        DiscriminantKind::Empty => DiscriminantKind::Empty,
    }
}

pub fn fold_type_bound(folder: &mut impl Folder, head: Name, bounds: Vec<Typ>) -> TypeBound {
    TypeBound {
        head: folder.fold_name(head),
        args: bounds.into_iter().map(|typ| folder.fold_typ(typ)).collect(),
    }
}

pub fn fold_instance_definition(
    folder: &mut impl Folder,
    name: Option<String>,
    free_variables: Vec<String>,
    bounds: Vec<TypeBound>,
    TypeBound {
        head: def_name,
        args: def_args,
    }: TypeBound,
    body: Vec<InstanceDefinitionItem>,
) -> InstanceDefinition {
    InstanceDefinition {
        name: name.map(|n| folder.fold_identifier(n)),
        free_variables,
        bounds: bounds
            .into_iter()
            .map(|TypeBound { head, args }| folder.fold_type_bound(head, args))
            .collect(),
        instanced_class: folder.fold_type_bound(def_name, def_args),
        body: body
            .into_iter()
            .map(|item| folder.fold_instance_definition_item(item))
            .collect(),
    }
}

pub fn fold_instance_definition_item(
    folder: &mut impl Folder,
    item: InstanceDefinitionItem,
) -> InstanceDefinitionItem {
    match item {
        InstanceDefinitionItem::FunctionDefinition(def) => {
            InstanceDefinitionItem::FunctionDefinition(folder.fold_function_definition(def))
        }
        InstanceDefinitionItem::MethodsBlock(methods) => {
            InstanceDefinitionItem::MethodsBlock(folder.fold_methods_block(methods))
        }
    }
}

pub fn fold_class_definition(
    folder: &mut impl Folder,
    name: String,
    args: Vec<String>,
    bounds: Vec<TypeBound>,
    body: Vec<ClassDefinitionItem>,
) -> ClassDefinition {
    ClassDefinition {
        name: folder.fold_identifier(name),
        args,
        bounds: bounds
            .into_iter()
            .map(|TypeBound { head, args }| folder.fold_type_bound(head, args))
            .collect(),
        body: body
            .into_iter()
            .map(|item| folder.fold_class_definition_item(item))
            .collect(),
    }
}

pub fn fold_class_definition_item(
    folder: &mut impl Folder,
    item: ClassDefinitionItem,
) -> ClassDefinitionItem {
    match item {
        ClassDefinitionItem::Function(def) => {
            ClassDefinitionItem::Function(folder.fold_class_function_decl(def))
        }
        ClassDefinitionItem::MethodBlock(methods) => {
            ClassDefinitionItem::MethodBlock(folder.fold_class_methods_block(methods))
        }
    }
}

pub fn fold_class_function_decl(
    folder: &mut impl Folder,
    FuncDecl { name, typ }: FuncDecl,
) -> FuncDecl {
    FuncDecl {
        name: folder.fold_identifier(name),
        typ: folder.fold_typ(typ),
    }
}

pub fn fold_class_methods_block(
    folder: &mut impl Folder,
    MethodsBlock { name, typ, methods }: MethodsBlock<FuncDecl>,
) -> MethodsBlock<FuncDecl> {
    MethodsBlock {
        name: name.map(|n| folder.fold_identifier(n)),
        typ: folder.fold_typ(typ),
        methods: methods
            .into_iter()
            .map(|method| folder.fold_class_methods_block_method(method))
            .collect(),
    }
}

pub fn fold_class_methods_block_method(
    folder: &mut impl Folder,
    Method {
        id,
        item: FuncDecl { name, typ },
    }: Method<FuncDecl>,
) -> Method<FuncDecl> {
    Method {
        id,
        item: FuncDecl {
            name: folder.fold_identifier(name),
            typ: folder.fold_typ(typ),
        },
    }
}
