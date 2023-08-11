use classy_syntax::ast::{self, Folder, Namespace};

use crate::{compile::CompilationError, knowledge::Database, typecheck::types::Type};

use super::AstPass;

/// TODO: We should have a scope there
/// in case someone uses variables with the same name as the types
pub struct PromoteCallToStructLiteral<'ctx> {
    db: &'ctx Database,
    namespace: Vec<String>,
    errors: Vec<CompilationError>,
}

impl AstPass for PromoteCallToStructLiteral<'_> {
    fn run(&mut self, ast: ast::SourceFile, _: &crate::session::Session) -> ast::SourceFile {
        self.fold_program(ast)
    }
}

impl<'db> PromoteCallToStructLiteral<'db> {
    pub fn new(db: &'db Database) -> Self {
        Self {
            db,
            namespace: Default::default(),
            errors: Default::default(),
        }
    }

    pub fn error(&mut self, msg: impl Into<String>) {
        self.errors.push(CompilationError::semantic_error(msg));
    }

    fn fold_program(&mut self, program: ast::SourceFile) -> ast::SourceFile {
        self.namespace = program
            .namespace
            .as_ref()
            .map(Namespace::as_segments)
            .unwrap_or_default();
        ast::fold::fold_program(self, program)
    }

    fn type_from_name<'a, 'e>(&'a self, lval: &'e ast::Expr) -> Option<(&'e ast::Name, &'a Type)> {
        let ast::Expr {
            kind: ast::ExprKind::Name(name @ ast::Name::Unresolved { path, identifier }),
            ..
        } = lval
        else {
            return None;
        };
        self.db
            .get_type_by_unresolved_name(&self.namespace, path, identifier)
            .map(|typ| (name, typ))
    }

    fn is_struct<'e, 'a>(&'a self, lval: &'e ast::Expr) -> Option<(&'e ast::Name, &'a Type)> {
        match self.type_from_name(lval)? {
            (n, s @ Type::Struct { .. }) => Some((n, s)),
            _ => None,
        }
    }

    fn is_adt_constructor<'a, 'e>(
        &'a self,
        lval: &'e ast::Expr,
        case: &'e str,
    ) -> Option<(&'e ast::Name, &'a (String, Type))> {
        let typ = self.type_from_name(lval)?;
        match typ {
            (n, Type::ADT { constructors, .. }) => constructors
                .iter()
                .find(|(name, _)| name == case)
                .map(|constr| (n, constr)),
            _ => None,
        }
    }

    fn is_proper_initializer(&self, args: &[ast::Expr]) -> Option<Vec<(String, ast::Expr)>> {
        let [ast::Expr {
            kind:
                ast::ExprKind::Lambda {
                    body:
                        box ast::Expr {
                            kind: ast::ExprKind::Sequence(initializers),
                            ..
                        },
                    parameters,
                },
            ..
        }] = args
        else {
            return None;
        };
        if !parameters.is_empty() {
            return None;
        }
        let mut fields = Vec::with_capacity(initializers.len());
        for initializer in initializers {
            let ast::Expr {
                kind:
                    ast::ExprKind::Assignment {
                        lval:
                            box ast::Expr {
                                kind:
                                    ast::ExprKind::Name(ast::Name::Unresolved { path, identifier }),
                                ..
                            },
                        rval,
                    },
                ..
            } = initializer
            else {
                return None;
            };
            if !path.is_empty() {
                return None;
            }
            fields.push((identifier.clone(), rval.as_ref().clone()));
        }
        Some(fields)
    }
}

impl Folder for PromoteCallToStructLiteral<'_> {
    fn fold_function_call(
        &mut self,
        func: classy_syntax::ast::Expr,
        args: Vec<classy_syntax::ast::Expr>,
        kwargs: std::collections::HashMap<String, classy_syntax::ast::Expr>,
    ) -> classy_syntax::ast::ExprKind {
        // none of the constructors accept kwargs
        if !kwargs.is_empty() {
            return ast::fold::fold_function_call(self, func, args, kwargs);
        }
        if let Some((name, _)) = self.is_struct(&func) {
            let fields = self
                .is_proper_initializer(&args)
                .expect("not a proper struct initializer");
            return ast::fold::fold_struct_literal(
                self,
                name.clone(),
                fields.into_iter().collect(),
            );
        }
        ast::fold::fold_function_call(self, func, args, kwargs)
    }

    fn fold_access(
        &mut self,
        val: classy_syntax::ast::Expr,
        field: String,
    ) -> classy_syntax::ast::ExprKind {
        if let Some((name, (case, Type::Unit))) = self.is_adt_constructor(&val, &field) {
            return ast::ExprKind::AdtUnitConstructor {
                typ: name.clone(),
                constructor: case.clone(),
            };
        }
        ast::fold::fold_access(self, val, field)
    }

    fn fold_methods_call(
        &mut self,
        receiver: ast::Expr,
        method: String,
        args: Vec<ast::Expr>,
        kwargs: std::collections::HashMap<String, ast::Expr>,
    ) -> ast::ExprKind {
        if !kwargs.is_empty() {
            return ast::fold::fold_method_call(self, receiver, method, args, kwargs);
        }
        if let Some((name, (constr, typ))) = self.is_adt_constructor(&receiver, &method) {
            match typ {
                Type::Struct { .. } => {
                    let fields = self
                        .is_proper_initializer(&args)
                        .expect("not a proper struct initializer");
                    return ast::fold::fold_adt_struct_constructor(
                        self,
                        name.clone(),
                        constr.clone(),
                        fields,
                    );
                }
                Type::Tuple(..) => {
                    return ast::fold::fold_adt_tuple_constructor(
                        self,
                        name.clone(),
                        constr.clone(),
                        args,
                    );
                }
                _ => {
                    panic!("ICE: only expected struct constructors")
                }
            }
        }
        ast::fold::fold_method_call(self, receiver, method, args, kwargs)
    }
}
