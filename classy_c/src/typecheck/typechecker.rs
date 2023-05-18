use crate::syntax::ast::Visitor;

use super::type_context::TypCtx;

struct TypeChecker<'ctx, 'parent> {
    type_ctx: &'ctx TypCtx<'parent>,
}

impl<'ctx, 'parent> TypeChecker<'ctx, 'parent> {
    pub fn new(type_ctx: &'ctx TypCtx<'parent>) -> Self {
        Self { type_ctx }
    }
}

impl<'ast, 'ctx, 'parent> Visitor<'ast> for TypeChecker<'ctx, 'parent> {
    fn visit_type_def(&mut self, _node: &'ast crate::syntax::ast::TypeDefinition) {}

    fn visit_fn_def(&mut self, _fn_def: &'ast crate::syntax::ast::FunctionDefinition) {}
}
