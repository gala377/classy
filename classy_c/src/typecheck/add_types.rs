use crate::syntax::ast;

use super::{type_context::TypCtx, r#type::Type};


pub struct AddTypes<'ctx, 'parent> {
    ctx: &'ctx mut TypCtx<'parent>,
}

impl<'ctx, 'parent> AddTypes<'ctx, 'parent> {
    pub fn new(ctx: &'ctx mut TypCtx<'parent>) -> Self {
        Self { ctx }
    }

    pub fn with_primitive_types(ctx: &'ctx mut TypCtx<'parent>) -> Self {
        let mut this = Self::new(ctx);
        insert_primitive_types(&mut this.ctx);
        this
    }
}

impl<'ast, 'ctx, 'parent> ast::Visitor<'ast> for AddTypes<'ctx, 'parent> {

    fn visit_fn_def(&mut self, node: &'ast ast::FunctionDefinition) {
        let _id = self.ctx.add_fn_node(node);
        let typ_id = self.ctx.reserve_id();
        self.ctx.add_variable(&node.name, typ_id);
    }

    fn visit_type_def(&mut self, node: &'ast ast::TypeDefinition) {
        println!("Adding node for: {node:?}");
        let _id = self.ctx.add_type_node(node);
        let type_id = self.ctx.reserve_id();
        self.ctx.add_type_name(node.name.clone(), type_id);
    }
}


pub fn insert_primitive_types(ctx: &mut TypCtx) {
    let id = ctx.add_type(Type::UInt);
    ctx.add_type_name("UInt", id);
    let id = ctx.add_type(Type::Int);
    ctx.add_type_name("Int", id);
    let id = ctx.add_type(Type::Bool);
    ctx.add_type_name("Bool", id);
    let id = ctx.add_type(Type::String);
    ctx.add_type_name("String", id);
    let id = ctx.add_type(Type::Float);
    ctx.add_type_name("Float", id);
    let id = ctx.add_type(Type::Unit);
    ctx.unit_id = id;
}
