use classy_syntax::ast;

use crate::typecheck::{type_context::TypCtx, types::Type};

pub struct AddTypes<'ctx> {
    ctx: &'ctx mut TypCtx,
}

impl<'ctx> AddTypes<'ctx> {
    pub fn new(ctx: &'ctx mut TypCtx) -> Self {
        Self { ctx }
    }

    pub fn with_primitive_types(ctx: &'ctx mut TypCtx) -> Self {
        let this = Self::new(ctx);
        insert_primitive_types(this.ctx);
        this
    }
}

/// TODO: We need more complex logic for methods here

impl<'ast, 'ctx> ast::Visitor<'ast> for AddTypes<'ctx> {
    fn visit_fn_def(&mut self, node: &'ast ast::FunctionDefinition) {
        let _id = self.ctx.add_fn_node(node);
        let typ_id = self.ctx.reserve_id();
        self.ctx.add_variable(&node.name, typ_id);
    }

    fn visit_const_definition(&mut self, def: &'ast ast::ConstDefinition) {
        let _id = self.ctx.add_const_node(def);
        let typ_id = self.ctx.reserve_id();
        self.ctx.add_variable(&def.name, typ_id);
    }

    fn visit_type_def(&mut self, node: &'ast ast::TypeDefinition) {
        println!("Adding node for: {node:?}");
        let _id = self.ctx.add_type_node(node);
        let type_id = self.ctx.reserve_id();
        self.ctx.add_type_name(node.name.clone(), type_id);
    }

    fn visit_methods_block(&mut self, meth: &'ast ast::MethodsBlock<ast::FunctionDefinition>) {
        let _id = self.ctx.add_methods_block_node(meth);
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
    let id = ctx.add_type(Type::Byte);
    ctx.add_type_name("Byte", id);
    let id = ctx.add_type(Type::Unit);
    ctx.unit_id = id;
    let id = ctx.add_type(Type::ToInfere);
    ctx.to_infere_id = id;
}
