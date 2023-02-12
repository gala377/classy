use crate::syntax::ast;

pub trait Visitor<'ast>: Sized {
    fn visit(&mut self, node: &'ast ast::Program) {
        walk_program(self, node)
    }

    fn visit_top_level_item(&mut self, node: &'ast ast::TopLevelItem) {
        walk_top_level_item(self, node)
    }

    fn visit_fn_def(&mut self, _node: &'ast ast::FunctionDefinition) {
        unimplemented!()
    }

    fn visit_type_def(&mut self, node: &'ast ast::TypeDefinition);
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
