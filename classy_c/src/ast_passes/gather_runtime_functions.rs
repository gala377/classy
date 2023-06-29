use crate::syntax::ast::Visitor;

pub struct GatherRuntimeFunctions {
    pub res: Vec<String>,
}

impl GatherRuntimeFunctions {
    pub fn new() -> Self {
        Self { res: Vec::new() }
    }
}

impl<'a> Visitor<'a> for GatherRuntimeFunctions {
    fn visit_fn_def(&mut self, def: &'a crate::syntax::ast::FunctionDefinition) {
        if def.attributes.contains(&"runtime".to_owned()) {
            self.res.push(def.name.clone());
        }
    }
}