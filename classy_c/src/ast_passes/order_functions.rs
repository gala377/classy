use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::syntax::ast;

enum Mode {
    GatherFunctions,
    BuildGraph,
}
impl Default for Mode {
    fn default() -> Self {
        Self::GatherFunctions
    }
}

#[derive(Default)]
pub struct FunctionsOrderer {
    /// Top level function names
    /// Also information if it has type annotation
    functions: HashMap<String, (usize, bool)>,
    /// Variables defined within the current function
    variables: HashSet<String>,
    order: Vec<String>,
    /// Standar graph where graph[i] is a list of edges from i
    graph: Vec<Vec<usize>>,
    mode: Mode,
    curr_id: usize,
}

impl FunctionsOrderer {
    pub fn new() -> Self {
        Default::default()
    }
}

impl FunctionsOrderer {
    pub fn id(&self) -> usize {
        self.order.len()
    }

    pub fn order(&mut self) {
        let cycles = CycleFinder::new(&self.graph).find_cycles();
        let roots = self
            .graph
            .iter()
            .map(|v| v.is_empty())
            .enumerate()
            .filter(|(_, b)| *b)
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        let all = Vec::from_iter(cycles.iter().chain(roots.iter()));
        for i in all {
            let name = self.order[*i].clone();
            let (_, annotation) = self.functions[&name];
            if !annotation {
                panic!("Function {name} requires type annotation");
            }
        }
    }
}

impl<'ast> ast::Visitor<'ast> for FunctionsOrderer {
    fn visit(&mut self, prog: &'ast ast::Program) {
        for i in &prog.items {
            self.visit_top_level_item(i);
        }
        self.mode = Mode::BuildGraph;
        for i in &prog.items {
            self.visit_top_level_item(i);
        }
    }

    fn visit_fn_def(&mut self, def: &'ast ast::FunctionDefinition) {
        match self.mode {
            Mode::GatherFunctions => {
                let id = self.id();
                let annotation = match def.typ {
                    ast::Typ::ToInfere => false,
                    _ => true,
                };
                self.functions.insert(def.name.clone(), (id, annotation));
                self.order.push(def.name.clone());
                self.graph.push(Vec::new());
            }
            Mode::BuildGraph => {
                self.variables.clear();
                (self.curr_id, _) = self.functions[&def.name];
                for arg in &def.parameters {
                    self.variables.insert(arg.clone());
                }
                self.visit_expr(&def.body);
            }
        }
    }

    fn visit_let(&mut self, name: &'ast str, _: &'ast ast::Typ, init: &'ast ast::Expr) {
        self.variables.insert(name.to_string());
        self.visit_expr(init);
    }

    fn visit_name(&mut self, node: &'ast str) {
        if self.variables.contains(node) {
            return;
        }
        if self.functions.contains_key(node) {
            let (id, _) = self.functions[node];
            self.graph[self.curr_id].push(id);
        }
    }
}

struct CycleFinder<'graph> {
    graph: &'graph Vec<Vec<usize>>,
    visited: HashSet<usize>,
}

impl<'g> CycleFinder<'g> {
    pub fn new(graph: &'g Vec<Vec<usize>>) -> Self {
        Self {
            graph,
            visited: HashSet::new(),
        }
    }

    pub fn find_cycles(&mut self) -> Vec<usize> {
        let mut cycles = Vec::new();
        for i in 0..self.graph.len() {
            if self.visited.contains(&i) {
                continue;
            }
            let mut stack = HashSet::new();
            self.dfs(i, &mut stack, &mut cycles);
        }
        cycles
    }

    fn dfs(&mut self, node: usize, stack: &mut HashSet<usize>, cycles: &mut Vec<usize>) {
        if stack.contains(&node) {
            cycles.push(node);
            return;
        }
        stack.insert(node);
        if self.visited.contains(&node) {
            return;
        }
        self.visited.insert(node);
        for child in &self.graph[node] {
            self.dfs(*child, stack, cycles);
        }
        stack.remove(&node);
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::ast::{self, Visitor};

    fn mk_ast(source: &str) -> ast::Program {
        let lexer = crate::syntax::lexer::Lexer::new(source);
        let mut parser = crate::syntax::parser::Parser::new(lexer);
        parser.parse().unwrap()
    }

    #[test]
    fn does_not_panic_on_empty_program() {
        let ast = mk_ast("");
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        orderer.order();
    }

    #[test]
    fn does_not_panic_on_function_without_dependencies() {
        let ast = mk_ast("main: () ->();main() = ();");
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        orderer.order();
    }

    #[test]
    fn does_not_panic_ontwo_functions_with_dependencies_with_annotations() {
        let ast = mk_ast(
            "
            main: () -> ();
            main() {
                f()
            }
            f: () -> ();
            f() = ()

            ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        orderer.order();
    }
}