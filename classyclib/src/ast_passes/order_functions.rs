use std::collections::{HashMap, HashSet};

use classy_syntax::ast;

/// TODO: Involve methods into the ordering

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
    /// and the count of arguments (annotation not needed if only return type
    /// has has to be inferred)
    functions: HashMap<String, (usize, bool, usize)>,

    // TODO: Change it to scope so that the variables get removed
    // when they go aout of scope of match, if, while and so on
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

    pub fn order(&mut self) -> Vec<String> {
        let cycles = CycleFinder::new(&self.graph).find_cycles();
        self.error_for_annotations(&cycles);
        let sorted = self.topological_sort(&cycles);
        sorted.into_iter().map(|i| self.order[i].clone()).collect()
    }

    fn topological_sort(&mut self, cycles: &Vec<usize>) -> Vec<usize> {
        let mut roots = self
            .graph
            .iter()
            .map(|v| v.is_empty())
            .enumerate()
            .filter(|(_, b)| *b)
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        roots.extend(cycles);
        println!("Functions: {:?}", self.functions);
        println!("Cycles: {:?}", cycles);
        println!("Roots: {:?}", roots);
        let mut sorted = Vec::new();
        while let Some(root) = roots.pop() {
            println!("Checking function {}", self.order[root]);
            sorted.push(root);
            for i in 0..self.graph.len() {
                if self.graph[i].contains(&root) {
                    self.graph[i].retain(|&x| x != root);
                    if self.graph[i].is_empty() {
                        roots.push(i);
                    }
                }
            }
        }
        sorted.dedup();
        sorted.retain(|x| !cycles.contains(x));
        sorted.extend(cycles);
        sorted
    }

    fn error_for_annotations(&self, cycles: &[usize]) {
        // let mut roots = self
        //     .graph
        //     .iter()
        //     .map(|v| v.is_empty())
        //     .enumerate()
        //     .filter(|(_, b)| *b)
        //     .map(|(i, _)| i)
        //     .collect::<Vec<_>>();
        // roots.retain(|f| {
        //     let (_, _, argc) = self.functions[&self.order[*f]];
        //     argc != 0
        // });
        // let all = Vec::from_iter(cycles.iter().chain(roots.iter()));
        let all = Vec::from_iter(cycles.iter());
        for i in all {
            let name = self.order[*i].clone();
            let (_, annotation, _) = self.functions[&name];
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
                let annotation = !matches!(
                    &def.typ,
                    ast::Typ::Function { ret, .. } if **ret == ast::Typ::ToInfere);
                let argc = def.parameters.len();
                self.functions
                    .insert(def.name.clone(), (id, annotation, argc));
                self.order.push(def.name.clone());
                self.graph.push(Vec::new());
            }
            Mode::BuildGraph => {
                self.variables.clear();
                (self.curr_id, _, _) = self.functions[&def.name];
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
            let (id, _, _) = self.functions[node];
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
        println!("Graph: {:?}", self.graph);
        for i in 0..self.graph.len() {
            if self.visited.contains(&i) {
                continue;
            }
            let mut stack = HashSet::new();
            println!("Running dfs from {}", i);
            self.dfs(i, &mut stack, &mut cycles);
        }
        cycles
    }

    fn dfs(&mut self, node: usize, stack: &mut HashSet<usize>, cycles: &mut Vec<usize>) {
        println!("Visiting {}", node);
        if stack.contains(&node) {
            println!("Found cycle, stack {stack:?}");
            cycles.push(node);
            return;
        }
        if self.visited.contains(&node) {
            return;
        }
        stack.insert(node);
        self.visited.insert(node);
        for child in &self.graph[node] {
            self.dfs(*child, stack, cycles);
        }
        stack.remove(&node);
    }
}

#[cfg(test)]
mod tests {
    use classy_syntax::ast::{self, Visitor};

    fn mk_ast(source: &str) -> ast::Program {
        let lexer = classy_syntax::lexer::Lexer::new(source);
        let mut parser = classy_syntax::parser::Parser::new(lexer);
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

    #[test]
    fn check_order_for_simple_non_recursive_chain() {
        let ast = mk_ast(
            "
            main: () -> ();
            main() {
                f()
            }
            f: () -> ();
            f = g()

            g: () -> ()
            g() = ()

            ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        let order = orderer.order();
        assert_eq!(order, vec!["g", "f", "main"]);
    }

    #[test]
    fn check_order_with_recursive_function() {
        let ast = mk_ast(
            "
            main: () -> ();
            main() {
                f()
            }
            f: () -> ();
            f = f();
            ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        let order = orderer.order();
        assert_eq!(order, vec!["main", "f"]);
    }

    #[test]
    fn check_recursive_function_with_dependencies() {
        let ast = mk_ast(
            "
            main: () -> ();
            main() = ();

            f: () -> ();
            f = {
                main()
                g()
                f()
            }

            g: () -> ();
            g = ();
            ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        let order = orderer.order();
        assert_eq!(order, vec!["g", "main", "f"]);
    }

    #[test]
    fn check_some_other_tree() {
        let ast = mk_ast(
            "
            main: () -> ();
            main() = ();

            f: () -> ();
            f = {
                main()
                g()
                f()
            }

            g: () -> ();
            g = main();
            ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        let order = orderer.order();
        assert_eq!(order, vec!["main", "g", "f"]);
    }

    #[test]
    fn check_mutaul_recursion() {
        let ast = mk_ast(
            "
            main: () -> ();
            main() = f();

            f: () -> ();
            f = {
                g()
            }

            g: () -> ();
            g = f();
            ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        let order = orderer.order();
        assert_eq!(order, vec!["g", "main", "f"]);
    }

    #[test]
    fn order_inferrable_functions() {
        let ast = mk_ast(
            "
            a = b()
            b = c()
            c = ()
        ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        let order = orderer.order();
        assert_eq!(order, vec!["c", "b", "a"]);
    }

    #[test]
    fn simple_polymorphic_funcion_with_no_annotation1() {
        let ast = mk_ast(
            "
            id x = x
        ",
        );
        let mut orderer = super::FunctionsOrderer::new();
        orderer.visit(&ast);
        let order = orderer.order();
        assert_eq!(order, vec!["id"]);
    }
}
