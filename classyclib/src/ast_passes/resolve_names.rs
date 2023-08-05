use classy_syntax::ast::{self, Folder};

use crate::{
    knowledge::{Database, CURRENT_PACKAGE_ID},
    scope::Scope,
};

use super::AstPass;

enum ResolutionContext {
    Type,
    Variable,
}

pub struct NameResolver<'db> {
    database: &'db Database,
    current_namespace_prefix: String,
    variable_scope: Scope<String, ()>,
    type_scope: Scope<String, ()>,
    context: ResolutionContext,
}

impl<'db> NameResolver<'db> {
    pub fn new(database: &'db Database) -> Self {
        Self {
            database,
            current_namespace_prefix: String::new(),
            variable_scope: Scope::new(),
            type_scope: Scope::new(),
            context: ResolutionContext::Variable,
        }
    }
}

impl AstPass for NameResolver<'_> {
    fn run(&mut self, ast: ast::SourceFile, _: &crate::session::Session) -> ast::SourceFile {
        self.current_namespace_prefix = ast
            .namespace
            .as_ref()
            .map(ast::Namespace::joined)
            .unwrap_or_default();
        self.fold_program(ast)
    }
}

impl<'db> Folder for NameResolver<'db> {
    fn fold_function_definition(
        &mut self,
        def: ast::FunctionDefinition,
    ) -> ast::FunctionDefinition {
        self.variable_scope.new_scope();
        self.type_scope.new_scope();
        for var in def.parameters.iter() {
            self.type_scope.add(var.clone(), ());
        }
        for arg in def.parameters.iter() {
            self.variable_scope.add(arg.clone(), ());
        }
        let res = ast::fold::fold_function_definition(self, def);
        self.type_scope.pop_scope();
        self.variable_scope.pop_scope();
        res
    }

    fn fold_type_definition(&mut self, def: ast::TypeDefinition) -> ast::TypeDefinition {
        self.type_scope.new_scope();
        for var in def.type_variables.iter() {
            self.type_scope.add(var.name.clone(), ());
        }
        let res = ast::fold::fold_type_definition(self, def);
        self.type_scope.pop_scope();
        res
    }

    fn fold_methods_block(&mut self, mut meths: ast::MethodsBlock) -> ast::MethodsBlock {
        // this expansion needs to be delayed until we resolve type aliases.
        // This is required as we have implicit `this` and as such need to
        // look into the type to know what fields are within scope.
        eprintln!(
            "WARNING: Methods are not getting name expanded. Remember to expand them in a later \
             stage"
        );
        meths.typ = self.fold_typ(meths.typ);
        meths
    }

    fn fold_poly_type(&mut self, vars: Vec<String>, typ: ast::Typ) -> ast::Typ {
        self.type_scope.new_scope();
        for var in vars.iter() {
            self.type_scope.add(var.clone(), ());
        }
        let typ = ast::fold::fold_poly_type(self, vars, typ);
        self.type_scope.pop_scope();
        typ
    }

    fn fold_sequence(&mut self, seq: Vec<ast::Expr>) -> Vec<ast::Expr> {
        self.variable_scope.new_scope();
        let seq = ast::fold::fold_sequence(self, seq);
        self.variable_scope.pop_scope();
        seq
    }

    fn fold_if(
        &mut self,
        cond: ast::Expr,
        body: ast::Expr,
        else_body: Option<ast::Expr>,
    ) -> ast::ExprKind {
        self.variable_scope.new_scope();
        let res = ast::fold::fold_if(self, cond, body, else_body);
        self.variable_scope.pop_scope();
        res
    }

    fn fold_while(&mut self, cond: ast::Expr, body: ast::Expr) -> ast::ExprKind {
        self.variable_scope.new_scope();
        let res = ast::fold::fold_while(self, cond, body);
        self.variable_scope.pop_scope();
        res
    }

    fn fold_match(
        &mut self,
        expr: ast::Expr,
        cases: Vec<(ast::Pattern, ast::Expr, Option<Box<ast::Expr>>)>,
    ) -> ast::ExprKind {
        let expr = ast::fold::fold_expr(self, expr);
        let cases = cases
            .into_iter()
            .map(|(pat, body, guard)| {
                self.variable_scope.new_scope();
                let pat = ast::fold::fold_pattern(self, pat);
                let guard = guard.map(|g| ast::fold::fold_expr(self, *g)).map(Box::new);
                let body = ast::fold::fold_expr(self, body);
                self.variable_scope.pop_scope();
                (pat, body, guard)
            })
            .collect();
        ast::ExprKind::Match {
            expr: Box::new(expr),
            cases,
        }
    }

    fn fold_lambda(&mut self, params: Vec<ast::TypedIdentifier>, body: ast::Expr) -> ast::ExprKind {
        self.variable_scope.new_scope();
        for param in params.iter() {
            self.variable_scope.add(param.name.clone(), ());
        }
        let res = ast::fold::fold_lambda(self, params, body);
        self.variable_scope.pop_scope();
        res
    }

    fn fold_let_rec(&mut self, definitions: Vec<ast::FunctionDefinition>) -> ast::ExprKind {
        for def in definitions.iter() {
            self.variable_scope.add(def.name.clone(), ());
        }
        let res = ast::fold::fold_let_rec(self, definitions);
        res
    }

    fn fold_local_function_def(&mut self, def: ast::FunctionDefinition) -> ast::FunctionDefinition {
        self.variable_scope.new_scope();
        for var in def.parameters.iter() {
            self.variable_scope.add(var.clone(), ());
        }
        let res = ast::fold::fold_local_function_def(self, def);
        self.variable_scope.pop_scope();
        res
    }

    fn fold_let(&mut self, name: String, typ: ast::Typ, init: ast::Expr) -> ast::ExprKind {
        let res = ast::fold::fold_let(self, name.clone(), typ, init);
        self.variable_scope.add(name, ());
        res
    }

    fn fold_name(&mut self, name: ast::Name) -> ast::Name {
        resolve_name(
            self.database,
            &self.variable_scope,
            &self.current_namespace_prefix,
            name,
        )
    }

    fn fold_name_type(&mut self, name: ast::Name) -> ast::Typ {
        let name = resolve_name(
            self.database,
            &self.type_scope,
            &self.current_namespace_prefix,
            name,
        );
        ast::Typ::Name(name)
    }
}

fn resolve_name(
    database: &Database,
    resolution_scope: &Scope<String, ()>,
    current_namespace_prefix: &str,
    name: ast::Name,
) -> ast::Name {
    let ast::Name::Unresolved {
        mut path,
        identifier,
    } = name
    else {
        panic!("Name should be unresolved at this stage")
    };
    if path.is_empty() {
        if resolution_scope.contains(&identifier) {
            return ast::Name::Local(identifier);
        }

        let full_name = if current_namespace_prefix.is_empty() {
            identifier.clone()
        } else {
            format!("{}::{}", current_namespace_prefix, identifier)
        };
        let definition = database.get_global(&full_name).expect(&format!(
            "Could not find global definition with name {full_name}"
        ));
        return ast::Name::Global {
            package: CURRENT_PACKAGE_ID.0,
            definition: definition.0,
        };
    }
    let possible_package = path.first().clone().unwrap();
    let possible_package = database.package_id(&possible_package);
    match possible_package {
        Some(package_id) => {
            let package = database.get_package(package_id.clone());
            path.push(identifier);
            let full_name = path[1..].join("::");
            let definition = package.get_definition(&full_name).unwrap();
            ast::Name::Global {
                package: package_id.0,
                definition: definition.0,
            }
        }
        None => {
            path.push(identifier);
            path.insert(0, current_namespace_prefix.to_string());
            let full_name = path.join("::");
            let definition = database.get_global(&full_name).unwrap();
            ast::Name::Global {
                package: CURRENT_PACKAGE_ID.0,
                definition: definition.0,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use classy_sexpr::ToSExpr;
    use classy_sexpr_proc_macro::sexpr;
    use classy_syntax::ast;

    use crate::{
        ast_passes::AstPass,
        knowledge::{Database, DefinitionId, TypeHashMap},
        session::Session,
    };
    use std::collections::HashMap;

    fn test_db(
        packages: HashMap<String, HashMap<String, usize>>,
        definitions: HashMap<String, usize>,
    ) -> Database {
        let mut db = Database {
            packages: Vec::new(),
            packages_map: HashMap::new(),
            variable_definitions: HashMap::new(),
            function_definitions: HashMap::new(),
            type_definitions: HashMap::new(),
            globals: HashMap::new(),
            definition_types: HashMap::new(),
            type_aliases: HashMap::new(),
            reverse_type_aliases: TypeHashMap::new(100),
            method_blocks: HashMap::new(),
        };
        for (name, id) in definitions {
            db.globals.insert(name, DefinitionId(id));
        }
        for (name, globals) in packages {
            let package = crate::knowledge::PackageInfo {
                name,
                globals: globals
                    .into_iter()
                    .map(|(name, id)| (name, DefinitionId(id)))
                    .collect(),
            };
            db.add_package(package);
        }
        db
    }

    fn parse_source(source: &str) -> ast::SourceFile {
        let lexer = classy_syntax::lexer::Lexer::new(source);
        let mut parser = classy_syntax::parser::Parser::new(lexer);
        parser.parse().unwrap()
    }

    fn run_test(
        source: &str,
        expected: classy_sexpr::SExpr,
        definitions: HashMap<String, usize>,
        packages: HashMap<String, HashMap<String, usize>>,
    ) {
        let ast = parse_source(source);
        let db = test_db(packages, definitions);
        let mut pass = super::NameResolver::new(&db);
        let sess = Session::new("test");
        let ast = pass.run(ast, &sess);
        let actual = ast.to_sexpr();
        similar_asserts::assert_eq!(actual, expected);
    }

    macro_rules! map {
        ($($key:literal -> $val:expr),*) => {
            {
                #[allow(unused_mut)]
                let mut m = std::collections::HashMap::new();
                $(
                    m.insert($key.into(), $val);
                )*
                m
            }
        };
    }

    #[test]
    fn resolves_globals_without_namespace_correctly() {
        run_test(
            r"
                foo = x
            ",
            sexpr!((
                (fn {}
                    (type (fn [] () infere))
                    foo () (global 0 10)
                )
            )),
            map! { "x" -> 10 },
            map! {},
        );
    }

    #[test]
    fn resolves_globals_within_other_packages() {
        run_test(
            r"
                foo = foo::x
            ",
            sexpr!((
                (fn {}
                    (type (fn [] () infere))
                    foo () (global 1 10)
                )
            )),
            map! {},
            map! {
                "foo" -> map! {
                    "x" -> 10
                }
            },
        );
    }

    #[test]
    fn resolves_names_in_the_current_namespace() {
        run_test(
            r"
                namespace a::b

                foo = x
            ",
            sexpr!((
                (namespace a::b)
                (fn {}
                    (type (fn [] () infere))
                    foo () (global 0 11)
                )
            )),
            map! {
                "a::b::x" -> 11
            },
            map! {},
        );
    }

    #[test]
    fn resolves_names_in_nested_namespace() {
        run_test(
            r"
                namespace a::b

                foo = c::d::x
            ",
            sexpr!((
                (namespace a::b)
                (fn {}
                    (type (fn [] () infere))
                    foo () (global 0 11)
                )
            )),
            map! {
                "a::b::c::d::x" -> 11
            },
            map! {},
        );
    }

    #[test]

    fn resolves_names_in_namespaces_of_packages() {
        run_test(
            r"
                foo = foo::a::b::x
            ",
            sexpr!((
                (fn {}
                    (type (fn [] () infere))
                    foo () (global 1 11)
                )
            )),
            map! {},
            map! {
                "foo" -> map! {
                    "a::b::x" -> 11
                }
            },
        );
    }

    #[test]

    fn mixing_namespace_and_package_names_works_prioritizing_package_names() {
        run_test(
            r"
                namespace a::b

                foo = foo::a::b::x
            ",
            sexpr!((
                (namespace a::b)
                (fn {}
                    (type (fn [] () infere))
                    foo () (global 1 11)
                )
            )),
            map! {},
            map! {
                "foo" -> map! {
                    "a::b::x" -> 11
                }
            },
        );
    }

    #[test]
    fn resplves_local_names() {
        run_test(
            r"
                foo a {
                    foo()
                    let x = 10
                    x
                    a
                    if (true) {
                        let c = 1
                        c
                    } else {
                        a
                    }
                    c
                }
            ",
            sexpr!((
                (fn {}
                    (type (fn [] (infere) infere))
                    foo (a) {
                        (call (global 0 1) () {})
                        (let x infere 10)
                        x
                        a
                        (if true {
                            (let c infere 1)
                            c
                        }
                        { a })
                        (global 0 10)
                    }
                )
            )),
            map! {"c" -> 10, "foo" -> 1},
            map! {},
        );
    }

    #[test]
    fn type_name_resolution() {
        todo!()
    }

    #[test]
    fn type_name_resolution_with_forall() {
        todo!()
    }
}
