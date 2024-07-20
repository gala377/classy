use classy_syntax::ast::{self, Folder};

use crate::{
    scope::Scope,
    v2::knowledge::{Database, CURRENT_PACKAGE_ID},
};

use super::AstPass;

pub struct NameResolver<'db> {
    database: &'db Database,
    current_namespace_prefix: String,
    variable_scope: Scope<String, ()>,
    type_scope: Scope<String, ()>,
    /// This fields is important for patterns resolutions.
    /// As pattern resolution depends on the type of the matcher
    /// We need to delay this resolution to the constraint solving phase
    dont_resolve_names: bool,
}

impl<'db> NameResolver<'db> {
    pub fn new(database: &'db Database) -> Self {
        Self {
            database,
            current_namespace_prefix: String::new(),
            variable_scope: Scope::with_empty_scope(),
            type_scope: Scope::with_empty_scope(),
            dont_resolve_names: false,
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
            self.type_scope.add(var.clone(), ());
        }
        let res = ast::fold::fold_type_definition(self, def);
        self.type_scope.pop_scope();
        res
    }

    fn fold_methods_block(
        &mut self,
        mut meths: ast::MethodsBlock<ast::FunctionDefinition>,
    ) -> ast::MethodsBlock<ast::FunctionDefinition> {
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

    fn fold_class_definition(
        &mut self,
        ast::ClassDefinition {
            name,
            bounds,
            args,
            body,
        }: ast::ClassDefinition,
    ) -> ast::ClassDefinition {
        self.type_scope.new_scope();
        for var in args.iter() {
            self.type_scope.add(var.clone(), ());
        }
        let bounds = bounds
            .into_iter()
            .map(|ast::TypeBound { head, args }| self.fold_type_bound(head, args))
            .collect();
        // no functions in class definitions have bodies
        // so we don't need to care about an implicit scope from this
        // and can just fold away, all of the name resultion should work as expected.
        let res = ast::fold::fold_class_definition(self, name, args, bounds, body);
        self.type_scope.pop_scope();
        res
    }

    fn fold_poly_type(
        &mut self,
        vars: Vec<String>,
        bounds: Vec<ast::TypeBound>,
        typ: ast::Typ,
    ) -> ast::Typ {
        self.type_scope.new_scope();
        for var in vars.iter() {
            self.type_scope.add(var.clone(), ());
        }
        let bounds = bounds
            .into_iter()
            .map(|ast::TypeBound { head, args }| self.fold_type_bound(head, args))
            .collect();
        let typ = ast::fold::fold_poly_type(self, vars, bounds, typ);
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
                // Do not resolve names in patterns as patterns are
                // type sensitive. We need to delay the check until constraint solving.
                self.dont_resolve_names = true;
                let pat = ast::fold::fold_pattern(self, pat);
                self.dont_resolve_names = false;
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
        
        ast::fold::fold_let_rec(self, definitions)
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
        if self.dont_resolve_names {
            return name;
        }
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
        let definition = database.get_global(&full_name).unwrap_or_else(|| panic!("Could not find global definition with name {full_name}"));
        return ast::Name::Global {
            package: CURRENT_PACKAGE_ID.0,
            definition: definition.0 .0,
        };
    }
    let possible_package = path.first().unwrap();
    let possible_package = database.package_id(possible_package);
    match possible_package {
        Some(package_id) => {
            let package = database.get_package(package_id);
            path.push(identifier);
            let full_name = path[1..].join("::");
            let definition = package.globals.get(&full_name).unwrap();
            ast::Name::Global {
                package: package_id.0,
                definition: definition.0 .0,
            }
        }
        None => {
            path.push(identifier);
            path.insert(0, current_namespace_prefix.to_string());
            let full_name = path.join("::");
            let definition = database.get_global(&full_name).unwrap();
            ast::Name::Global {
                package: CURRENT_PACKAGE_ID.0,
                definition: definition.0 .0,
            }
        }
    }
}
