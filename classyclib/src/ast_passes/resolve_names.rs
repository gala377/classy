use std::collections::HashMap;

use classy_syntax::ast::{self, Folder, Visitor};

use crate::{knowledge::Database, scope::Scope};

use super::AstPass;

pub struct NameResolved<'db> {
    database: &'db Database,
    current_namespace_prefix: String,
    variable_scope: Scope<String, ()>,
    type_scope: Scope<String, ()>,
}

impl<'db> NameResolved<'db> {
    pub fn new(database: &'db Database) -> Self {
        Self {
            database,
            current_namespace_prefix: String::new(),
            variable_scope: Scope::new(),
            type_scope: Scope::new(),
        }
    }
}

impl AstPass for NameResolved<'_> {
    fn run(&mut self, ast: ast::SourceFile, _: &crate::session::Session) -> ast::SourceFile {
        self.current_namespace_prefix = ast
            .namespace
            .as_ref()
            .map(|ns| ns.joined())
            .unwrap_or_default();
        self.fold_program(ast)
    }
}

impl<'db> Folder for NameResolved<'db> {
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

    // sequence
    // if
    // while
    // match
    // lambda
    // let
    // let rec
    // name
}
