use crate::{
    scope::Scope,
    syntax::ast::{self, Folder},
};

use super::AstPass;

type PrefexScope = Scope<String, ()>;

#[derive(Default)]
pub struct ImplicitForall {
    prefex: Vec<String>,
    ignore_names: PrefexScope,
}

impl ImplicitForall {
    pub fn new() -> Self {
        Self {
            prefex: Vec::new(),
            ignore_names: PrefexScope::new(),
        }
    }
}

impl AstPass for ImplicitForall {
    fn run(&mut self, ast: ast::Program) -> ast::Program {
        self.fold_program(ast)
    }
}

impl ast::Folder for ImplicitForall {
    fn fold_function_definition(
        &mut self,
        ast::FunctionDefinition {
            name,
            typ,
            parameters,
            body,
            attributes,
        }: ast::FunctionDefinition,
    ) -> ast::FunctionDefinition {
        self.prefex.clear();
        let new_t = self.fold_typ(typ);
        let new_t = if self.prefex.len() > 0 {
            match new_t {
                ast::Typ::Function {
                    generics,
                    args,
                    ret,
                } => {
                    if !generics.is_empty() {
                        panic!("not all generics mentioned in the forall section")
                    }
                    ast::Typ::Function {
                        generics: self.prefex.iter().cloned().collect(),
                        args,
                        ret,
                    }
                }
                t => ast::Typ::Poly(self.prefex.iter().cloned().collect(), Box::new(t)),
            }
        } else {
            new_t
        };
        ast::FunctionDefinition {
            name,
            typ: new_t,
            parameters,
            body,
            attributes,
        }
    }

    fn fold_name_type(&mut self, name: String) -> ast::Typ {
        if name.chars().all(|c| c.is_lowercase())
            && !self.ignore_names.contains(&name)
            && !self.prefex.contains(&name)
        {
            self.prefex.push(name.clone());
        }
        ast::Typ::Name(name)
    }

    fn fold_function_type(
        &mut self,
        generics: Vec<String>,
        args: Vec<ast::Typ>,
        ret: ast::Typ,
    ) -> ast::Typ {
        self.ignore_names.new_scope();
        for name in &generics {
            self.ignore_names.add(name.clone(), ());
        }
        let res = ast::fold::fold_function_type(self, generics, args, ret);
        self.ignore_names.pop_scope();
        res
    }

    fn fold_poly_type(&mut self, vars: Vec<String>, typ: ast::Typ) -> ast::Typ {
        self.ignore_names.new_scope();
        for name in &vars {
            self.ignore_names.add(name.clone(), ());
        }
        let res = ast::fold::fold_poly_type(self, vars, typ);
        self.ignore_names.pop_scope();
        res
    }
}
