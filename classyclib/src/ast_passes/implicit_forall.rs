use core::panic;

use classy_syntax::ast::{self, Folder};

use crate::{scope::Scope, session::Session};

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
    fn run(&mut self, ast: ast::Program, _: &Session) -> ast::Program {
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
        let new_t = if !self.prefex.is_empty() {
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
                        generics: self.prefex.to_vec(),
                        args,
                        ret,
                    }
                }
                t => ast::Typ::Poly(self.prefex.to_vec(), Box::new(t)),
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

    fn fold_name_type(&mut self, name: ast::Name) -> ast::Typ {
        let ast::Name::Unresolved { path, identifier } = name else {
            panic!("expected unresolved name")
        };
        if !path.is_empty() {
            return ast::Typ::Name(ast::Name::Unresolved { path, identifier });
        }
        if identifier.chars().all(|c| c.is_lowercase())
            && !self.ignore_names.contains(&identifier)
            && !self.prefex.contains(&identifier)
        {
            self.prefex.push(identifier.clone());
        }
        ast::Typ::Name(ast::Name::Unresolved {
            path: vec![],
            identifier,
        })
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

    fn fold_methods_block(&mut self, meths: ast::MethodsBlock) -> ast::MethodsBlock {
        let typ = self.fold_methods_type(meths.typ);
        let generics = match &typ {
            ast::Typ::Poly(generics, _) => generics.clone(),
            ast::Typ::Function { generics, .. } => generics.clone(),
            _ => vec![],
        };
        self.ignore_names.new_scope();
        for name in &generics {
            self.ignore_names.add(name.clone(), ());
        }
        let methods = meths
            .methods
            .into_iter()
            .map(|def| ast::fold::fold_function_definition(self, def))
            .collect();
        self.ignore_names.pop_scope();

        ast::MethodsBlock {
            name: meths.name,
            typ,
            methods,
        }
    }
}

impl ImplicitForall {
    fn fold_methods_type(&mut self, typ: ast::Typ) -> ast::Typ {
        self.prefex.clear();
        let new_t = self.fold_typ(typ);
        if !self.prefex.is_empty() {
            ast::Typ::Poly(self.prefex.to_vec(), Box::new(new_t))
        } else {
            new_t
        }
    }
}
