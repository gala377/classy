use std::{collections::HashMap, path::PathBuf};

use classy_syntax::{
    ast,
    lexer::Lexer,
    parser::{Parser, SyntaxError},
};

use thiserror::Error;

use crate::{knowledge::Database, session::Session};

#[derive(Error, Debug)]
pub enum CompilationError {
    #[error("Syntax error: {0:?}")]
    SyntaxError(HashMap<PathBuf, Vec<SyntaxError>>),
}

pub struct Compiler {
    sources: Vec<(PathBuf, String)>,
    package_ast: Vec<ast::SourceFile>,

    session: Session,
    database: Database,
}

impl Compiler {
    pub fn new(package: &str, source: Vec<(PathBuf, String)>) -> Self {
        Compiler {
            sources: source,
            session: Session::new(package),
            package_ast: Vec::new(),
            database: Database::new(),
        }
    }

    pub fn compile_package(&mut self) -> Result<(), CompilationError> {
        self.parse_source_files()?;
        // basically just rewrite ast names in every file
        // based on the import statements in this file
        // self.resolve_imports()?;
        self.populate_db_definitions();
        Ok(())
    }

    pub fn parse_source_files(&mut self) -> Result<(), CompilationError> {
        let mut errors = HashMap::new();
        for (path, source) in &self.sources {
            let lexer = Lexer::new(source);
            let mut parser = Parser::new(lexer);
            let ast = match parser.parse() {
                Ok(ast) => ast,
                Err(err) => {
                    errors.insert(path.clone(), err);
                    continue;
                }
            };
            self.package_ast.push(ast);
        }
        if !errors.is_empty() {
            return Err(CompilationError::SyntaxError(errors));
        }
        Ok(())
    }

    pub fn populate_db_definitions(&mut self) {}
}
