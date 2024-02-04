use std::{collections::HashMap, path::PathBuf};

use classy_syntax::{
    ast,
    lexer::Lexer,
    parser::{Parser, SyntaxError},
};

use thiserror::Error;

use crate::{
    ast_passes,
    session::Session,
    v2::knowledge::{Database, DefinitionId, PackageInfo},
};

#[derive(Error, Debug)]
pub enum CompilationError {
    #[error("Syntax error: {0:?}")]
    SyntaxError(HashMap<PathBuf, Vec<SyntaxError>>),
    #[error("Semantic error: {0:?}")]
    SemanticError(String),
}

impl CompilationError {
    pub fn semantic_error(msg: impl Into<String>) -> Self {
        Self::SemanticError(msg.into())
    }
}

pub struct Compiler {
    sources: Vec<(PathBuf, String)>,
    package_ast: Vec<ast::SourceFile>,

    session: Session,
    database: Database,
}

impl Compiler {
    pub fn new(package: &str, sources: Vec<(PathBuf, String)>, packages: Vec<PackageInfo>) -> Self {
        let mut database = Database::new();
        for package in packages {
            database.add_package(package);
        }
        Compiler {
            sources,
            database,
            session: Session::new(package),
            package_ast: Vec::new(),
        }
    }

    pub fn compile_package(&mut self) -> Result<(), CompilationError> {
        self.parse_source_files()?;
        self.after_parsing_passes();
        self.populate_db_definitions();
        Ok(())
    }

    /// Parse source files belonging to this package.
    /// Report aggragated syntax errors that arise during parsing.
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

    /// Runs basic passes that can operate without knowing about other files.
    /// This includes:
    ///     - assigning ids to all nodes
    ///     - verifying lvalues
    ///     - hoisting anonymous types
    ///     - expanding imports
    ///     - exapnding filly namespaced names for top level defnitions
    pub fn after_parsing_passes(&mut self) {
        for ast in &mut self.package_ast {
            *ast = ast_passes::run_after_parsing_passes(ast.clone(), &self.session);
        }
    }

    /// Add all defintions from the parsed AST to the database.
    pub fn populate_db_definitions(&mut self) {
        for ast in &mut self.package_ast {
            for ast::TopLevelItem {
                id,
                kind,
                export: _export,
            } in &mut ast.items
            {
                use ast::TopLevelItemKind::*;
                match kind {
                    TypeDefinition(type_def) => self
                        .database
                        .add_type_definition(DefinitionId(*id), type_def.clone()),
                    FunctionDefinition(fn_def) => self
                        .database
                        .add_function_definition(DefinitionId(*id), fn_def.clone()),
                    ConstDefinition(const_def) => self
                        .database
                        .add_const_definition(DefinitionId(*id), const_def.clone()),
                    MethodsBlock(_) => {
                        // method blocks can be skipped for now.
                        // TODO: Note that is not true in the future as in general we will
                        // want to assign names to method blocks and import them into the scope.
                        continue;
                    }
                    // TODO: We also need to add instance and methods imports here as
                    // visible but also resolved so that they don't need to be typechecked
                    // Also anonymous instances and anonymous methods need to have visibility
                    // figured out. In general, instance or methods that are defined within the
                    // same file as the type are visible everywhere where the type is imported.
                    // Named instances and named method blocks are visible within the namespace
                    // they were defined in but not outside of it unless imported.
                    t => unimplemented!("{t:?}"),
                }
            }
        }
    }
}
