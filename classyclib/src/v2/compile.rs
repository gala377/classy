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
    package_ast: Vec<(PathBuf, ast::SourceFile)>,

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
        self.database.create_type_and_class_stumps(&self.session);
        self.database.lower_type_definitions(&self.session);
        self.database.lower_class_definitions(&self.session);
        self.database.dump_all();
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
            self.package_ast.push((path.clone(), ast));
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
    ///     - exapnding filly namespaced names for top level defnitions
    pub fn after_parsing_passes(&mut self) {
        for (_, ast) in &mut self.package_ast {
            *ast = ast_passes::run_after_parsing_passes(ast.clone(), &self.session);
        }
    }

    /// Add all defintions from the parsed AST to the database.
    pub fn populate_db_definitions(&mut self) {
        for (path, ast) in &mut self.package_ast {
            for ast::TopLevelItem {
                id,
                kind,
                export: _export,
            } in &mut ast.items
            {
                let namespace = ast
                    .namespace
                    .as_ref()
                    .map(|ns| ns.as_segments())
                    .unwrap_or_default();
                let file = self.database.add_file(&self.session, &path, &namespace);
                use ast::TopLevelItemKind::*;
                match kind {
                    TypeDefinition(type_def) => {
                        self.database
                            .add_type_definition(DefinitionId(*id), type_def.clone(), file)
                    }
                    FunctionDefinition(fn_def) => self.database.add_function_definition(
                        DefinitionId(*id),
                        fn_def.clone(),
                        file,
                    ),
                    ConstDefinition(const_def) => self.database.add_const_definition(
                        DefinitionId(*id),
                        const_def.clone(),
                        file,
                    ),
                    MethodsBlock(meths_def) => self.database.add_method_block_definition(
                        DefinitionId(*id),
                        meths_def.clone(),
                        file,
                    ),
                    ClassDefinition(class_def) => self.database.add_class_definition(
                        DefinitionId(*id),
                        class_def.clone(),
                        file,
                    ),
                    InstanceDefinition(inst_def) => self.database.add_instance_definition(
                        DefinitionId(*id),
                        inst_def.clone(),
                        file,
                    ),
                    t => unimplemented!("{:?}", t),
                }
            }
        }
    }
}
