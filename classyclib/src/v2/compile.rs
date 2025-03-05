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
    v2::{
        constraint_generation::{self, ExprId},
        knowledge::{
            Database, DefinitionId, DefinitionKind, FunctionInfo, MethodInfo, PackageInfo,
            CURRENT_PACKAGE_ID,
        },
        ty::Type,
    },
};

use super::{constraint_solver::CallResolution, rast::build_rast, ty::TypeFolder};

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

    pub fn populate_primitive_types(&mut self) {
        let std_package_id = self
            .database
            .packages_map
            .get("std")
            .expect("std package not found")
            .clone();
        let std_package = self.database.get_package(std_package_id);
        let primitive_types = std_package
            .definition
            .iter()
            .filter_map(|(id, def)| {
                let t = def.ty;
                let t = std_package.typeid_to_type.get(&t).unwrap();
                match t {
                    crate::v2::ty::Type::Int => Some((*id, t.clone())),
                    crate::v2::ty::Type::UInt => Some((*id, t.clone())),
                    crate::v2::ty::Type::Bool => Some((*id, t.clone())),
                    crate::v2::ty::Type::Float => Some((*id, t.clone())),
                    crate::v2::ty::Type::Byte => Some((*id, t.clone())),
                    crate::v2::ty::Type::String => Some((*id, t.clone())),
                    crate::v2::ty::Type::Unit => Some((*id, t.clone())),
                    _ => None,
                }
            })
            .map(|(id, t)| (id.as_global(std_package_id), t))
            .collect::<Vec<_>>();
        for (id, ty) in primitive_types {
            self.database.add_primitive_type(ty.clone(), id);
        }
    }

    pub fn compile_package(&mut self) -> Result<(), CompilationError> {
        println!("🦀Compiling package {}", self.session.current_package());
        println!("🦀Parsing source fiels");
        self.parse_source_files()?;
        println!("🦀After parsing passes");
        self.after_parsing_passes();
        println!("🦀Populating db definitions");
        self.populate_db_definitions();
        println!("🦀Creating type and class stumps");
        self.database.create_type_and_class_stumps(&self.session);
        println!("🦀Lowering type definitions");
        self.database.lower_type_definitions(&self.session);
        println!("🦀Lowering class definitions");
        self.database.lower_class_definitions(&self.session);
        println!("🦀Lowering method blocks");
        self.database.lower_method_blocks(&self.session);
        println!("🦀Lowering instances");
        self.database.lower_instances(&self.session);
        println!("🦀Lowering functions");
        self.database.lower_functions(&self.session);
        println!("🦀Dump database");
        self.database.dump_all();
        // can go anywhere before typechecking
        println!("🦀Populating primitive types");
        self.populate_primitive_types();
        println!("🦀Fold database");
        self.fold_database();
        println!("🦀Typechecking");
        let typechecking_result = self.typecheck();
        println!("🦀Building RAST");
        let rast = build_rast(
            &self.database,
            &typechecking_result.expr_types,
            &typechecking_result.call_resolutions,
            &typechecking_result.name_resolutions,
        );
        println!("{:#?}", rast);
        //render::render_db(&self.database, "./render");
        // generate constraints for all functions and methods in the database
        // solve the constraints
        // create resolved syntax tree for function and methods calls
        // do more lowering later.
        Ok(())
    }

    pub fn fold_database(&mut self) {
        let commit = {
            let resolver = ast_passes::resolve_struct_literal_names::ResolveStructLiteralNames::new(
                &self.database,
            );
            self.database.fold_program(resolver)
        };
        commit(&mut self.database);
    }

    pub fn typecheck(&mut self) -> TypecheckingResult {
        println!("🐍Starting typechecking. Collecting functions");
        let funcs = self
            .database
            .definitions
            .iter()
            .filter_map(|(id, def)| match def.kind {
                DefinitionKind::Function(FunctionInfo { is_abstract, .. })
                | DefinitionKind::Method(MethodInfo { is_abstract, .. })
                    if !is_abstract && !def.annotations.contains("runtime") =>
                {
                    Some(id.as_global(CURRENT_PACKAGE_ID))
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        let mut call_resolutions = HashMap::new();
        let mut substitutions = HashMap::new();
        let mut expr_types = HashMap::<ExprId, Type>::new();
        let mut name_resolutions = HashMap::<ExprId, ast::Name>::new();
        println!("🐍Typechecking functions");
        for func in funcs {
            let mut inferer = constraint_generation::Inferer::for_function(
                func,
                &mut self.database,
                &self.session,
            );
            inferer.infer_function_body();
            expr_types.extend(inferer.expr_types.iter().map(|(k, v)| (*k, v.clone())));
            name_resolutions.extend(
                inferer
                    .trivial_name_resolutions
                    .iter()
                    .map(|(k, v)| (*k, v.clone())),
            );
            let mut solver = inferer.into_constraint_solver();
            solver.solve();
            call_resolutions.extend(solver.call_resolutions);
            substitutions.extend(solver.substitutions);
        }
        println!("🐍Typechecking done");
        println!("🐍 expr_types => {:#?}", expr_types);
        println!("🐍 substitutions => {:#?}", substitutions);
        println!("🐍 call resolutions => {:#?}", call_resolutions);
        let mut substitutor = FreshTypeSubstitutor { substitutions };
        let expr_types = expr_types
            .into_iter()
            .map(|(id, ty)| (id, substitutor.fold_type(ty).unwrap()))
            .collect::<HashMap<_, _>>();
        println!("🐍 expr_types after substitutions {:#?}", expr_types);
        TypecheckingResult {
            expr_types,
            call_resolutions,
            name_resolutions,
        }
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

    pub fn after_types_exist_passes(&mut self) {
        //! TODO
        // We need to promoto some function calls to struct literals
        // We can ignore the name resolutions and delay them to the typechecking
        // phase. Some of the names can be resolved during constraint
        // generation some have to be delayed to constraint solving.
        for (_, ast) in &mut self.package_ast {
            *ast =
                ast_passes::run_after_db_creation_passes(ast.clone(), &self.database, &self.session)
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

#[cfg(test)]
impl Compiler {
    pub fn make_database(mut self) -> (Database, Session) {
        self.parse_source_files().unwrap();
        self.after_parsing_passes();
        self.populate_db_definitions();
        self.database.create_type_and_class_stumps(&self.session);
        self.database.lower_type_definitions(&self.session);
        self.database.lower_class_definitions(&self.session);
        self.database.lower_method_blocks(&self.session);
        self.database.lower_instances(&self.session);
        self.database.lower_functions(&self.session);
        (self.database, self.session)
    }
}

pub struct TypecheckingResult {
    expr_types: HashMap<ExprId, Type>,
    call_resolutions: HashMap<usize, CallResolution>,
    name_resolutions: HashMap<ExprId, ast::Name>,
}

struct FreshTypeSubstitutor {
    substitutions: HashMap<usize, Type>,
}

impl TypeFolder for FreshTypeSubstitutor {
    type Error = ();

    fn fold_fresh(&mut self, id: usize) -> Result<Type, Self::Error> {
        Ok(self
            .substitutions
            .get(&id)
            .cloned()
            .expect(format!("fresh type {} not found", id).as_str()))
    }
}
