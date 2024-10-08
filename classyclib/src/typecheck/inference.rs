use core::panic;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter,
    rc::Rc,
};

use classy_blackboard::database::{self, AnswerOrigin, Database as BlackboardDatabase, GenericRef};
use classy_syntax::ast::{self, ExprKind, FunctionDefinition, MethodsBlock, Visitor};

use crate::{
    id_provider::UniqueId,
    session::{Session, SharedIdProvider},
    typecheck::{
        constraints::Constraint,
        constrait_solver::{instance, ConstraintSolver, FreshTypeReplacer},
        fix_fresh::{self, generalize_type_above, generalize_types},
        scope::Scope,
        type_context::TypCtx,
        types::{Type, TypeFolder},
    },
};

use super::{
    ast_to_type::PrefexScope,
    type_context::{DefId, MethodSet},
    types::DeBruijn,
};

/// Maps unique node id in the ast with its type.
pub type TypeEnv = HashMap<usize, Type>;
/// Maps a given method call id with the resolved definition id.
pub type MethodResolution = HashMap<usize, DefId>;

pub fn run(
    tctx: &mut TypCtx,
    ast: &ast::SourceFile,
    session: &Session,
) -> (TypeEnv, MethodResolution) {
    tctx.remove_to_infere_type();

    let mut cons = Inference::infer_types(tctx, ast, session);

    println!("{}", tctx.debug_string());
    println!("SUBSTITUTIONS");
    println!("{}", tctx.debug_string());
    println!("ENV IS:");
    for (id, typ) in cons.env.iter() {
        let mut node = GetNode {
            id: *id,
            found: None,
        };
        node.visit(ast);
        let typ_repr = format!("{:?}", typ);
        println!(
            "{} -> {typ_repr:<25}|{:<50}",
            id,
            node.found
                .map(|x| x.pretty())
                .unwrap_or_else(|| "NOT FOUND".to_owned()),
        );
    }
    remove_aliases_in_env(&mut cons.env, tctx);
    (cons.env, cons.resolved_method_calls)
}

struct GetNode {
    id: usize,
    found: Option<ast::Expr>,
}

impl<'ast> ast::Visitor<'ast> for GetNode {
    fn visit_expr(&mut self, node: &'ast ast::Expr) {
        if node.id == self.id {
            self.found = Some(node.clone());
        }
        ast::visitor::walk_expr(self, node);
    }
}

pub(super) struct Inference<'sess> {
    /// the scope, maps names to types
    scope: Rc<RefCell<Scope>>,
    /// allows matching expression ids with types
    env: HashMap<UniqueId, Type>,
    /// Constraints generated for this mutually recursive function group.
    constraints: Vec<Constraint>,
    /// Return type of checked function.
    ret_t: Option<Type>,
    call_stack: Vec<String>,
    /// Names of functions that have been already typechecked so we can just get
    /// it's type straight away instead of pushing it onto the inference
    /// stack.
    already_typechecked: HashSet<String>,
    /// A set of function names that wait for their generalization to happen
    /// after all of the functions within the mutually recursive group they
    /// belong to have their constraints generated and solved.
    ///
    /// We will generalize all of those functions when we can. So when the
    /// generalize_after counter hits 0.
    typecheck_until_generalization: HashSet<String>,
    generalize_after: usize,
    id_provider: SharedIdProvider,
    session: &'sess Session,

    /// Database used for constraint solving, to resolve methods and instances.
    /// The database is derived from the type context.
    blackboard_database: Rc<classy_blackboard::database::Database>,
    definitions: Rc<HashMap<GenericRef, DefId>>,
    /// Maps method call ids to the resolved method definition id
    resolved_method_calls: HashMap<usize, DefId>,
}

impl<'sess> Inference<'sess> {
    pub fn new(
        scope: Rc<RefCell<Scope>>,
        session: &'sess Session,
        blackboard_database: classy_blackboard::database::Database,
        definitions: HashMap<GenericRef, DefId>,
    ) -> Self {
        Self {
            scope,
            env: HashMap::new(),
            constraints: Vec::new(),
            id_provider: session.id_provider(),
            ret_t: None,
            already_typechecked: HashSet::new(),
            typecheck_until_generalization: HashSet::new(),
            call_stack: Vec::new(),
            generalize_after: 0,
            session,
            blackboard_database: Rc::new(blackboard_database),
            definitions: Rc::new(definitions),
            resolved_method_calls: HashMap::new(),
        }
    }

    pub fn set_ret_t(&mut self, t: Option<Type>) {
        self.ret_t = t;
    }

    pub fn in_scope(
        &mut self,
        scope: Rc<RefCell<Scope>>,
        f: impl FnOnce(&mut Inference) -> Type,
    ) -> Type {
        let mut sub = Self {
            scope,
            env: HashMap::new(),
            constraints: Vec::new(),
            id_provider: self.id_provider.clone(),
            ret_t: self.ret_t.clone(),
            call_stack: self.call_stack.clone(),
            typecheck_until_generalization: self.typecheck_until_generalization.clone(),
            already_typechecked: self.already_typechecked.clone(),
            generalize_after: self.generalize_after,
            session: self.session,
            blackboard_database: self.blackboard_database.clone(),
            definitions: self.definitions.clone(),
            resolved_method_calls: HashMap::new(),
        };
        let ret = f(&mut sub);
        self.merge(sub);
        ret
    }

    pub fn infer_function(&mut self, tctx: &mut TypCtx, fn_def: &ast::FunctionDefinition) -> Type {
        // runtime functions are assumed to be already typed with a signature
        // so there is not need to infer them. They also don't have bodies
        // so we don't want to go into them.
        let debug_name = fn_def.name.clone();
        println!("Infering function {}", fn_def.name);
        if fn_def.attributes.contains(&"runtime".to_owned()) {
            return tctx.type_of(&fn_def.name).unwrap();
        }
        let stack_position = self
            .call_stack
            .iter()
            .rposition(|n| *n == fn_def.name)
            // add 1 here because if function found itself immediately we need to
            // know this instead of returning 0. Basically it means:
            //
            // This is amount of functions in the recursive chain.
            // If there is any chain the amount is at least one.
            .map(|x| x + 1)
            .unwrap_or(0);
        // function is already in the stack so this is a (mutually) recursive call
        if stack_position > 0 {
            println!("Stack position is more than 1 so this is recursive call");
            // TODO: should it be stack_position or stack_position - 1?
            self.generalize_after = std::cmp::max(self.generalize_after, stack_position - 1);
            return tctx.type_of(&fn_def.name).unwrap();
        }
        self.call_stack.push(fn_def.name.clone());
        let global_scope = Scope::get_global(self.scope.clone());
        println!("Creating new inferer for new function {}", fn_def.name);
        let mut inferer = Inference {
            scope: global_scope.clone(),
            env: HashMap::new(),
            constraints: Vec::new(),
            id_provider: self.id_provider.clone(),
            ret_t: None,
            call_stack: self.call_stack.clone(),
            already_typechecked: self.already_typechecked.clone(),
            typecheck_until_generalization: self.typecheck_until_generalization.clone(),
            generalize_after: 0,
            session: self.session,
            blackboard_database: self.blackboard_database.clone(),
            definitions: self.definitions.clone(),
            resolved_method_calls: HashMap::new(),
        };
        println!("Generating constraints for {debug_name}");
        let mut prefex = PrefexScope::with_empty_scope();
        inferer.generate_constrains_for_function(tctx, global_scope.clone(), &mut prefex, fn_def);
        // if the function should be generalized immiediately it will be removed
        // right after this statement. If not it will be merged with our
        // chain for later generalization.
        inferer
            .typecheck_until_generalization
            .insert(fn_def.name.clone());
        // we need to generalize all functions in the mutualy recursive chain here.
        if inferer.generalize_after == 0 {
            println!("Generalization is 0 so we can generalize now for {debug_name}");
            // Only now we solve constraints after we gathered all information
            // after mutally recursive calls in the call stack.
            println!("Solving constraints");
            // TODO: We have this weird way of finding recursive groups of functions by
            // traversing the class stack. So we do solving within the inference
            // code instead of generating constraints for everything and just
            // passing it to the solver. We need to have forest and database
            // here to create a SlgSolver. Also we need to gather typeclasses
            // from the function definition, if any, to pass them to the
            // constraint solver so it knows its context.
            let mut solver = ConstraintSolver::new(
                tctx,
                self.id_provider.clone(),
                &inferer.blackboard_database,
                self.definitions.clone(),
            );
            let constraints = inferer.constraints.clone();
            solver.solve(constraints);
            let resolved_methods = solver.resolved_nodes;
            self.resolved_method_calls.extend(resolved_methods);
            println!("Constraints solved {debug_name}");
            let mut substitutions = solver.substitutions.into_iter().collect();
            fix_fresh::fix_types_after_inference(&mut substitutions, tctx, &mut inferer.env);
            // drain removes the functions from the chain so we won't try to
            // generalize them later
            println!("Constraints are solved");
            for name in inferer.typecheck_until_generalization.drain() {
                println!("Function {name} has been typechecked");
                generalize_types(tctx, &name, global_scope.clone(), &mut inferer.env);
                self.already_typechecked.insert(name);
            }
            // The constraints generated by this call have to be discarded as we already
            // solved them
            self.merge_without_constraints(inferer);
        } else {
            inferer.generalize_after -= 1;
            // we need to keep constaraints here as they will be necessary when
            // we want to solve mutally recursive function types
            self.merge(inferer);
        }
        println!("returning from function {debug_name}");
        self.call_stack.pop();
        dbg!(tctx.type_of(&fn_def.name).unwrap())
    }

    /// Entry point to typechecking.
    ///
    /// Creates an instance of type inferer and runs it on the program.
    /// Then returns itself with all the information gathered.
    pub fn infer_types(tctx: &mut TypCtx, ast: &ast::SourceFile, session: &'sess Session) -> Self {
        // create fresh variables for anonymous types
        let replace_to_infere = &mut ReplaceInferTypes::new(session.id_provider());
        tctx.fold_types(replace_to_infere);
        let global_scope = Rc::new(RefCell::new(Scope::from_type_ctx(tctx)));
        let (blackboard_database, definitions) = prepare_blackboard_databse(tctx);
        let mut inferer = Inference::new(
            global_scope.clone(),
            session,
            blackboard_database,
            definitions,
        );
        for item in &ast.items {
            match &item.kind {
                ast::TopLevelItemKind::FunctionDefinition(fn_def) => {
                    if inferer.already_typechecked.contains(&fn_def.name) {
                        continue;
                    }
                    let _ = inferer.infer_function(tctx, fn_def);
                }
                ast::TopLevelItemKind::ConstDefinition(def) => {
                    inferer.infer_const_def(tctx, global_scope.clone(), def);
                }
                ast::TopLevelItemKind::MethodsBlock(methods) => {
                    inferer.infer_methods_block(tctx, global_scope.clone(), methods);
                }
                _ => continue,
            }
        }
        inferer
    }

    fn infer_methods_block(
        &mut self,
        tctx: &mut TypCtx,
        global_scope: Rc<RefCell<Scope>>,
        methods_block: &MethodsBlock<FunctionDefinition>,
    ) {
        // TODO: Also do something about the constraints
        let mut prefex_scope = PrefexScope::with_empty_scope();
        let scope = global_scope.borrow();
        println!("Infering on ast type {:?}", &methods_block.typ);
        let receiver_t = self.ast_type_to_type(&scope, &mut prefex_scope, &methods_block.typ);
        println!("Invering method on type {:?}", receiver_t);
        let (receiver_blackboard_t, existentials) = ty_to_blackboard_type(
            tctx,
            &receiver_t,
            &self.blackboard_database,
            &mut Vec::new(),
        );
        let query = classy_blackboard::goal::Goal::Exists(
            existentials,
            Box::new(classy_blackboard::goal::Goal::Domain(
                classy_blackboard::goal::DomainGoal::MethodBlockExists {
                    on_type: receiver_blackboard_t,
                },
            )),
        );
        let mut forest = classy_blackboard::slg::Forest::new();
        let solver =
            classy_blackboard::slg::SlgSolver::new(&self.blackboard_database, &mut forest, query);
        println!("Solving for method block {:?}", methods_block);
        println!(
            "Receiver t: {:?}, existentials {}",
            receiver_t, existentials
        );
        let matching_blocks = solver.take(101).collect::<Vec<_>>();
        if matching_blocks.len() == 101 {
            panic!("Too many matching blocks, the limit is 100");
        }
        for classy_blackboard::slg::Answer { origin, .. } in matching_blocks {
            // TODO:
            // We need to get origin to retrieve the correct methods block and from that
            // retrieve all the methods.
            // Now we need to use subst to get the correct receiver type and correct
            // method types.
            // We know what it because if the receiver type of the method block is a Scheme
            // it has all of the type variables in the prefex.
            println!("GOT A MATHICNG BLOCK");
            println!("{:?}", origin);
            // TODO: Those substitutions don't work, for example if we have
            // Option(String) and Option(a)
            // then there is nothing to substitute but we want ot have
            // a -> String
            // so we need to do union now with the type of the found method
            // block on the left and the original receiver on the
            // right and see what the substitution should be
            let AnswerOrigin::FromRef(origin) = origin else {
                panic!("Expected AnswerOrigin::FromRef got {origin:?}")
            };
            let def_id = self.definitions[&origin];
            let methods_block = tctx
                .get_methods_block(def_id)
                .expect("could not find methods block");
            let _specialisation_tid = methods_block.specialisation;
            // TODO: Union specialisation_tid with the receiver_tid and get
            // substitutions so that TODO: We can substitute within
            // methods
        }
        todo!();
    }

    fn generate_constrains_for_function(
        &mut self,
        tctx: &mut TypCtx,
        fn_scope: Rc<RefCell<Scope>>,
        prefex_scope: &mut PrefexScope,
        ast::FunctionDefinition {
            name,
            parameters,
            body,
            ..
        }: &ast::FunctionDefinition,
    ) {
        let t = fn_scope
            .borrow()
            .type_of(name)
            .expect("Expected type of function to exist");
        let (args, ret, prefex) = match t {
            Type::Function { args, ret } => (args, ret, vec![]),
            Type::Scheme { typ, prefex } => match *typ {
                Type::Function { args, ret } => (args, ret, prefex),
                Type::Alias(for_type) => {
                    let for_type = tctx.definitions.get(&for_type).unwrap().clone();
                    match for_type {
                        Type::Function { args, ret } => (args, ret, prefex),
                        t => panic!("Expected function type got {t:?}"),
                    }
                }
                t => panic!("Expected function type got {t:?}"),
            },
            Type::App { typ, args } => {
                println!("INSTANTIATING {typ:?} with {args:?}");
                let instantiated = instance(tctx, args, *typ);
                match instantiated {
                    Type::Function { args, ret } => (args, ret, vec![]),
                    t => panic!("Expected function type - got {t:?}"),
                }
            }
            t => panic!("Expected function type - got {t:?}"),
        };
        let fn_actual_type = {
            let fn_scope = Scope::new_debruijn(fn_scope);
            self.in_scope(fn_scope.clone(), |scope| {
                scope.set_ret_t(Some(*(ret.clone())));
                for (param, typ) in parameters.iter().zip(&args) {
                    fn_scope.borrow_mut().add_variable(param, typ.clone());
                }
                prefex_scope.new_scope();
                prefex_scope.add_type_vars(&prefex);
                let ret = scope.infer_in_expr(body, prefex_scope, tctx);
                prefex_scope.pop_scope();
                ret
            })
        };
        self.constraints.push(Constraint::Eq(fn_actual_type, *ret));
    }

    fn infer_const_def(
        &mut self,
        tctx: &mut TypCtx,
        global_scope: Rc<RefCell<Scope>>,
        ast::ConstDefinition { name, init, .. }: &ast::ConstDefinition,
    ) {
        let t = global_scope.borrow().type_of(name).unwrap();
        let mut inferer = Inference {
            scope: global_scope.clone(),
            env: HashMap::new(),
            constraints: Vec::new(),
            ret_t: None,
            call_stack: self.call_stack.clone(),
            already_typechecked: self.already_typechecked.clone(),
            typecheck_until_generalization: self.typecheck_until_generalization.clone(),
            generalize_after: 0,
            id_provider: self.id_provider.clone(),
            session: self.session,
            blackboard_database: self.blackboard_database.clone(),
            definitions: self.definitions.clone(),
            resolved_method_calls: HashMap::new(),
        };
        let const_actual_type = {
            let const_scope = Scope::new(global_scope.clone());
            inferer.in_scope(const_scope.clone(), |scope| {
                scope.set_ret_t(Some(t.clone()));
                let mut prefex_scope = PrefexScope::with_empty_scope();
                scope.infer_in_expr(init, &mut prefex_scope, tctx)
            })
        };
        inferer
            .constraints
            .push(Constraint::Eq(const_actual_type, t));
        let mut solver = ConstraintSolver::new(
            tctx,
            self.id_provider.clone(),
            &inferer.blackboard_database,
            inferer.definitions.clone(),
        );
        let constraints = inferer.constraints.clone();
        solver.solve(constraints);
        let mut substitutions = solver.substitutions.into_iter().collect();
        fix_fresh::fix_types_after_inference(&mut substitutions, tctx, &mut inferer.env);
        generalize_types(tctx, name, global_scope.clone(), &mut inferer.env);
        // discard constraints as we alrady solved them
        self.merge_without_constraints(inferer);
    }

    pub fn merge(&mut self, other: Self) {
        self.constraints.extend(other.constraints.iter().cloned());
        self.merge_without_constraints(other);
    }

    pub fn merge_without_constraints(&mut self, other: Self) {
        self.env.extend(other.env);
        // not sure we need extend here but it is just for safety
        self.already_typechecked.extend(other.already_typechecked);
        self.generalize_after = std::cmp::max(self.generalize_after, other.generalize_after);
        self.typecheck_until_generalization
            .extend(other.typecheck_until_generalization);
        self.resolved_method_calls
            .extend(other.resolved_method_calls);
    }

    pub fn infer_in_expr(
        &mut self,
        expr: &ast::Expr,
        prefex_scope: &mut PrefexScope,
        tctx: &mut TypCtx,
    ) -> Type {
        let id = expr.id;
        match &expr.kind {
            ast::ExprKind::Unit => {
                self.env.insert(id, Type::Unit);
                Type::Unit
            }
            ast::ExprKind::Sequence(exprs) => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                let exprs = exprs
                    .iter()
                    .map(|expr| self.infer_in_expr(expr, prefex_scope, tctx))
                    .collect::<Vec<_>>();
                let last_t = exprs.last().unwrap().clone();
                self.constraints.push(Constraint::Eq(ret_t.clone(), last_t));
                ret_t
            }
            ast::ExprKind::Assignment { lval, rval } => {
                let l_t = self.infer_in_expr(lval, prefex_scope, tctx);
                let r_t = self.infer_in_expr(rval, prefex_scope, tctx);
                self.constraints.push(Constraint::Eq(l_t, r_t));
                self.env.insert(id, Type::Unit);
                Type::Unit
            }
            ast::ExprKind::IntConst(_) => {
                self.env.insert(id, Type::Int);
                Type::Int
            }
            ast::ExprKind::StringConst(_) => {
                self.env.insert(id, Type::String);
                Type::String
            }
            ast::ExprKind::FloatConst(_) => {
                self.env.insert(id, Type::Float);
                Type::Float
            }
            ast::ExprKind::BoolConst(_) => {
                self.env.insert(id, Type::Bool);
                Type::Bool
            }
            ast::ExprKind::Name(ast::Name::Unresolved { path, identifier }) if path.is_empty() => {
                let name = identifier;
                let mut typ = {
                    println!("Checking name {name}");
                    let scope = self.scope.borrow();

                    scope.type_of(name).unwrap_or_else(|| {
                        println!("Expression checked:\n{expr:?}");
                        panic!("Unknown variable {name}")
                    })
                };
                if self.scope.borrow().is_global(name) && requires_typechecking(typ.clone()) {
                    println!("Name {name} is global and requires typechecking. {typ:?}");
                    if self.already_typechecked.contains(name) {
                        panic!(
                            "Function {name} has been already typechecked but still requires \
                             typechecking {typ:?}. Possibly a recursive definition."
                        )
                    }
                    let fn_def = tctx
                        .nodes
                        .iter()
                        .find_map(|(_, def)| match def {
                            ast::TopLevelItemKind::FunctionDefinition(fn_def)
                                if fn_def.name == *name =>
                            {
                                Some(fn_def)
                            }
                            _ => None,
                        })
                        .unwrap()
                        .clone();
                    typ = self.infer_function(tctx, &fn_def);
                }
                self.env.insert(id, typ.clone());
                typ
            }
            ast::ExprKind::FunctionCall { func, args, kwargs } => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                let f_t = self.infer_in_expr(func, prefex_scope, tctx);
                let args_t = args
                    .iter()
                    .map(|arg| self.infer_in_expr(arg, prefex_scope, tctx))
                    .collect::<Vec<_>>();
                if !kwargs.is_empty() {
                    todo!("kwargs not implemented yet")
                }
                let f_t = match f_t {
                    Type::Scheme { prefex, typ } => {
                        let args = prefex.iter().map(|_| self.fresh_type()).collect();
                        Type::App {
                            typ: Box::new(Type::Scheme { prefex, typ }),
                            args,
                        }
                    }
                    t => t,
                };
                self.constraints.push(Constraint::Eq(
                    f_t,
                    Type::Function {
                        args: args_t,
                        ret: Box::new(ret_t.clone()),
                    },
                ));
                ret_t
            }
            ast::ExprKind::Access { val, field } => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                let val_t = self.infer_in_expr(val, prefex_scope, tctx);
                self.constraints.push(Constraint::HasField {
                    t: val_t,
                    field: field.clone(),
                    of_type: ret_t.clone(),
                });
                ret_t
            }
            ast::ExprKind::Tuple(args) => {
                let t_t = self.fresh_type();
                self.env.insert(id, t_t.clone());
                let args_t = args
                    .iter()
                    .map(|arg| self.infer_in_expr(arg, prefex_scope, tctx))
                    .collect::<Vec<_>>();
                self.constraints
                    .push(Constraint::Eq(t_t.clone(), Type::Tuple(args_t)));
                t_t
            }
            ast::ExprKind::Lambda { parameters, body } => {
                let lambda_t = self.fresh_type();
                self.env.insert(id, lambda_t.clone());
                let args_t = parameters
                    .iter()
                    .map(|ast::TypedIdentifier { typ, .. }| match &typ {
                        ast::Typ::ToInfere => self.fresh_type(),
                        typ => {
                            self.ast_type_to_type(&self.scope.clone().borrow(), prefex_scope, typ)
                        }
                    })
                    .collect::<Vec<_>>();
                let lambda_scope = Scope::new_debruijn(self.scope.clone());
                for (par, typ) in parameters.iter().zip(args_t.iter()) {
                    lambda_scope
                        .borrow_mut()
                        .add_variable(&par.name, typ.clone());
                }
                let body_t = self.in_scope(lambda_scope, |scope| {
                    scope.infer_in_expr(body, prefex_scope, tctx)
                });
                self.constraints.push(Constraint::Eq(
                    lambda_t.clone(),
                    Type::Function {
                        args: args_t,
                        ret: Box::new(body_t),
                    },
                ));
                lambda_t
            }
            ast::ExprKind::TypedExpr { expr, typ } => {
                let expr_t = self.infer_in_expr(expr, prefex_scope, tctx);
                let typ_t = self.ast_type_to_type(&self.scope.clone().borrow(), prefex_scope, typ);
                self.constraints.push(Constraint::Eq(expr_t, typ_t.clone()));
                self.env.insert(id, typ_t.clone());
                typ_t
            }
            ast::ExprKind::StructLiteral {
                strct: ast::Name::Unresolved { path, identifier },
                values,
            } if path.is_empty() => {
                let ret_t = self.fresh_type();
                self.env.insert(id, ret_t.clone());
                let name = identifier.clone();
                let strct_t = self.scope.borrow().lookup_type(&name).unwrap();
                let strct_t = self.instance_if_possible(&strct_t);
                let Some(fields) = self.extract_fields(strct_t.clone(), ret_t.clone(), true) else {
                    panic!("expected truct type, got {strct_t:?}")
                };
                let args_t = values
                    .iter()
                    .map(|(name, expr)| {
                        let expr_t = self.infer_in_expr(expr, prefex_scope, tctx);
                        (name, expr_t)
                    })
                    .collect::<HashMap<_, _>>();
                for (f_name, typ) in &fields {
                    let expr_t = args_t.get(f_name).unwrap();
                    self.constraints
                        .push(Constraint::Eq(expr_t.clone(), typ.clone()));
                }
                ret_t
            }
            ast::ExprKind::While { cond, body } => {
                self.env.insert(id, Type::Unit);
                let cond_t = self.infer_in_expr(cond, prefex_scope, tctx);
                self.constraints.push(Constraint::Eq(cond_t, Type::Bool));
                let _ = self.in_scope(Scope::new(self.scope.clone()), |this| {
                    this.infer_in_expr(body, prefex_scope, tctx)
                });
                Type::Unit
            }
            ast::ExprKind::Return(expr) => {
                self.env.insert(id, Type::Divergent);
                let expr_t = self.infer_in_expr(expr, prefex_scope, tctx);
                self.constraints
                    .push(Constraint::Eq(expr_t, self.ret_t.clone().unwrap()));
                Type::Divergent
            }
            ast::ExprKind::If {
                cond,
                body,
                else_body,
            } => {
                let if_t = self.fresh_type();
                self.env.insert(id, if_t.clone());
                let cond_t = self.infer_in_expr(cond, prefex_scope, tctx);
                self.constraints.push(Constraint::Eq(cond_t, Type::Bool));
                let body_t = self.in_scope(Scope::new(self.scope.clone()), |this| {
                    this.infer_in_expr(body, prefex_scope, tctx)
                });
                match else_body {
                    None => {
                        self.constraints
                            .push(Constraint::Eq(if_t.clone(), Type::Unit));
                    }
                    Some(else_body) => {
                        let else_body_t = self.in_scope(Scope::new(self.scope.clone()), |this| {
                            this.infer_in_expr(else_body, prefex_scope, tctx)
                        });
                        self.constraints
                            .push(Constraint::Eq(body_t.clone(), else_body_t));
                        self.constraints.push(Constraint::Eq(if_t.clone(), body_t));
                    }
                }
                if_t
            }
            ast::ExprKind::Let { name, typ, init } => {
                self.env.insert(id, Type::Unit);
                let init_t = self.infer_in_expr(init, prefex_scope, tctx);
                let let_t = match typ {
                    ast::Typ::ToInfere => init_t.clone(),
                    t => self.ast_type_to_type(&self.scope.clone().borrow(), prefex_scope, t),
                };
                self.constraints.push(Constraint::Eq(init_t, let_t.clone()));
                self.scope.borrow_mut().add_variable(name, let_t);
                Type::Unit
            }
            ast::ExprKind::ArrayLiteral { typ, size, init } => {
                let array_t = self.fresh_type();
                self.env.insert(id, array_t.clone());
                let size_t = self.infer_in_expr(size, prefex_scope, tctx);
                self.constraints.push(Constraint::Eq(size_t, Type::Int));
                let array_inner_t = match typ {
                    ast::Typ::ToInfere => self.fresh_type(),
                    t => self.ast_type_to_type(&self.scope.clone().borrow(), prefex_scope, t),
                };
                self.constraints.push(Constraint::Eq(
                    array_t.clone(),
                    Type::Array(Box::new(array_inner_t.clone())),
                ));
                let init_types: Vec<_> = init
                    .iter()
                    .map(|expr| self.infer_in_expr(expr, prefex_scope, tctx))
                    .collect();
                for t in init_types {
                    self.constraints
                        .push(Constraint::Eq(t, array_inner_t.clone()));
                }
                array_t
            }
            ast::ExprKind::AnonType { .. } => {
                panic!("There should be no anon types when type checking")
            }
            ast::ExprKind::IndexAccess { lhs, index } => {
                let inner_t = self.fresh_type();
                self.env.insert(id, inner_t.clone());
                let lhs_t = self.infer_in_expr(lhs, prefex_scope, tctx);
                let index_t = self.infer_in_expr(index, prefex_scope, tctx);
                self.constraints.push(Constraint::Eq(index_t, Type::Int));
                self.constraints.push(Constraint::Eq(
                    lhs_t,
                    Type::Array(Box::new(inner_t.clone())),
                ));
                inner_t
            }
            ast::ExprKind::Match { expr, cases } => {
                let ret = self.fresh_type();
                self.env.insert(id, ret.clone());
                let expr_t = self.infer_in_expr(expr, prefex_scope, tctx);
                for (pattern, body, guard) in cases {
                    self.in_scope(Scope::new(self.scope.clone()), |this| {
                        // i think we need to reverse constraints generated by patterns
                        let pattern_t = this.infer_in_pattern(pattern);
                        this.constraints
                            .push(Constraint::Eq(expr_t.clone(), pattern_t));
                        // Pattern constrains have to be reversed to be solved properly.
                        // The same applies to normal constraints.
                        this.constraints.reverse();
                        if let Some(cond) = guard {
                            let cond_t = this.infer_in_expr(cond, prefex_scope, tctx);
                            this.constraints.push(Constraint::Eq(cond_t, Type::Bool));
                        }
                        let body_t = this.infer_in_expr(body, prefex_scope, tctx);
                        this.constraints.push(Constraint::Eq(body_t, ret.clone()));
                        Type::Unit
                    });
                }
                ret
            }
            ast::ExprKind::AdtTupleConstructor {
                typ: ast::Name::Unresolved { path, identifier },
                constructor,
                args,
            } if path.is_empty() => {
                let ret = self.fresh_type();
                self.env.insert(id, ret.clone());
                // TODO: This is a bit of a problem because creating any struct
                // or case actually creates an application.
                // So probably befor we create this type we should check if its a scheme
                // and if it is we should instance it. Thats the same for any other case
                // even structure creation.
                let typ = identifier;
                let t = self.scope.borrow().lookup_type(typ).unwrap();
                let t = self.instance_if_possible(&t);
                self.constraints.push(Constraint::Eq(ret.clone(), t));
                let args_t = args
                    .iter()
                    .map(|arg| self.infer_in_expr(arg, prefex_scope, tctx))
                    .collect::<Vec<_>>();
                self.constraints.push(Constraint::HasCase {
                    t: ret.clone(),
                    case: constructor.clone(),
                    of_type: Type::Tuple(args_t),
                });
                ret
            }
            ast::ExprKind::AdtStructConstructor {
                typ: ast::Name::Unresolved { path, identifier },
                constructor,
                fields,
            } if path.is_empty() => {
                let ret = self.fresh_type();
                self.env.insert(id, ret.clone());

                let typ = identifier;
                let t = self.scope.borrow().lookup_type(typ).unwrap();
                let t = self.instance_if_possible(&t);
                self.constraints.push(Constraint::Eq(ret.clone(), t));
                let fields_t = fields
                    .iter()
                    .map(|(name, expr)| {
                        let expr_t = self.infer_in_expr(expr, prefex_scope, tctx);
                        (name.clone(), expr_t)
                    })
                    .collect::<Vec<_>>();
                self.constraints.push(Constraint::HasCase {
                    t: ret.clone(),
                    case: constructor.clone(),
                    of_type: Type::Struct {
                        def: 0,
                        fields: fields_t.into_iter().collect(),
                    },
                });
                ret
            }
            ast::ExprKind::AdtUnitConstructor {
                typ: ast::Name::Unresolved { path, identifier },
                constructor,
            } if path.is_empty() => {
                let ret = self.fresh_type();
                self.env.insert(id, ret.clone());

                let typ = identifier;
                let t = self.scope.borrow().lookup_type(typ).unwrap();
                let t = self.instance_if_possible(&t);
                self.constraints.push(Constraint::Eq(ret.clone(), t));
                self.constraints.push(Constraint::HasCase {
                    t: ret.clone(),
                    case: constructor.clone(),
                    of_type: Type::Unit,
                });
                ret
            }
            ast::ExprKind::MethodCall {
                receiver,
                method,
                args,
                kwargs,
                ..
            } => {
                assert!(
                    kwargs.is_empty(),
                    "Keyword arguments are not implemented yet"
                );
                let ret = self.fresh_type();
                self.env.insert(id, ret.clone());
                let receiver_t = self.infer_in_expr(receiver, prefex_scope, tctx);
                let args_t = args
                    .iter()
                    .map(|arg| self.infer_in_expr(arg, prefex_scope, tctx))
                    .collect::<Vec<_>>();
                self.constraints.push(Constraint::HasMethod {
                    nodeid: id,
                    receiver: receiver_t,
                    method: method.clone(),
                    args: args_t,
                    ret: ret.clone(),
                });
                ret
            }
            ExprKind::LetRec { definitions } => {
                self.infer_let_rec(definitions, prefex_scope, tctx);
                Type::Unit
            }
            _ => panic!("panic"),
        }
    }

    fn infer_in_pattern(&mut self, pattern: &ast::Pattern) -> Type {
        let id = pattern.id;
        match &pattern.kind {
            ast::PatternKind::Var(n) => {
                let typ = self.fresh_type();
                self.env.insert(id, typ.clone());
                if n.starts_with(char::is_uppercase) {
                    self.constraints.push(Constraint::HasCase {
                        t: typ.clone(),
                        case: n.clone(),
                        of_type: Type::Unit,
                    })
                } else {
                    self.scope.borrow_mut().add_variable(n, typ.clone());
                }
                typ
            }
            ast::PatternKind::TypeSpecifier(ast::Name::Unresolved { path, identifier }, case)
                if path.is_empty() =>
            {
                let tname = identifier;
                let typ = self
                    .scope
                    .borrow()
                    .lookup_type(tname)
                    .expect("unknown type");
                let typ = self.instance_if_possible(&typ);
                let case_t = self.infer_in_pattern(case);
                self.constraints.push(Constraint::Eq(case_t, typ.clone()));

                self.env.insert(id, typ.clone());
                typ
            }
            ast::PatternKind::Tuple(inner) => {
                let typ = self.fresh_type();

                self.env.insert(id, typ.clone());
                let inner_types = inner
                    .iter()
                    .map(|p| self.infer_in_pattern(p))
                    .collect::<Vec<_>>();
                self.constraints
                    .push(Constraint::Eq(typ.clone(), Type::Tuple(inner_types)));
                typ
            }
            ast::PatternKind::Struct {
                strct: ast::Name::Unresolved { path, identifier },
                fields,
            } if path.is_empty() => {
                let typ = self.fresh_type();
                let strct = identifier;
                self.env.insert(id, typ.clone());
                let inner_types = fields
                    .iter()
                    .map(|(n, p)| {
                        let typ = self.infer_in_pattern(p);
                        (n.clone(), typ)
                    })
                    .collect::<Vec<_>>();
                self.constraints.push(Constraint::HasCase {
                    t: typ.clone(),
                    case: strct.clone(),
                    of_type: Type::Struct {
                        def: 0,
                        fields: inner_types.into_iter().collect(),
                    },
                });
                typ
            }
            ast::PatternKind::AnonStruct { fields } => {
                let typ = self.fresh_type();

                self.env.insert(id, typ.clone());
                let inner_types = fields
                    .iter()
                    .map(|(n, p)| {
                        let typ = self.infer_in_pattern(p);
                        (n.clone(), typ)
                    })
                    .collect::<Vec<_>>();
                for (name, t) in &inner_types {
                    self.constraints.push(Constraint::HasField {
                        t: typ.clone(),
                        field: name.clone(),
                        of_type: t.clone(),
                    })
                }
                typ
            }
            ast::PatternKind::TupleStruct {
                strct: ast::Name::Unresolved { path, identifier },
                fields,
            } if path.is_empty() => {
                let typ = self.fresh_type();

                let strct = identifier;
                self.env.insert(id, typ.clone());
                let inner_types = fields
                    .iter()
                    .map(|p| self.infer_in_pattern(p))
                    .collect::<Vec<_>>();
                self.constraints.push(Constraint::HasCase {
                    t: typ.clone(),
                    case: strct.clone(),
                    of_type: Type::Tuple(inner_types),
                });
                typ
            }
            ast::PatternKind::Array(patterns) => {
                let typ = self.fresh_type();
                self.env.insert(id, typ.clone());
                let inner_types = patterns
                    .iter()
                    .map(|p| self.infer_in_pattern(p))
                    .collect::<Vec<_>>();
                for t in &inner_types {
                    self.constraints
                        .push(Constraint::Eq(t.clone(), inner_types[0].clone()));
                }
                self.constraints.push(Constraint::Eq(
                    typ.clone(),
                    Type::Array(Box::new(inner_types[0].clone())),
                ));
                typ
            }
            ast::PatternKind::Rest(_) => {
                todo!("Rest patterns are kinds hard as they mess up tuple types a bit")
            }
            ast::PatternKind::Wildcard => self.fresh_type(),
            ast::PatternKind::Unit => {
                self.env.insert(id, Type::Unit);
                Type::Unit
            }
            ast::PatternKind::String(_) => {
                self.env.insert(id, Type::String);
                Type::String
            }
            ast::PatternKind::Int(_) => {
                self.env.insert(id, Type::Int);
                Type::Int
            }

            ast::PatternKind::Bool(_) => {
                self.env.insert(id, Type::Bool);
                Type::Bool
            }
            p => panic!("Pattern {p:?} is not supported yet"),
        }
    }

    fn infer_let_rec(
        &mut self,
        definitions: &[ast::FunctionDefinition],
        prefex_scope: &mut PrefexScope,
        tctx: &mut TypCtx,
    ) {
        let fresh_types_below = self.id_provider.last();
        // Add definitions with their types
        println!("INFERING LET REC");
        println!("DEFINITIONS {definitions:?}");
        for def in definitions {
            println!("DEFINITION {}", def.name);
            let typ = self.ast_type_to_type(&self.scope.clone().borrow(), prefex_scope, &def.typ);
            println!("Rplaced to infer {typ:?}");
            println!("INSERTING TYPE {typ:?} for {}", def.name);
            self.scope.borrow_mut().add_variable(&def.name, typ)
        }

        let mut inferer = Inference {
            scope: self.scope.clone(),
            env: HashMap::new(),
            constraints: Vec::new(),
            id_provider: self.id_provider.clone(),
            ret_t: None,
            call_stack: self.call_stack.clone(),
            already_typechecked: self.already_typechecked.clone(),
            typecheck_until_generalization: self.typecheck_until_generalization.clone(),
            generalize_after: 0,
            session: self.session,
            blackboard_database: self.blackboard_database.clone(),
            definitions: self.definitions.clone(),
            resolved_method_calls: HashMap::new(),
        };
        for def in definitions {
            inferer.generate_constrains_for_function(tctx, self.scope.clone(), prefex_scope, def);
        }
        for cons in &inferer.constraints {
            println!("Constraint {cons:?}");
        }
        /*
           TODO: During this solving constraint solver looks up name
           for type of "arg" which is Generic(0, 0), it should be generic(1, 0)


        So what I think happens is that when the solver solves this and the function refers to
        the argument of type t1 which later is generalized to generic type of the outer
        function the shifting does not work. So the local function is infered to be generic
        when in fact it is not.
        */
        let mut solver = ConstraintSolver::new(
            tctx,
            self.id_provider.clone(),
            &inferer.blackboard_database,
            inferer.definitions.clone(),
        );
        let constraints = inferer.constraints.clone();
        solver.solve(constraints);
        let mut substitutions = solver.substitutions.into_iter().collect();
        fix_fresh::fix_types_after_inference(&mut substitutions, tctx, &mut inferer.env);
        println!("Constraints are solved");
        let mut replacer = FreshTypeReplacer {
            substitutions: substitutions.clone(),
        };
        for def in definitions {
            let t = self.scope.borrow().type_of(&def.name).unwrap();
            let new_t = replacer.fold_type(t).unwrap();
            self.scope.borrow_mut().add_variable(&def.name, new_t);
            generalize_type_above(
                fresh_types_below,
                &def.name,
                self.scope.clone(),
                &mut inferer.env,
            );
            println!(
                "After generalisation type of {} is {:?}",
                &def.name,
                self.scope.borrow().type_of(&def.name).unwrap()
            );
        }
        // The constraints generated by this call have to be discarded as we already
        // solved them
        self.merge_without_constraints(inferer);
    }

    fn fresh_type(&mut self) -> Type {
        Type::Fresh(self.id_provider.next())
    }

    fn instance_if_possible(&mut self, t: &Type) -> Type {
        match t {
            Type::Scheme { prefex, typ } => {
                if prefex.is_empty() {
                    return *typ.clone();
                }
                let args = prefex.iter().map(|_| self.fresh_type()).collect();
                Type::App {
                    typ: Box::new(Type::Scheme {
                        prefex: prefex.clone(),
                        typ: typ.clone(),
                    }),
                    args,
                }
            }
            Type::Alias(for_type) => {
                let resolved = self.scope.borrow().resolve_alias(*for_type).unwrap();
                self.instance_if_possible(&resolved)
            }
            t => t.clone(),
        }
    }

    fn ast_type_to_type(
        &mut self,
        scope: &Scope,
        prefex_scope: &mut PrefexScope,
        typ: &ast::Typ,
    ) -> Type {
        // convert ast::Typ to Type
        match typ {
            ast::Typ::Unit => Type::Unit,
            ast::Typ::Name(ast::Name::Unresolved { path, identifier }) if path.is_empty() => {
                match prefex_scope.get(identifier) {
                    Some(i) => {
                        let pos = prefex_scope.position(identifier).unwrap();
                        Type::Generic(DeBruijn(pos as isize), *i)
                    }
                    // TODO: #problem This might lookup a function shich name
                    // we do not know. So we should solve it here it the type
                    // of this function is unknown.
                    None => scope.lookup_type(identifier).expect("unknown type"),
                }
            }
            ast::Typ::Application { callee, args } => {
                let callee = Box::new(self.ast_type_to_type(scope, prefex_scope, callee));
                let args = args
                    .iter()
                    .map(|typ| self.ast_type_to_type(scope, prefex_scope, typ))
                    .collect::<Vec<_>>();
                Type::App { typ: callee, args }
            }
            ast::Typ::Poly {
                free_variables,
                bounds: _bounds,
                typ,
            } => {
                if free_variables.is_empty() {
                    return self.ast_type_to_type(scope, prefex_scope, typ);
                }
                prefex_scope.with_scope(|prefex_scope| {
                    prefex_scope.add_type_vars(free_variables);
                    Type::Scheme {
                        prefex: free_variables.clone(),
                        typ: Box::new(self.ast_type_to_type(scope, prefex_scope, typ)),
                    }
                })
            }
            ast::Typ::Array(t) => {
                let t = Box::new(self.ast_type_to_type(scope, prefex_scope, t));
                Type::Array(t)
            }
            ast::Typ::Function { args, ret } => {
                let args = args
                    .iter()
                    .map(|typ| self.ast_type_to_type(scope, prefex_scope, typ))
                    .collect::<Vec<_>>();
                let ret = Box::new(self.ast_type_to_type(scope, prefex_scope, ret));
                Type::Function { args, ret }
            }
            ast::Typ::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|typ| self.ast_type_to_type(scope, prefex_scope, typ))
                    .collect::<Vec<_>>();
                Type::Tuple(types)
            }
            ast::Typ::ToInfere => self.fresh_type(),
            _ => panic!("panic"),
        }
    }

    fn extract_fields(
        &mut self,
        t: Type,
        ret_t: Type,
        push_constraint: bool,
    ) -> Option<Vec<(String, Type)>> {
        match t {
            Type::Struct { def, fields } => {
                if push_constraint {
                    self.constraints.push(Constraint::Eq(
                        ret_t.clone(),
                        Type::Struct {
                            def,
                            fields: fields.clone(),
                        },
                    ));
                }
                Some(fields)
            }
            Type::Scheme { prefex, typ } => {
                if push_constraint {
                    let args = prefex.iter().map(|_| self.fresh_type()).collect();
                    self.constraints.push(Constraint::Eq(
                        ret_t.clone(),
                        Type::App {
                            typ: Box::new(Type::Scheme {
                                prefex,
                                typ: typ.clone(),
                            }),
                            args,
                        },
                    ));
                }
                self.extract_fields(*typ, ret_t, false)
            }
            Type::Alias(for_type) => {
                let resolved = self.scope.borrow().resolve_alias(for_type)?;
                self.extract_fields(resolved, ret_t, push_constraint)
            }
            Type::App { typ, args } => {
                if push_constraint {
                    self.constraints.push(Constraint::Eq(
                        ret_t.clone(),
                        Type::App {
                            typ: typ.clone(),
                            args,
                        },
                    ));
                }
                self.extract_fields(*typ, ret_t, false)
            }
            _ => None,
        }
    }
}

struct ReplaceInferTypes {
    id_provider: SharedIdProvider,
}

impl ReplaceInferTypes {
    fn new(id_provider: SharedIdProvider) -> Self {
        Self { id_provider }
    }
}

impl TypeFolder for ReplaceInferTypes {
    type Error = ();
    fn fold_to_infere(&mut self) -> Result<Type, ()> {
        Ok(Type::Fresh(self.id_provider.next()))
    }
}

fn remove_aliases_in_env(env: &mut HashMap<usize, Type>, tctx: &TypCtx) {
    for typ in env.values_mut() {
        if let Type::Alias(for_type) = typ {
            let resolved = tctx.resolve_alias(*for_type);
            *typ = resolved;
        }
    }
}

pub fn requires_typechecking(t: Type) -> bool {
    let mut r = RequiresTypeChecking {
        requires_typechecking: false,
    };
    r.fold_type(t).unwrap();
    r.requires_typechecking
}

struct RequiresTypeChecking {
    requires_typechecking: bool,
}

impl TypeFolder for RequiresTypeChecking {
    type Error = ();
    fn fold_fresh(&mut self, id: usize) -> Result<Type, ()> {
        self.requires_typechecking = true;
        Ok(Type::Fresh(id))
    }
}

fn prepare_blackboard_databse(tctx: &TypCtx) -> (BlackboardDatabase, HashMap<GenericRef, DefId>) {
    let mut database = BlackboardDatabase::new();
    // Add basic types
    database.add_type_impl(database::TypeImpl {
        name: "String".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });

    database.add_type_impl(database::TypeImpl {
        name: "String".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    database.add_type_impl(database::TypeImpl {
        name: "Int".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    database.add_type_impl(database::TypeImpl {
        name: "UInt".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    database.add_type_impl(database::TypeImpl {
        name: "Bool".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    database.add_type_impl(database::TypeImpl {
        name: "Byte".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    database.add_type_impl(database::TypeImpl {
        name: "Unit".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    database.add_type_impl(database::TypeImpl {
        name: "Float".to_owned(),
        type_params: vec![],
        constraints: vec![],
        fields: vec![],
    });
    let mut defnitions = HashMap::new();
    // Add all structs and algebraic data types
    for ty in tctx.definitions.values() {
        let type_impl = match ty {
            Type::Struct { def, .. } | Type::ADT { def, .. } => {
                let name = tctx.name_by_def_id(*def);
                database::TypeImpl {
                    name,
                    type_params: vec![],
                    constraints: vec![],
                    fields: vec![],
                }
            }
            Type::Scheme {
                prefex,
                typ: box Type::Struct { def, .. } | box Type::ADT { def, .. },
            } => {
                let name = tctx.name_by_def_id(*def);
                let type_params = prefex.clone();
                database::TypeImpl {
                    name,
                    type_params,
                    constraints: vec![],
                    fields: vec![],
                }
            }
            Type::Alias(_)
            | Type::Generic(_, _)
            | Type::Fresh(_)
            | Type::ToInfere
            | Type::Divergent => {
                panic!("Unexpected type")
            }
            _ => continue,
        };
        database.add_type_impl(type_impl);
    }

    for method_sets in tctx.methods.values() {
        for MethodSet {
            specialisation,
            methods,
            def_id,
        } in method_sets
        {
            let ty = tctx.definitions.get(specialisation).unwrap();
            let methods = methods
                .iter()
                .map(|(name, tid)| (name.clone(), tctx.definitions.get(tid).unwrap().clone()))
                .collect::<Vec<_>>();
            let mut seen_generics = Vec::new();
            let (on_type, _) = ty_to_blackboard_type(tctx, ty, &database, &mut seen_generics);
            if let Type::Scheme { prefex, .. } = ty {
                seen_generics.push(Vec::new());
                seen_generics[0].extend(prefex.iter().map(|_| false));
            }
            println!("SEEN GENERICS {seen_generics:?}");
            let gref = database.add_method_block(database::MethodsBlock {
                // TODO: Support named method blocks
                name: None,
                methods: methods
                    .into_iter()
                    .map(|(name, ty)| database::Definition {
                        type_params: match &ty {
                            Type::Scheme { prefex, .. } => prefex.clone(),
                            _ => vec![],
                        },
                        name,
                        ty: ty_to_blackboard_type(tctx, &ty, &database, &mut seen_generics).0,
                    })
                    .collect(),
                type_params: match ty {
                    Type::Scheme { prefex, .. } => prefex.clone(),
                    _ => vec![],
                },
                constraints: vec![],
                on_type,
            });
            defnitions.insert(GenericRef::MethodBlock(gref), *def_id);
        }
    }
    database.lower_to_clauses();
    (database, defnitions)
}

pub type ExistentialCount = usize;

// TODO: This always returns 0, we would need to have a scope for the generics
// So that we don't count the same generic twice but yeah, the generics need to
// be accounted
pub fn ty_to_blackboard_type(
    tctx: &TypCtx,
    ty: &Type,
    database: &classy_blackboard::database::Database,
    seen_generics: &mut Vec<Vec<bool>>,
) -> (classy_blackboard::ty::Ty, ExistentialCount) {
    println!("TY TO BLACKBOARD TYPE {ty:?}");
    match ty {
        Type::Int => (
            classy_blackboard::ty::Ty::Ref(database.typeref_from_name("Int").unwrap()),
            0,
        ),
        Type::UInt => (
            classy_blackboard::ty::Ty::Ref(database.typeref_from_name("UInt").unwrap()),
            0,
        ),
        Type::Bool => (
            classy_blackboard::ty::Ty::Ref(database.typeref_from_name("Bool").unwrap()),
            0,
        ),
        Type::String => (
            classy_blackboard::ty::Ty::Ref(database.typeref_from_name("String").unwrap()),
            0,
        ),
        Type::Float => (
            classy_blackboard::ty::Ty::Ref(database.typeref_from_name("Float").unwrap()),
            0,
        ),
        Type::Unit => (
            classy_blackboard::ty::Ty::Ref(database.typeref_from_name("Unit").unwrap()),
            0,
        ),
        Type::Byte => (
            classy_blackboard::ty::Ty::Ref(database.typeref_from_name("Byte").unwrap()),
            0,
        ),
        Type::Struct { def, .. } | Type::ADT { def, .. } => (
            classy_blackboard::ty::Ty::Ref(
                database
                    .typeref_from_name(&tctx.name_by_def_id(*def))
                    .unwrap(),
            ),
            0,
        ),
        Type::Function { args, ret } => {
            let args_and_count = args
                .iter()
                .map(|t| ty_to_blackboard_type(tctx, t, database, seen_generics))
                .collect::<Vec<_>>();
            let existentials = args_and_count.iter().fold(0, |acc, (_, c)| acc + c);
            let args = args_and_count.into_iter().map(|(t, _)| t).collect();
            let (ret_t, ret_count) = ty_to_blackboard_type(tctx, ret, database, seen_generics);
            (
                classy_blackboard::ty::Ty::Fn(args, Box::new(ret_t)),
                existentials + ret_count,
            )
        }
        Type::Tuple(args) => {
            let args_and_count = args
                .iter()
                .map(|t| ty_to_blackboard_type(tctx, t, database, seen_generics))
                .collect::<Vec<_>>();
            let existentials = args_and_count.iter().fold(0, |acc, (_, c)| acc + c);
            let args = args_and_count.into_iter().map(|(t, _)| t).collect();
            (classy_blackboard::ty::Ty::Tuple(args), existentials)
        }
        Type::Array(inner) => {
            let (inner, count) = ty_to_blackboard_type(tctx, inner, database, seen_generics);
            (classy_blackboard::ty::Ty::Array(Box::new(inner)), count)
        }
        Type::Scheme { typ, .. } => {
            seen_generics.push(Vec::new());
            let res = ty_to_blackboard_type(tctx, typ, database, seen_generics);
            seen_generics.pop();
            res
        }
        Type::App { typ, args } => {
            let (typ, _) = ty_to_blackboard_type(tctx, typ, database, seen_generics);
            let args_and_count = args
                .iter()
                .map(|t| ty_to_blackboard_type(tctx, t, database, seen_generics))
                .collect::<Vec<_>>();
            let existentials = args_and_count.iter().fold(0, |acc, (_, c)| acc + c);
            let args = args_and_count.into_iter().map(|(t, _)| t).collect();
            (
                classy_blackboard::ty::Ty::App(Box::new(typ), args),
                existentials,
            )
        }
        Type::Generic(shift, index) => {
            let len = seen_generics.len();
            println!("Seen generics {seen_generics:?}");
            println!("Generic {shift:?} {index:?}");
            let scope = &mut seen_generics[len - shift.0 as usize - 1];
            if scope.len() <= *index {
                scope.extend(iter::repeat(false).take(*index - scope.len() + 1))
            }
            let account_generic = if scope[*index] {
                0
            } else {
                scope[*index] = true;
                1
            };
            (
                classy_blackboard::ty::Ty::Generic {
                    scopes: shift.0 as usize,
                    index: *index,
                },
                account_generic,
            )
        }
        Type::Alias(id) => {
            let resolved = tctx.resolve_alias(*id);
            ty_to_blackboard_type(tctx, &resolved, database, seen_generics)
        }
        t => panic!("Unsupported type {t:?}"),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast_passes::{self},
        session::Session,
        typecheck,
    };
    use classy_syntax::{lexer::Lexer, parser::Parser};

    fn run_typechecker(source: &str) {
        let lex = Lexer::new(source);
        let mut parser = Parser::new(lex);
        let res = parser.parse().unwrap();
        let sess = Session::new("test");
        let res = ast_passes::run_after_parsing_passes(res, &sess);
        let mut tctx = typecheck::prepare_for_typechecking(&res);
        let res = ast_passes::run_before_typechecking_passes(&tctx, res, &sess);
        typecheck::inference::run(&mut tctx, &res, &sess);
        println!("Final ast {res:#?}");
    }

    #[test]
    fn function_returning_literal_typechecks() {
        let source = r#"
            main: () -> Int
            main = 1
        "#;
        run_typechecker(source);
    }

    #[test]
    #[should_panic]
    fn function_returning_wrong_type_panics() {
        let source = r#"
            main: () -> Int
            main = "Hello world"
        "#;
        run_typechecker(source);
    }

    #[test]
    fn variable_defined_in_if_does_not_propagate_type() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let a = 1
                if (true) {
                    let a = "Hello world"
                    is_string a
                }
                is_int a
            }
        "#;
        run_typechecker(source);
    }

    #[test]
    fn return_in_if_diverges_and_typechecks() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            main: () -> ()
            main {
                let a = if (true) {
                    return ()
                } else {
                    "Hello"
                }
                is_string a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    fn return_type_of_if_is_based_on_both_branches() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            main: () -> ()
            main {
                let a = if (true) {
                    "Hello"
                } else {
                    "World"
                }
                is_string a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    #[should_panic]
    fn return_type_of_if_is_based_on_both_branches_error() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()
            
            main: () -> ()
            main {
                let a = if (true) {
                    "Hello"
                } else {
                    1
                }
                is_string a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    fn assigning_functions_typecheck() {
        let source = r#"
            is_function: (() -> ()) -> ()
            is_function f = ()

            main: () -> ()
            main {
                let a = main
                is_function main
                is_function a
            }
            "#;
        run_typechecker(source);
    }

    #[test]
    fn struct_literals_typecheck() {
        let source = r#"
            type Foo {
                a: Int
                b: String
            }

            is_foo: (Foo) -> ()
            is_foo f = ()

            main: () -> ()
            main {
                let a = Foo{ a = 1; b = "Hello"}
                is_foo a
            }
        "#;
        run_typechecker(source);
    }

    #[test]
    fn anon_types_typecheck() {
        let source = r#"
            type Foo {
                a: Int
                b: String
            }

            is_string: (String) -> ()
            is_string s = ()

            is_foo: (Foo) -> ()
            is_foo f = ()

            main: () -> ()
            main {
                let a = Foo { a = 1;  b = "Hello"}
                let anon = type { foo = a; bar = "World" }
                is_foo(anon.foo)
                is_string(anon.bar)
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn nested_anon_types_typecheck() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()

            main: () -> ()
            main {
                let anon = type { a = type { b = type { c = "Hello" } } }
                is_string(anon.a.b.c)
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn check_typechecking_arrays_with_externally_provided_type() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            main: () -> ()
            main {
                let arr = array[0]Int
                is_arr arr
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn check_typechecking_arrays_with_type_inferred_by_call() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            main: () -> ()
            main {
                let arr = array[0]
                is_arr arr
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    #[should_panic]
    fn check_typechecking_fails_arrays_with_assigned_typed() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            is_arr_2: ([String]) -> ()
            is_arr_2 a = ()

            main: () -> ()
            main {
                let arr = array[0]
                is_arr arr
                is_arr_2 arr 
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn check_array_with_type_by_elements() {
        let source = r#"
            is_arr: ([Int]) -> ()
            is_arr a = ()

            main: () -> ()
            main {
                let arr = array{1, 2}
                is_arr arr
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn infer_type_based_on_access_call() {
        let source = r#"
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let arr = array[1]
                is_int(arr[0])
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    #[should_panic]
    fn infer_type_based_on_access_assign_rhs() {
        let source = r#"
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let arr = array[1]
                let a: String = arr[0]
                is_int(arr[0])
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn infer_type_based_on_access_assign_lhs() {
        let source = r#"
            is_int: (Int) -> ()
            is_int i = ()

            main: () -> ()
            main {
                let arr = array[1]
                arr[0] = 1
                is_int(arr[0])
            }
        "#;
        run_typechecker(source)
    }

    #[test]
    fn generic_functions() {
        let source = r#"
            is_string: (String) -> ()
            is_string s = ()

            id: forall a => (a) -> a
            id a = a

            main: () -> ()
            main {
                is_string(id "hello")
            }
        "#;
        run_typechecker(source)
    }
}
