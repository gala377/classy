use std::collections::HashMap;

use crate::{
    typecheck::{ast_to_type::PrefexScope, types::DeBruijn},
    v2::knowledge::{ClassMethodBlock, DefinitionId},
};

use classy_blackboard::{
    self as blackboard,
    clauses::Clause,
    database::{AnswerOrigin, GenericRef},
    ty::Constraint,
    DomainGoal,
};

use super::{
    knowledge::{self, GenericConstraint, Id, InstanceMethodBlock, MethodHandle},
    ty::Type,
};

#[derive(Debug)]
pub enum ResolvedMethod {
    Static {
        /// Statically known method.
        /// Might reference a method within a method block or a global instance.
        def_id: Id<DefinitionId>,
    },
    FromInstanceInScope {
        /// References method declaration within a class definition.
        /// As the method implementation is not known at compile time.
        method_id: Id<DefinitionId>,
        /// References instance from constraints from an enclosing scopes.
        ///
        /// For example:
        ///   methods for { Read(a), Show(a) } => Foo(a) {
        ///     foo: () -> String {
        ///       inner_value.show()
        ///     }
        ///   }
        ///
        /// In the case above the resolution of `show` should yield `Show(a)`
        /// instance. This would be represented same as generic types
        /// indexes. In the above example it would be (0, 1) as `Show`
        /// is the second constraint in innermost constraint scope.
        referenced_constraint: usize,
    },
}

#[derive(Debug)]
pub enum MethodResolutionError {
    ReceiverNotResolved(Type),
    MethodNotFound,
    Ambiguity { candidates: Vec<ResolvedMethod> },
}

pub struct MethodResolver<'db, 'scope> {
    database: &'db knowledge::Database,
    blackboard_database: blackboard::Database,
    forest: blackboard::slg::Forest,

    constarints_in_scope: Vec<blackboard::ty::Constraint>,
    generics_scope: &'scope PrefexScope,

    class_to_class_id: HashMap<Id<DefinitionId>, blackboard::ty::ClassRef>,
    instance_to_instance_id: HashMap<Id<DefinitionId>, blackboard::ty::InstanceRef>,
    type_to_type_id: HashMap<Id<DefinitionId>, blackboard::ty::TyRef>,
    meth_block_to_meth_block_id: HashMap<Id<DefinitionId>, blackboard::ty::MethodBlockRef>,

    synthetic_method_blocks: HashMap<blackboard::ty::MethodBlockRef, SyntheticData>,
}

#[allow(dead_code)]
enum SyntheticData {
    FromInstance(Id<DefinitionId>),
    FromClass(Id<DefinitionId>),
}

impl<'db, 'scope> MethodResolver<'db, 'scope> {
    /// create blackboard database for the given function
    /// using definitions in scope and function constraints
    pub fn within_function(
        database: &'db knowledge::Database,
        generics_scope: &'scope PrefexScope,
        constraints_in_scope: Vec<GenericConstraint>,
        visible_instances: Vec<Id<DefinitionId>>,
        visible_method_blocks: Vec<Id<DefinitionId>>,
        // probably all types of the compilation need to be there
        // as a method can return a type that is not imported
        visible_types: Vec<Id<DefinitionId>>,
        classes: Vec<Id<DefinitionId>>,
    ) -> Self {
        let blackboard_database = blackboard::Database::new();
        let mut resolver = Self {
            database,
            blackboard_database,
            forest: blackboard::slg::Forest::new(),
            constarints_in_scope: Vec::new(),

            generics_scope,

            class_to_class_id: HashMap::new(),
            instance_to_instance_id: HashMap::new(),
            type_to_type_id: HashMap::new(),
            meth_block_to_meth_block_id: HashMap::new(),
            synthetic_method_blocks: HashMap::new(),
        };

        resolver.reserve_classes(&classes);
        resolver.reserve_types(&visible_types);
        resolver.add_classes(&classes);
        resolver.add_types(&visible_types);
        resolver.add_instances(&visible_instances);
        resolver.add_method_blocks(&visible_method_blocks);

        resolver.blackboard_database.lower_to_clauses();

        let constraints_in_scope = resolver.translate_constraints(&constraints_in_scope);
        resolver.constarints_in_scope = constraints_in_scope;

        resolver
    }

    fn add_method_blocks(&mut self, method_blocks: &[Id<DefinitionId>]) {
        for method_block in method_blocks {
            self.add_method_block(*method_block, &[], &[], None);
        }
    }

    fn add_method_block(
        &mut self,
        method_block: Id<DefinitionId>,
        additional_constraints: &[GenericConstraint],
        additional_free_vars: &[String],
        synthetic: Option<SyntheticData>,
    ) {
        let package = method_block.package;
        let method_block_definition = self.database.get_definition(method_block).unwrap();
        let constraints = additional_constraints
            .iter()
            .chain(&method_block_definition.constraints)
            .cloned()
            .collect::<Vec<_>>();
        let constraints = self.translate_constraints(&constraints);
        let info = method_block_definition
            .kind
            .as_method_block()
            .cloned()
            .unwrap();
        let receiver = self.database.resolve_alias_to_type(info.receiver).unwrap();
        let receiver = self.to_blackboard_type(&receiver);
        let methods = info
            .methods
            .iter()
            .map(|MethodHandle { name, definition }| {
                let definition = definition.as_global(package);
                let definition = self.database.get_definition(definition).unwrap();
                let info = definition.kind.as_method().cloned().unwrap();
                let method_type = info.ty.as_global(package);
                let method_type = self.database.resolve_alias_to_type(method_type).unwrap();
                let (free_vars, method_type) = match method_type {
                    Type::Scheme { prefex, typ } => (prefex, *typ),
                    t => (Vec::new(), t),
                };
                let method_type = self.to_blackboard_type(&method_type);
                blackboard::database::Definition {
                    type_params: free_vars,
                    name: name.clone(),
                    ty: method_type,
                }
            })
            .collect();
        let type_params = info
            .free_vars
            .iter()
            .chain(additional_free_vars)
            .cloned()
            .collect();
        let b_methods_block = blackboard::database::MethodsBlock {
            on_type: receiver,
            name: None,
            type_params,
            constraints,
            methods,
        };
        let methods_block_ref = self.blackboard_database.add_method_block(b_methods_block);
        self.meth_block_to_meth_block_id
            .insert(method_block, methods_block_ref.clone());

        if let Some(synthetic) = synthetic {
            self.synthetic_method_blocks
                .insert(methods_block_ref.clone(), synthetic);
        }
        let method_block = self.blackboard_database.get_method_block(
            classy_blackboard::database::GenericRef::MethodBlock(methods_block_ref),
        );
        println!("Method block: {:#?}", method_block);
    }

    fn add_instances(&mut self, instances: &[Id<DefinitionId>]) {
        for instance in instances {
            let package = instance.package;
            let instance_definition = self.database.get_definition(*instance).unwrap();
            let constraints = self.translate_constraints(&instance_definition.constraints);
            let info = instance_definition.kind.as_instance().cloned().unwrap();
            let GenericConstraint { class, args } = info.receiver.clone();
            let class_ref = *self.class_to_class_id.get(&class).unwrap();
            let args = args.iter().map(|ty| self.to_blackboard_type(ty)).collect();
            // ! Why instance does not have members?
            // ! Looking for methods in instances is not supported yet.
            // ! Oh well, that's fine for now I guess.
            let b_instance = blackboard::database::Instance {
                type_class: class_ref,
                args,
                type_params: info.free_vars.clone(),
                constraints,
            };
            let instance_ref = self.blackboard_database.add_instance(b_instance);
            self.instance_to_instance_id
                .insert(*instance, instance_ref);
            for InstanceMethodBlock { id, .. } in &info.method_blocks {
                let id = id.as_global(package);
                self.add_method_block(
                    id,
                    &[info.receiver.clone()],
                    &info.free_vars,
                    Some(SyntheticData::FromInstance(*instance)),
                );
            }
        }
    }

    fn reserve_classes(&mut self, classes: &[Id<DefinitionId>]) {
        for class in classes {
            let resolved_class = self.database.get_class(*class).unwrap();
            let class_id = self.blackboard_database.reserve_class(&resolved_class.name);
            self.class_to_class_id.insert(*class, class_id);
        }
    }

    fn reserve_types(&mut self, types: &[Id<DefinitionId>]) {
        for ty in types {
            let resolved_ty = self
                .database
                .get_definition_map(*ty, |def| {
                    assert!(matches!(def.kind, knowledge::DefinitionKind::Type));
                    def.name.clone()
                })
                .unwrap();
            let type_id = self.blackboard_database.reserve_type_impl(&resolved_ty);
            self.type_to_type_id.insert(*ty, type_id);
        }
    }

    fn add_classes(&mut self, classes: &[Id<DefinitionId>]) {
        for class in classes {
            let package = class.package;
            let resolved_class = self.database.get_definition(*class).unwrap();
            let type_params = resolved_class.kind.as_class().unwrap().arguments.clone();
            let class_ref = *self.class_to_class_id.get(class).unwrap();
            let constraints = self.translate_constraints(&resolved_class.constraints);
            let class_def = blackboard::database::TypeClass {
                name: resolved_class.name.clone(),
                type_params,
                constraints,
                // ! I am pretty sure this does not matter as we only resolve methods
                // ! from method blocks and instance definitions so we will never look at
                // ! the members of the class
                members: Vec::new(),
            };
            self.blackboard_database.replace_class(class_ref, class_def);
            let class_info = resolved_class.kind.as_class().unwrap();
            for ClassMethodBlock { id, .. } in &class_info.method_blocks {
                let id = id.as_global(package);
                self.add_method_block(
                    id,
                    &[GenericConstraint {
                        class: *class,
                        args: class_info
                            .arguments
                            .iter()
                            .enumerate()
                            .map(|(i, _)| Type::Generic(DeBruijn::zero(), i))
                            .collect(),
                    }],
                    &class_info.arguments,
                    Some(SyntheticData::FromClass(*class)),
                );
            }
        }
    }

    fn translate_constraints(
        &self,
        constraints: &[GenericConstraint],
    ) -> Vec<blackboard::ty::Constraint> {
        let mut res = Vec::new();
        for GenericConstraint { class, args } in constraints {
            let class_ref = self.class_to_class_id.get(class).unwrap();
            let args = args.iter().map(|ty| self.to_blackboard_type(ty)).collect();
            res.push(blackboard::ty::Constraint::Class(*class_ref, args));
        }
        res
    }

    fn add_types(&mut self, types: &[Id<DefinitionId>]) {
        for ty in types {
            let resolved_ty = self.database.get_definition(*ty).unwrap();
            let type_ref = *self.type_to_type_id.get(ty).unwrap();
            let type_params = {
                let t = self.database.get_type(*ty).unwrap();
                match t {
                    Type::Scheme { prefex, .. } => prefex.clone(),
                    _ => Vec::new(),
                }
            };
            let constraints = self.translate_constraints(&resolved_ty.constraints);
            let type_def = blackboard::database::TypeImpl {
                name: resolved_ty.name.clone(),
                type_params,
                constraints,
                // ! I don't think we will ever look at the fields when resolving
                // ! methods so this is fine I think
                fields: Vec::new(),
            };
            self.blackboard_database
                .replace_type_impl(type_ref, type_def);
        }
    }

    // For free function calls within methods the receiver should
    // be a type of `this`
    pub fn resolve_method(
        &mut self,
        receiver: &Type,
        method: &str,
    ) -> Result<ResolvedMethod, MethodResolutionError> {
        if !self.database.is_resolved_type(receiver) {
            return Err(MethodResolutionError::ReceiverNotResolved(receiver.clone()));
        }
        let query = self.create_blackboard_query(method, receiver);

        let solver =
            blackboard::slg::SlgSolver::new(&self.blackboard_database, &mut self.forest, query);
        let mut answers: Vec<_> = solver.into_iter().collect();
        println!("answers: {:#?}", answers);
        if answers.is_empty() {
            return Err(MethodResolutionError::MethodNotFound);
        }
        if answers.len() > 1 {
            let filtered_out_synthetic_blocks = answers
                .iter()
                .filter(|answer| match &answer.origin {
                    AnswerOrigin::FromRef(GenericRef::MethodBlock(block_id)) => {
                        match self.synthetic_method_blocks.get(block_id) {
                            Some(SyntheticData::FromClass(_)) => false,
                            _ => true,
                        }
                    }
                    _ => true,
                })
                .collect::<Vec<_>>();
            if filtered_out_synthetic_blocks.len() != 1 {
                let candidates = answers
                    .iter()
                    .map(|a| self.method_from_origin(a, method))
                    .collect();
                println!("Ambiguity: {:#?}", candidates);
                return Err(MethodResolutionError::Ambiguity { candidates });
            }
            answers = filtered_out_synthetic_blocks
                .into_iter()
                .cloned()
                .collect::<Vec<_>>();
        }
        // ! For now we ignore evidence and so on
        let resolved = self.method_from_origin(&answers[0], method);
        Ok(resolved)
    }

    fn method_from_origin(
        &self,
        answers: &blackboard::slg::Answer,
        method: &str,
    ) -> ResolvedMethod {
        let blackboard::slg::Answer {
            origin, evidence, ..
        } = answers;
        match origin {
            AnswerOrigin::FromRef(classy_blackboard::database::GenericRef::MethodBlock(id)) => {
                let kid = self
                    .meth_block_to_meth_block_id
                    .iter()
                    .find_map(|(k, v)| if v == id { Some(k) } else { None })
                    .unwrap();
                if self.synthetic_method_blocks.get(id).is_some() {
                    println!("EVIDENCE: {:#?}", evidence);
                    if let AnswerOrigin::FromAssumption(index) = evidence[0].origin {
                        let definition = self.database.get_definition(*kid).unwrap();
                        let info = definition.kind.as_method_block().cloned().unwrap();
                        let method = info
                            .methods
                            .iter()
                            .find(|MethodHandle { name, .. }| name == method)
                            .unwrap()
                            .definition;
                        return ResolvedMethod::FromInstanceInScope {
                            method_id: method.as_global(kid.package),
                            referenced_constraint: index,
                        };
                    }
                }
                let method_block = self
                    .database
                    .get_definition_map(*kid, |def| {
                        def.kind.as_method_block().cloned().unwrap()
                    })
                    .unwrap();
                method_block
                    .methods
                    .iter()
                    .find_map(|MethodHandle { name, definition }| {
                        if name == method {
                            Some(ResolvedMethod::Static {
                                def_id: definition.as_global(kid.package),
                            })
                        } else {
                            None
                        }
                    })
                    .unwrap()
            }
            _ => todo!("Only method blocks are supported for now"),
        }
    }

    fn create_blackboard_query(&self, method: &str, receiver: &Type) -> blackboard::Goal {
        let receiver_as_blackboard_type = self.to_blackboard_type(receiver);
        let mut query = blackboard::Goal::Domain(blackboard::DomainGoal::FindMethod {
            name: method.to_owned(),
            on_type: receiver_as_blackboard_type,
        });
        let constraints = self
            .constarints_in_scope
            .iter()
            .map(|c| match c {
                Constraint::Class(class, args) => {
                    Clause::Fact(DomainGoal::InstanceExistsAndWellFormed {
                        head: *class,
                        args: args.clone(),
                    })
                }
                Constraint::Eq(_, _) => todo!(),
            })
            .collect::<Vec<_>>();
        if !constraints.is_empty() {
            query = blackboard::Goal::Implies(constraints, Box::new(query));
        }
        let query = self
            .generics_scope
            .iter_scopes()
            // ? Maybe this is should be enabled in the future but with how generic scopes
            // ? are handled now i don't think we should ever get en empty prefex scope
            //.filter(|s| !s.is_empty())
            .fold(query, |query, scope| {
                blackboard::Goal::Forall(scope.len(), Box::new(query))
            });
        println!("Query: {:#?}", query);
        query
    }

    fn to_blackboard_type(&self, ty: &Type) -> blackboard::Ty {
        match ty {
            // blackbooard does not know about basic types so we need to
            // return a type ref to them if we added them before.
            Type::Struct { def, .. } => {
                blackboard::Ty::Ref(*self.type_to_type_id.get(def).unwrap())
            }
            Type::ADT { def, .. } => {
                blackboard::Ty::Ref(*self.type_to_type_id.get(def).unwrap())
            }
            Type::Function { args, ret } => {
                let args = args
                    .iter()
                    .map(|t| self.to_blackboard_type(t))
                    .collect::<Vec<_>>();
                let ret = self.to_blackboard_type(ret);
                blackboard::Ty::Fn(args, Box::new(ret))
            }
            Type::Tuple(inner) => {
                let inner = inner
                    .iter()
                    .map(|t| self.to_blackboard_type(t))
                    .collect::<Vec<_>>();
                blackboard::Ty::Tuple(inner)
            }
            Type::Array(inner) => {
                let inner = self.to_blackboard_type(inner);
                blackboard::Ty::Array(Box::new(inner))
            }
            Type::Alias(id) => {
                let t = self.database.resolve_alias_to_type(*id).unwrap();
                self.to_blackboard_type(&t)
            }

            Type::App { typ, args } => {
                let typ = self.to_blackboard_type(typ);
                let args = args
                    .iter()
                    .map(|t| self.to_blackboard_type(t))
                    .collect::<Vec<_>>();
                blackboard::Ty::App(Box::new(typ), args)
            }
            Type::Generic(DeBruijn(scopes), index) => blackboard::Ty::Generic {
                scopes: *scopes as usize,
                index: *index,
            },
            Type::Scheme { typ, .. } => {
                // ! This is like 99% incorrect to ignore the prefex but oh well.
                // ! I want to get somewhere, i will fix it later
                self.to_blackboard_type(typ)
            }
            Type::Divergent => panic!("Divergent type should not be resolved"),
            Type::ToInfere => panic!("ToInfere type should not be resolved"),
            Type::Fresh(_) => panic!("Fresh type should not be resolved"),

            t => {
                println!("T is {:#?}", t);
                blackboard::Ty::Ref(
                    *self.type_to_type_id
                        .get(&self.database.get_primitive_type(t).unwrap())
                        .unwrap(),
                )
            }
        }
    }
}

/*
def find_most_specific_instance(instances)
  outer: for candidate in instances
    for other in instances if other != candidate
      if not substitutes(candidate, other)
        continue outer
    return candidate
  return ambiguity error
end

def substitutes(candidate, other)
  unions_left(candidate, other) and
    meets_constraints(candidate, of=other)
end

def meets_constraints(candidate, of)
  let x' = union_left(candidate, of)
  for constraint in of.constraints
    under constraints of candidate:
      if not meets_constraint(x, constraint)
        return false
  return true
end

def union_left(candidate, other)
  match (candidate, other)
    case (TVar, TVar) -> okay
    case (TVar, T) -> fail
    case (T, TVar) -> okay
    case (T1, T2) -> recurse
*/

#[cfg(test)]
mod tests {

    use knowledge::{PackageId, CURRENT_PACKAGE_ID};

    use self::knowledge::{Definition, DefinitionKind, LocalId, PackageInfo, TypeId};

    use super::*;
    use crate::session::Session;
    use crate::v2::compile::Compiler;
    use crate::v2::knowledge::Database;
    use std::collections::HashMap;

    const SOURCE_1: &str = r#"
        // import std::Int 

        type Foo {}

        methods for Foo {
            foo: () -> std::Int
            foo () {} 

            bar: () -> Foo 
            bar () {}
        }

        methods for Foo {
            baz: () -> Foo 
            baz () {} 
        }

        type Bar(a) {}

        methods for Bar(a) {
            foo: () -> Bar(a)
            foo () {}

            bar: () -> a 
            bar () {}
        }

    "#;

    fn prepare_std_package() -> PackageInfo {
        let types = vec![
            ("String", crate::v2::ty::Type::String),
            ("Int", crate::v2::ty::Type::Int),
            ("Bool", crate::v2::ty::Type::Bool),
            ("Float", crate::v2::ty::Type::Float),
            ("Byte", crate::v2::ty::Type::Byte),
            ("UInt", crate::v2::ty::Type::UInt),
            ("Unit", crate::v2::ty::Type::Unit),
        ];
        PackageInfo {
            name: "std".to_string(),
            globals: {
                let mut globals = HashMap::new();
                for (i, (name, _)) in types.iter().enumerate() {
                    globals.insert(name.to_string(), LocalId(DefinitionId(i)));
                }
                globals
            },
            definition: {
                let mut map = HashMap::new();
                for (i, (name, _)) in types.iter().enumerate() {
                    map.insert(
                        LocalId(DefinitionId(i)),
                        Definition {
                            name: name.to_string(),
                            kind: DefinitionKind::Type,
                            constraints: vec![],
                            ty: LocalId(TypeId(i)),
                            file: LocalId(DefinitionId(0)),
                            implicit_imports: vec![],
                            parent: None,
                        },
                    );
                }
                map
            },
            typeid_to_type: {
                let mut map = HashMap::new();
                for (i, (_, ty)) in types.iter().enumerate() {
                    map.insert(LocalId(TypeId(i)), ty.clone());
                }
                map
            },
            method_blocks: Default::default(),
            classes: Default::default(),
            instances: Default::default(),
            methods: Default::default(),
        }
    }

    fn setup_database(source: &str) -> (Database, Session) {
        let std_package = prepare_std_package();
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
            .map(|(id, t)| (id.as_global(PackageId(1)), t))
            .collect::<Vec<_>>();
        let compiler = Compiler::new(
            "test",
            vec![("test".into(), source.into())],
            vec![std_package],
        );
        let (mut db, sess) = compiler.make_database();
        for (name, id) in db.globals.iter() {
            println!("{name}: {id:?}");
            let def = db.get_global(name).unwrap();
            let ty = db.get_type(def.as_global(PackageId(0)));
            println!("type: {ty:?}");
        }
        for (id, ty) in primitive_types {
            db.add_primitive_type(ty.clone(), id);
        }

        (db, sess)
    }

    fn get_types(database: &Database) -> Vec<Id<DefinitionId>> {
        let mut types = database
            .type_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect::<Vec<_>>();
        let mut std_package_types = database.packages[0]
            .definition
            .iter()
            .filter_map(|(id, def)| match def.kind {
                DefinitionKind::Type => Some(id.as_global(PackageId(1))),
                _ => None,
            })
            .collect();
        types.append(&mut std_package_types);
        types
    }

    fn get_method_blocks(database: &Database) -> Vec<Id<DefinitionId>> {
        for method_block in database.method_blocks_definitions.keys() {
            println!("Method block: {method_block:#?}");
        }
        database
            .method_blocks_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect()
    }

    fn get_classes(database: &Database) -> Vec<Id<DefinitionId>> {
        database
            .class_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect()
    }

    fn get_instances(database: &Database) -> Vec<Id<DefinitionId>> {
        database
            .instance_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect()
    }

    fn get_type(database: &Database, name: &str) -> Type {
        database
            .get_type_by_unresolved_name(&[], &[], name)
            .unwrap()
            .clone()
    }

    fn get_class(database: &Database, name: &str) -> Id<DefinitionId> {
        database
            .get_definition_id_by_unresolved_name(&[], &[], name)
            .unwrap()
    }

    #[test]
    fn simple_resolve_method() {
        let (database, _) = setup_database(SOURCE_1);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let scope = PrefexScope::with_empty_scope();
        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            Vec::new(),
            method_blocks,
            types,
            Vec::new(),
        );
        let receiver = get_type(&database, "Foo");
        let methods = vec!["foo", "bar", "baz"];
        for name in methods {
            let ResolvedMethod::Static { def_id } =
                resolver.resolve_method(&receiver, name).unwrap()
            else {
                panic!("Method not found: {name}");
            };
            let definition = database.get_definition(def_id).unwrap();
            assert_eq!(definition.name, name);
            assert!(matches!(definition.kind, DefinitionKind::Method(_)));
        }
    }

    #[test]
    fn simple_resolve_methods_generic() {
        let (database, _) = setup_database(SOURCE_1);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let scope = PrefexScope::with_empty_scope();
        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            Vec::new(),
            method_blocks,
            types,
            Vec::new(),
        );
        let bar = get_type(&database, "Bar");
        let receiver = Type::App {
            typ: Box::new(bar.clone()),
            args: vec![get_type(&database, "Foo")],
        };
        let methods = vec!["foo", "bar"];
        for name in methods {
            let ResolvedMethod::Static { def_id } =
                resolver.resolve_method(&receiver, name).unwrap()
            else {
                panic!("Method not found: {name}")
            };
            let definition = database.get_definition(def_id).unwrap();
            assert_eq!(definition.name, name);
            assert!(matches!(definition.kind, DefinitionKind::Method(_)));
        }
    }

    const SOURCE_2: &str = r#"

        type Foo(a) {}
        type Baz {}
        type Bar {}

        methods for Foo(Baz) {
            foo: () -> Baz
            foo () {} 
        }

        methods for Foo(Bar) {
            foo: () -> Bar 
            foo () {} 
        }

    "#;

    #[test]
    fn resolve_between_2_overloads() {
        let (database, _) = setup_database(SOURCE_2);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let scope = PrefexScope::with_empty_scope();
        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            Vec::new(),
            method_blocks,
            types,
            Vec::new(),
        );
        let foo = get_type(&database, "Foo");
        let bar = get_type(&database, "Bar");
        let receiver = Type::App {
            typ: Box::new(foo.clone()),
            args: vec![bar.clone()],
        };
        let ResolvedMethod::Static { def_id } = resolver.resolve_method(&receiver, "foo").unwrap()
        else {
            panic!("Method not found")
        };
        let definition = database.get_definition(def_id).unwrap();
        let ty = database
            .resolve_alias_to_type(definition.ty.as_global(CURRENT_PACKAGE_ID))
            .unwrap();
        assert_eq!(definition.name, "foo");
        assert!(matches!(definition.kind, DefinitionKind::Method(_)));
        match ty {
            Type::Function { args, ret } if args.is_empty() => {
                let t = database.resolve_if_alias(&ret).unwrap();
                assert_eq!(bar, t);
            }
            _ => panic!(),
        }
    }

    const SOURCE_3: &str = r#"
         type Foo(a) {}
         methods for Foo(a) {
             foo: () -> a
             foo () {}
         }

         methods for Foo(std::Int) {
             foo: () -> std::Int
             foo () {}
         }
    "#;

    #[test]
    fn resolve_between_2_overloads_generic() {
        let (database, _) = setup_database(SOURCE_3);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);

        let mut scope = PrefexScope::with_empty_scope();
        scope.add_type_var("a");
        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            Vec::new(),
            method_blocks,
            types,
            Vec::new(),
        );
        let foo = get_type(&database, "Foo");
        let receiver = Type::App {
            typ: Box::new(foo.clone()),
            args: vec![Type::Generic(DeBruijn::zero(), 0)],
        };
        let ResolvedMethod::Static { def_id } = resolver.resolve_method(&receiver, "foo").unwrap()
        else {
            panic!("Method not found")
        };
        let definition = database.get_definition(def_id).unwrap();
        let ty = database
            .resolve_alias_to_type(definition.ty.as_global(CURRENT_PACKAGE_ID))
            .unwrap();
        assert_eq!(definition.name, "foo");
        assert!(matches!(definition.kind, DefinitionKind::Method(_)));
        match ty {
            Type::Function { args, ret } if args.is_empty() => {
                let t = database.resolve_if_alias(&ret).unwrap();
                assert_eq!(Type::Generic(DeBruijn(0), 0), t);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn resolve_between_2_overloads_generic_ambiguity() {
        let (database, _) = setup_database(SOURCE_3);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);

        let scope = PrefexScope::with_empty_scope();
        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            Vec::new(),
            method_blocks,
            types,
            Vec::new(),
        );
        let foo = get_type(&database, "Foo");
        let receiver = Type::App {
            typ: Box::new(foo.clone()),
            args: vec![Type::Int],
        };

        let Err(MethodResolutionError::Ambiguity { candidates }) =
            resolver.resolve_method(&receiver, "foo")
        else {
            panic!("Fail");
        };
        assert_eq!(candidates.len(), 2);
    }

    const SOURCE_4: &str = r#"
         type Foo(a) {}
         class Debug(a) {}
         class Clone(a) {}

         instance for Clone(std::String) {}

         methods for { Clone(a) } => Foo(a) {
             foo: () -> a
             foo () {}
         }
    "#;

    #[test]
    fn resolve_between_with_class_constraints() {
        let (database, _) = setup_database(SOURCE_4);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let classes = get_classes(&database);
        let instances = get_instances(&database);
        let scope = PrefexScope::with_empty_scope();

        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            instances,
            method_blocks,
            types,
            classes,
        );
        let foo = get_type(&database, "Foo");
        let receiver = Type::App {
            typ: Box::new(foo.clone()),
            args: vec![Type::Int],
        };

        let Err(MethodResolutionError::MethodNotFound) = resolver.resolve_method(&receiver, "foo")
        else {
            panic!("Fail");
        };

        let receiver = Type::App {
            typ: Box::new(foo.clone()),
            args: vec![Type::String],
        };
        let ResolvedMethod::Static { .. } = resolver.resolve_method(&receiver, "foo").unwrap()
        else {
            panic!("Method not found")
        };
    }

    #[test]
    fn resolve_between_within_context() {
        let (database, _) = setup_database(SOURCE_4);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let classes = get_classes(&database);
        let instances = get_instances(&database);
        let mut scope = PrefexScope::with_empty_scope();
        scope.add_type_var("a");

        let constraints = vec![
            GenericConstraint {
                class: get_class(&database, "Debug"),
                args: vec![Type::Generic(DeBruijn::zero(), 0)],
            },
            GenericConstraint {
                class: get_class(&database, "Clone"),
                args: vec![Type::Generic(DeBruijn::zero(), 0)],
            },
        ];
        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            constraints,
            instances,
            method_blocks,
            types,
            classes,
        );
        let foo = get_type(&database, "Foo");
        let receiver = Type::App {
            typ: Box::new(foo.clone()),
            args: vec![Type::Generic(DeBruijn::zero(), 0)],
        };

        let ResolvedMethod::Static { .. } = resolver.resolve_method(&receiver, "foo").unwrap()
        else {
            panic!("Method not found")
        };
    }

    const ADTS_SOURCE: &str = r#"
    type Foo {
        A
        B(std::Int)
    }

    methods for Foo {
      foo: () -> () 
      foo () {}
    }
    "#;

    #[test]
    fn resolve_methods_for_adt_type() {
        let (database, _) = setup_database(ADTS_SOURCE);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let classes = get_classes(&database);
        let instances = get_instances(&database);
        let scope = PrefexScope::with_empty_scope();
        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            instances,
            method_blocks,
            types,
            classes,
        );
        let receiver = get_type(&database, "Foo");
        let Ok(ResolvedMethod::Static { .. }) = resolver.resolve_method(&receiver, "foo") else {
            panic!("Method not found")
        };
    }

    const METHOD_BLOCKS: &str = r#"
      class C(a) {
        methods for a {
          foo: () -> std::Int
        }
      }
      type Foo {}

      instance for C(Foo) {
        methods for Foo {
          foo: () -> std::Int
          foo () {}
        }
      }
      "#;

    #[test]
    fn resolve_instance_methdos() {
        tracing_subscriber::fmt().pretty().init();
        let (database, _) = setup_database(METHOD_BLOCKS);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let classes = get_classes(&database);
        let instances = get_instances(&database);
        let mut scope = PrefexScope::with_empty_scope();
        scope.add_type_var("a");

        let constraints = vec![GenericConstraint {
            class: get_class(&database, "C"),
            args: vec![Type::Generic(DeBruijn::zero(), 0)],
        }];

        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            constraints,
            instances,
            method_blocks,
            types,
            classes,
        );
        let receiver = Type::Generic(DeBruijn::zero(), 0);
        match resolver.resolve_method(&receiver, "foo") {
            Ok(ResolvedMethod::FromInstanceInScope { .. }) => {}
            err => panic!("Error {err:?}"),
        }
    }

    #[test]
    fn resolve_instance_methods_static() {
        tracing_subscriber::fmt().pretty().init();
        let (database, _) = setup_database(METHOD_BLOCKS);
        let types = get_types(&database);
        let method_blocks = get_method_blocks(&database);
        let classes = get_classes(&database);
        let instances = get_instances(&database);
        let scope = PrefexScope::with_empty_scope();

        let mut resolver = MethodResolver::within_function(
            &database,
            &scope,
            Vec::new(),
            instances,
            method_blocks,
            types,
            classes,
        );
        let receiver = get_type(&database, "Foo");
        match resolver.resolve_method(&receiver, "foo") {
            Ok(ResolvedMethod::Static { .. }) => {}
            err => panic!("Error {err:?}"),
        }
    }
}
