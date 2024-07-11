use std::collections::HashMap;

use crate::{typecheck::types::DeBruijn, v2::knowledge::DefinitionId};

use classy_blackboard as blackboard;

use super::{
    knowledge::{self, GenericConstraint, Id, InstanceInfo, MethodHandle},
    ty::Type,
};

#[derive(Debug)]
pub struct ResolvedMethod {
    def_id: Id<DefinitionId>,
}

#[derive(Debug)]
pub enum MethodResolutionError {
    ReceiverNotResolved(Type),
    MethodNotFound,
    Ambiguity,
}

struct MethodResolver<'db> {
    database: &'db knowledge::Database,
    blackboard_database: blackboard::Database,
    forest: blackboard::slg::Forest,

    constarints_in_scope: Vec<blackboard::ty::Constraint>,

    class_to_class_id: HashMap<Id<DefinitionId>, blackboard::ty::ClassRef>,
    instance_to_instance_id: HashMap<Id<DefinitionId>, blackboard::ty::InstanceRef>,
    type_to_type_id: HashMap<Id<DefinitionId>, blackboard::ty::TyRef>,
    meth_block_to_meth_block_id: HashMap<Id<DefinitionId>, blackboard::ty::MethodBlockRef>,
}

impl<'db> MethodResolver<'db> {
    /// create blackboard database for the given function
    /// using definitions in scope and function constraints
    pub fn within_function(
        database: &'db knowledge::Database,
        constraints_in_scope: Vec<GenericConstraint>,
        visible_instances: Vec<Id<DefinitionId>>,
        visible_method_blocks: Vec<Id<DefinitionId>>,
        visible_types: Vec<Id<DefinitionId>>,
        classes: Vec<Id<DefinitionId>>,
    ) -> Self {
        if !constraints_in_scope.is_empty() {
            panic!("Constraints in scope are not supported yet");
        }
        let blackboard_database = blackboard::Database::new();
        let mut resolver = Self {
            database,
            blackboard_database,
            forest: blackboard::slg::Forest::new(),
            constarints_in_scope: Vec::new(),
            class_to_class_id: HashMap::new(),
            instance_to_instance_id: HashMap::new(),
            type_to_type_id: HashMap::new(),
            meth_block_to_meth_block_id: HashMap::new(),
        };

        resolver.reserve_classes(&classes);
        resolver.reserve_types(&visible_types);
        resolver.add_classes(&classes);
        resolver.add_types(&visible_types);
        resolver.add_instances(&visible_instances);
        resolver.add_method_blocks(&visible_method_blocks);

        resolver.blackboard_database.lower_to_clauses();

        resolver
    }

    fn add_method_blocks(&mut self, method_blocks: &[Id<DefinitionId>]) {
        for method_block in method_blocks {
            let package = method_block.package.clone();
            let method_block_definition = self
                .database
                .get_definition_map(method_block.clone(), |def| def.clone())
                .unwrap();
            let constraints = self.translate_constraints(&method_block_definition.constraints);
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
                    let definition = definition.as_global(package.clone());
                    let definition = self
                        .database
                        .get_definition_map(definition, |id| id.clone())
                        .unwrap();
                    let info = definition.kind.as_method().cloned().unwrap();
                    let method_type = info.ty.as_global(package.clone());
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
            let b_methods_block = blackboard::database::MethodsBlock {
                on_type: receiver,
                name: None,
                type_params: info.free_vars,
                constraints,
                methods,
            };
            let methods_block_ref = self.blackboard_database.add_method_block(b_methods_block);
            self.meth_block_to_meth_block_id
                .insert(method_block.clone(), methods_block_ref);
        }
    }

    fn add_instances(&mut self, instances: &[Id<DefinitionId>]) {
        for instance in instances {
            let instance_definition = self
                .database
                .get_definition_map(instance.clone(), |def| def.clone())
                .unwrap();
            let constraints = self.translate_constraints(&instance_definition.constraints);
            let info = instance_definition.kind.as_instance().cloned().unwrap();
            let GenericConstraint { class, args } = info.receiver;
            let class_ref = self.class_to_class_id.get(&class).unwrap().clone();
            let args = args.iter().map(|ty| self.to_blackboard_type(ty)).collect();
            // ! Why instance does not have members?
            // ! Looking for methods in instances is not supported yet.
            // ! Oh well, that's fine for now I guess.
            let b_instance = blackboard::database::Instance {
                type_class: class_ref,
                args,
                type_params: info.free_vars,
                constraints,
            };
            let instance_ref = self.blackboard_database.add_instance(b_instance);
            self.instance_to_instance_id
                .insert(instance.clone(), instance_ref);
        }
    }

    fn reserve_classes(&mut self, classes: &[Id<DefinitionId>]) {
        for class in classes {
            let resolved_class = self.database.get_class(class.clone()).unwrap();
            let class_id = self.blackboard_database.reserve_class(&resolved_class.name);
            self.class_to_class_id.insert(class.clone(), class_id);
        }
    }

    fn reserve_types(&mut self, types: &[Id<DefinitionId>]) {
        for ty in types {
            let resolved_ty = self
                .database
                .get_definition_map(ty.clone(), |def| {
                    assert!(matches!(def.kind, knowledge::DefinitionKind::Type));
                    def.name.clone()
                })
                .unwrap();
            let type_id = self.blackboard_database.reserve_type_impl(&resolved_ty);
            self.type_to_type_id.insert(ty.clone(), type_id);
        }
    }

    fn add_classes(&mut self, classes: &[Id<DefinitionId>]) {
        for class in classes {
            let resolved_class = self
                .database
                .get_definition_map(class.clone(), |def| def.clone())
                .unwrap();
            let type_params = resolved_class.kind.as_class().unwrap().arguments.clone();
            let class_ref = self.class_to_class_id.get(&class).unwrap().clone();
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
        }
    }

    fn translate_constraints(
        &self,
        constraints: &[GenericConstraint],
    ) -> Vec<blackboard::ty::Constraint> {
        let mut res = Vec::new();
        for GenericConstraint { class, args } in constraints {
            let class_ref = self.class_to_class_id.get(class).unwrap();
            let args = args
                .iter()
                .map(|ty| {
                    let Type::Generic(scopes, index) = ty else {
                        panic!();
                    };
                    let scopes = scopes.0 as usize;
                    blackboard::Ty::Generic {
                        scopes,
                        index: *index,
                    }
                })
                .collect();
            res.push(blackboard::ty::Constraint::Class(class_ref.clone(), args));
        }
        res
    }

    fn add_types(&mut self, types: &[Id<DefinitionId>]) {
        for ty in types {
            let resolved_ty = self
                .database
                .get_definition_map(ty.clone(), |def| def.clone())
                .unwrap();
            let type_ref = self.type_to_type_id.get(ty).unwrap().clone();
            let type_params = {
                let t = self.database.get_type(ty.clone()).unwrap();
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

        // 2. if it is, create a blackboard query
        //   under constraints in scope
        //      find method with the given name for the receiver
        //   if not found, return error
        //   if found find the most specific method and return it
        let query = self.create_blackboard_query(method, receiver);

        let solver =
            blackboard::slg::SlgSolver::new(&self.blackboard_database, &mut self.forest, query);
        let answers: Vec<_> = solver.into_iter().take(2).collect();
        if answers.len() == 0 {
            return Err(MethodResolutionError::MethodNotFound);
        }
        if answers.len() > 1 {
            return Err(MethodResolutionError::Ambiguity);
        }
        // ! For now we ignore evidence and so on
        let blackboard::slg::Answer { origin, .. } = answers[0].clone();
        let origin = origin.unwrap();
        match origin {
            classy_blackboard::database::GenericRef::MethodBlock(id) => {
                let id = self
                    .meth_block_to_meth_block_id
                    .iter()
                    .find_map(|(k, v)| if v == &id { Some(k) } else { None })
                    .unwrap();
                let method_block = self
                    .database
                    .get_definition_map(id.clone(), |def| {
                        def.kind.as_method_block().cloned().unwrap()
                    })
                    .unwrap();
                let method_id = method_block
                    .methods
                    .iter()
                    .find_map(|MethodHandle { name, definition }| {
                        if name == method {
                            Some(definition)
                        } else {
                            None
                        }
                    })
                    .map(|mid| mid.as_global(id.package))
                    .unwrap();

                return Ok(ResolvedMethod { def_id: method_id });
            }
            _ => todo!("Only method blocks are supported for now"),
        }
    }

    fn create_blackboard_query(&self, method: &str, receiver: &Type) -> blackboard::Goal {
        //   under constraints in scope
        //      find method with the given name for the receiver
        //   if not found, return error
        //   if found find the most specific method and return it
        let receiver_as_blackboard_type = self.to_blackboard_type(receiver);
        let query = blackboard::Goal::Domain(blackboard::DomainGoal::FindMethod {
            name: method.to_owned(),
            on_type: receiver_as_blackboard_type,
        });
        if self.constarints_in_scope.is_empty() {
            return query;
        }
        let constraints = self.constarints_in_scope.clone();
        let query = todo!("change constraints into if {{ constraints }} query");
        query
    }

    fn to_blackboard_type(&self, ty: &Type) -> blackboard::Ty {
        match ty {
            // blackbooard does not know about basic types so we need to
            // return a type ref to them if we added them before.
            Type::Int => todo!(),
            Type::UInt => todo!(),
            Type::Bool => todo!(),
            Type::String => todo!(),
            Type::Float => todo!(),
            Type::Unit => todo!(),
            Type::Byte => todo!(),
            Type::Struct { def, .. } => {
                blackboard::Ty::Ref(self.type_to_type_id.get(def).unwrap().clone())
            }
            Type::ADT { def, .. } => {
                blackboard::Ty::Ref(self.type_to_type_id.get(def).unwrap().clone())
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
                let t = self.database.resolve_alias_to_type(id.clone()).unwrap();
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
            foo: () -> Foo
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
        let compiler = Compiler::new(
            "test",
            vec![("test".into(), source.into())],
            vec![std_package],
        );
        let (db, sess) = compiler.make_database();
        for (name, id) in db.globals.iter() {
            println!("{name}: {id:?}");
            let def = db.get_global(name).unwrap();
            let ty = db.get_type(def.as_global(PackageId(0)));
            println!("type: {ty:?}");
        }
        (db, sess)
    }

    #[test]
    fn simple_resolve_method() {
        let (database, _) = setup_database(SOURCE_1);
        let types = database
            .type_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect();

        let method_blocks = database
            .method_blocks_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect();
        let mut resolver = MethodResolver::within_function(
            &database,
            Vec::new(),
            Vec::new(),
            method_blocks,
            types,
            Vec::new(),
        );
        let receiver = database
            .get_type_by_unresolved_name(&[], &[], "Foo")
            .unwrap();
        let res = resolver.resolve_method(receiver, "foo");
        let res = res.unwrap();
        let res_id = res.def_id;
        let definition = database
            .get_definition_map(res_id, |def| def.clone())
            .unwrap();
        assert_eq!(definition.name, "foo");
        assert!(matches!(definition.kind, DefinitionKind::Method(_)));

        let res = resolver.resolve_method(receiver, "bar");
        let res = res.unwrap();
        let res_id = res.def_id;
        let definition = database
            .get_definition_map(res_id, |def| def.clone())
            .unwrap();
        assert_eq!(definition.name, "bar");
        assert!(matches!(definition.kind, DefinitionKind::Method(_)));

        let res = resolver.resolve_method(receiver, "baz");
        let res = res.unwrap();
        let res_id = res.def_id;
        let definition = database
            .get_definition_map(res_id, |def| def.clone())
            .unwrap();
        assert_eq!(definition.name, "baz");
        assert!(matches!(definition.kind, DefinitionKind::Method(_)));
    }

    #[test]
    fn simple_resolve_methods_generic() {
        let (database, _) = setup_database(SOURCE_1);
        let types = database
            .type_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect();

        let method_blocks = database
            .method_blocks_definitions
            .keys()
            .cloned()
            .map(|id| id.as_global(CURRENT_PACKAGE_ID))
            .collect();
        let mut resolver = MethodResolver::within_function(
            &database,
            Vec::new(),
            Vec::new(),
            method_blocks,
            types,
            Vec::new(),
        );
        let receiver = database
            .get_type_by_unresolved_name(&[], &[], "Bar")
            .unwrap();
        let receiver = Type::App {
            typ: Box::new(receiver.clone()),
            args: vec![database
                .get_type_by_unresolved_name(&[], &[], "Foo")
                .unwrap()
                .clone()],
        };
        let res = resolver.resolve_method(&receiver, "bar");
        println!("{:?}", res);
        let res = res.unwrap();
        let res_id = res.def_id;
        let definition = database
            .get_definition_map(res_id, |def| def.clone())
            .unwrap();
        println!("{:?}", definition);
        assert_eq!(definition.name, "bar");
        assert!(matches!(definition.kind, DefinitionKind::Method(_)));

        let res = resolver.resolve_method(&receiver, "foo");
        println!("{:?}", res);
        let res = res.unwrap();
        let res_id = res.def_id;
        let definition = database
            .get_definition_map(res_id, |def| def.clone())
            .unwrap();
        println!("{:?}", definition);
        assert_eq!(definition.name, "foo");
        assert!(matches!(definition.kind, DefinitionKind::Method(_)));
    }
}
