use std::collections::HashMap;

use crate::{typecheck::types::DeBruijn, v2::knowledge::DefinitionId};

use classy_blackboard as blackboard;

use super::{
    knowledge::{self, GenericConstraint, Id, InstanceInfo},
    ty::Type,
};

struct ResolvedMethod {
    def_id: Id<DefinitionId>,
}

#[derive(Debug)]
pub enum MethodResolutionError {
    ReceiverNotResolved(Type),
}

struct MethodResolver<'db> {
    database: &'db knowledge::Database,
    blackboard_database: blackboard::Database,

    constarints_in_scope: Vec<blackboard::ty::Constraint>,

    class_to_class_id: HashMap<Id<DefinitionId>, blackboard::ty::ClassRef>,
    instance_to_instance_id: HashMap<Id<DefinitionId>, blackboard::ty::InstanceRef>,
    type_to_type_id: HashMap<Id<DefinitionId>, blackboard::ty::TyRef>,
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
        let mut blackboard_database = blackboard::Database::new();
        let class_to_class_id = Self::reserve_classes(database, &mut blackboard_database, &classes);
        let type_to_type_id =
            Self::reserve_types(database, &mut blackboard_database, &visible_types);
        Self::add_classes(
            database,
            &mut blackboard_database,
            &classes,
            &class_to_class_id,
        );
        Self::add_types(
            database,
            &mut blackboard_database,
            &visible_types,
            &type_to_type_id,
            &class_to_class_id,
        );

        blackboard_database.lower_to_clauses();
        todo!(" 1. Add instances, 2. Add method blocks, 3. Translate constraints and store them");
    }

    fn reserve_classes(
        db: &knowledge::Database,
        bdb: &mut blackboard::Database,
        classes: &[Id<DefinitionId>],
    ) -> HashMap<Id<DefinitionId>, blackboard::ty::ClassRef> {
        let mut class_to_class_id = HashMap::new();
        for class in classes {
            let resolved_class = db.get_class(class.clone()).unwrap();
            let class_id = bdb.reserve_class(&resolved_class.name);
            class_to_class_id.insert(class.clone(), class_id);
        }
        class_to_class_id
    }

    fn reserve_types(
        db: &knowledge::Database,
        bdb: &mut blackboard::Database,
        types: &[Id<DefinitionId>],
    ) -> HashMap<Id<DefinitionId>, blackboard::ty::TyRef> {
        let mut type_to_type_id = HashMap::new();
        for ty in types {
            let resolved_ty = db
                .get_definition_map(ty.clone(), |def| {
                    assert!(matches!(def.kind, knowledge::DefinitionKind::Type));
                    def.name.clone()
                })
                .unwrap();
            let type_id = bdb.reserve_type_impl(&resolved_ty);
            type_to_type_id.insert(ty.clone(), type_id);
        }
        type_to_type_id
    }

    fn add_classes(
        db: &knowledge::Database,
        bdb: &mut blackboard::Database,
        classes: &[Id<DefinitionId>],
        class_to_class_id: &HashMap<Id<DefinitionId>, blackboard::ty::ClassRef>,
    ) {
        for class in classes {
            let resolved_class = db
                .get_definition_map(class.clone(), |def| def.clone())
                .unwrap();
            let type_params = resolved_class.kind.as_class().unwrap().arguments.clone();
            let class_ref = class_to_class_id.get(&class).unwrap().clone();
            let constraints =
                Self::translate_constraints(class_to_class_id, &resolved_class.constraints);
            let class_def = blackboard::database::TypeClass {
                name: resolved_class.name.clone(),
                type_params,
                constraints,
                // ! I am pretty sure this does not matter as we only resolve methods
                // ! from method blocks and instance definitions so we will never look at
                // ! the members of the class
                members: Vec::new(),
            };
            bdb.replace_class(class_ref, class_def);
        }
    }

    fn translate_constraints(
        class_to_class_id: &HashMap<Id<DefinitionId>, blackboard::ty::ClassRef>,
        constraints: &[GenericConstraint],
    ) -> Vec<blackboard::ty::Constraint> {
        let mut res = Vec::new();
        for GenericConstraint { class, args } in constraints {
            let class_ref = class_to_class_id.get(class).unwrap();
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

    fn add_types(
        db: &knowledge::Database,
        bdb: &mut blackboard::Database,
        types: &[Id<DefinitionId>],
        type_to_type_id: &HashMap<Id<DefinitionId>, blackboard::ty::TyRef>,
        class_to_class_id: &HashMap<Id<DefinitionId>, blackboard::ty::ClassRef>,
    ) {
        for ty in types {
            let resolved_ty = db
                .get_definition_map(ty.clone(), |def| def.clone())
                .unwrap();
            let type_ref = type_to_type_id.get(ty).unwrap().clone();
            let type_params = {
                let t = db.get_type(ty.clone()).unwrap();
                match t {
                    Type::Scheme { prefex, .. } => prefex.clone(),
                    _ => Vec::new(),
                }
            };
            let constraints =
                Self::translate_constraints(class_to_class_id, &resolved_ty.constraints);
            let type_def = blackboard::database::TypeImpl {
                name: resolved_ty.name.clone(),
                type_params,
                constraints,
                // ! I don't think we will ever look at the fields when resolving
                // ! methods so this is fine I think
                fields: Vec::new(),
            };
            bdb.replace_type_impl(type_ref, type_def);
        }
    }

    // For free function calls within methods the receiver should
    // be a type of `this`
    pub fn resolve_method(
        &self,
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
        todo!()
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
        todo!()
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
