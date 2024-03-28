use crate::v2::knowledge::DefinitionId;

use classy_blackboard as blackboard;

use super::{
    knowledge::{self, GenericConstraint, Id},
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
}

impl<'db> MethodResolver<'db> {
    pub fn within_function(
        constraints_in_scope: Vec<GenericConstraint>,
        visible_instances: Vec<Id<DefinitionId>>,
        visible_method_blocks: Vec<Id<DefinitionId>>,
        visible_functions: Vec<Id<DefinitionId>>,
    ) -> Self {
        // create blackboard database for the given function
        // using visible instances, method blocks and functions
        todo!()
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

    fn to_blackboard_type(&self, _ty: &Type) -> blackboard::Ty {
        todo!()
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
