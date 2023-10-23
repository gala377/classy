use std::collections::HashMap;
use std::fmt::Write;

use crate::clauses::{Constraint, Instance, Ty, TyRef, TypeImpl};
use crate::slg::Substitution;

pub struct Database {
    type_impls: Vec<TypeImpl>,
    instances: Vec<Instance>,
    names_to_ty: HashMap<String, usize>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            type_impls: Vec::new(),
            instances: Vec::new(),
            names_to_ty: HashMap::new(),
        }
    }

    pub fn make_readable(&self, ty: Ty, res: &mut impl Write) {
        match ty {
            Ty::Ref(index) => {
                let t_impl = &self.type_impls[index.0];
                write!(res, "{}", t_impl.name).unwrap();
                if t_impl.type_params.len() > 0 {
                    write!(res, "<").unwrap();
                    for (i, param) in t_impl.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(res, ", ").unwrap();
                        }
                        write!(res, "{}", param).unwrap();
                    }
                    write!(res, ">").unwrap();
                }
            }
            Ty::Array(inner) => {
                let mut inner_str = String::new();
                self.make_readable(*inner, &mut inner_str);
                write!(res, "[{}]", inner_str).unwrap();
            }
            Ty::Tuple(inner) => {
                write!(res, "(").unwrap();
                for (i, inner) in inner.iter().enumerate() {
                    if i > 0 {
                        write!(res, ", ").unwrap();
                    }
                    self.make_readable(inner.clone(), res);
                }
                write!(res, ")").unwrap();
            }
            Ty::Fn(args, ret) => {
                write!(res, "(").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(res, ", ").unwrap();
                    }
                    self.make_readable(arg.clone(), res);
                }
                write!(res, ") -> ").unwrap();
                self.make_readable(*ret, res);
            }
            Ty::Generic(index) => {
                write!(res, "g{}", index).unwrap();
            }
            Ty::App(head, args) => {
                let t_impl = &self.type_impls[head.0];
                write!(res, "{}", t_impl.name).unwrap();
                write!(res, "<").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(res, ", ").unwrap();
                    }
                    self.make_readable(arg.clone(), res);
                }
                write!(res, ">").unwrap();
            }
            Ty::UnBound(index) => {
                write!(res, "∃{}", index).unwrap();
            }
            Ty::Forall(index) => {
                write!(res, "∀{}", index).unwrap();
            }
        }
    }

    pub fn add_type_impl(&mut self, type_impl: TypeImpl) -> TyRef {
        self.names_to_ty
            .insert(type_impl.name.clone(), self.type_impls.len());
        self.type_impls.push(type_impl);
        TyRef(self.type_impls.len() - 1)
    }

    pub fn add_struct(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
        members: Vec<(String, Ty)>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: false,
            members,
        };
        self.add_type_impl(type_impl)
    }

    pub fn add_type_class(
        &mut self,
        name: String,
        variables: Vec<String>,
        bounds: Vec<Constraint>,
        members: Vec<(String, Ty)>,
    ) -> TyRef {
        let type_impl = TypeImpl {
            name: name.to_string(),
            type_params: variables,
            constraints: bounds,
            is_class: true,
            members,
        };
        self.add_type_impl(type_impl)
    }

    pub fn add_instance_for(
        &mut self,
        type_class: TyRef,
        type_params: Vec<String>,
        constraints: Vec<Constraint>,
        args: Vec<Ty>,
    ) {
        let instance = Instance {
            type_class,

            type_params,
            args,
            constraints,
        };
        self.add_instance(instance);
    }

    pub fn add_instance(&mut self, instance: Instance) {
        self.instances.push(instance);
    }

    pub fn typeref_from_name(&self, name: &str) -> Option<TyRef> {
        self.names_to_ty.get(name).map(|i| TyRef(*i))
    }

    /// Union given type with all instances and type defnitions and returns
    /// resulting exclauses alongside a substitution that has been made in order
    /// to make the union possible.
    pub fn find_matching(&self, ty: &Ty) -> Vec<(ExClause, Substitution)> {
        let mut result = Vec::new();
        // match instances
        for instance in &self.instances {
            let union_with = Ty::App(instance.type_class.clone(), instance.args.clone());
            let mut subst = HashMap::new();
            if union(ty.clone(), union_with, &mut subst).is_some() {
                let constraints = instance
                    .constraints
                    .clone()
                    .into_iter()
                    .map(|c| match c {
                        Constraint::Eq(_, _) => unimplemented!(),
                        Constraint::Class(head, args) => Ty::App(head, args),
                    })
                    .collect();
                let ex_clause = ExClause {
                    head: ty.clone(),
                    constraints,
                };
                result.push((ex_clause, subst.into()));
            }
        }
        // match types and classes
        for type_impl in &self.type_impls {
            // skip type class definitions without constraint
            if type_impl.is_class && type_impl.constraints.is_empty() {
                continue;
            }
            let type_ref = self.typeref_from_name(&type_impl.name).unwrap();
            let union_with = if type_impl.type_params.len() == 0 {
                Ty::Ref(type_ref)
            } else {
                let mut args = Vec::new();
                for i in 0..type_impl.type_params.len() {
                    args.push(Ty::Generic(i));
                }
                Ty::App(type_ref, args)
            };
            let mut subst = HashMap::new();
            if union(ty.clone(), union_with, &mut subst).is_some() {
                let constraints = type_impl
                    .constraints
                    .clone()
                    .into_iter()
                    .map(|c| match c {
                        Constraint::Eq(_, _) => unimplemented!(),
                        Constraint::Class(head, args) => Ty::App(head, args),
                    })
                    .collect();
                let ex_clause = ExClause {
                    head: ty.clone(),
                    constraints,
                };
                result.push((ex_clause, subst.into()));
            }
        }
        result
    }
}

/// Union 2 types together and record all substitutions made along the way in
/// passed map.
///
/// Note that substitution rules might be unclear. For example, substitution is.
/// only recorder when an unboud EXISTS variable (?0) is matched agains other
/// type. What it means that generic variables do not generate substitutions.
fn union(t1: Ty, t2: Ty, substituted: &mut HashMap<usize, Ty>) -> Option<()> {
    match (t1, t2) {
        (Ty::UnBound(index), t2) => match substituted.get(&index) {
            Some(old) if *old != t2 => None,
            Some(_) => Some(()),
            None => {
                substituted.insert(index, t2);
                Some(())
            }
        },
        // TODO: we need to check that generics are substituted consistantly
        (Ty::Forall(_), Ty::Generic(_)) => Some(()),
        (Ty::App(h1, as1), Ty::App(h2, as2)) if h1 == h2 => {
            if as1.len() != as2.len() {
                return None;
            }
            for (a1, a2) in as1.into_iter().zip(as2.into_iter()) {
                union(a1, a2, substituted)?;
            }
            Some(())
        }
        (Ty::Array(t1), Ty::Array(t2)) => union(*t1, *t2, substituted),
        (Ty::Tuple(ts1), Ty::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                return None;
            }
            for (t1, t2) in ts1.into_iter().zip(ts2.into_iter()) {
                union(t1, t2, substituted)?;
            }
            Some(())
        }
        (Ty::Ref(t1), Ty::Ref(t2)) if t1 == t2 => Some(()),
        (Ty::Fn(args1, ret1), Ty::Fn(args2, ret2)) => {
            if args1.len() != args2.len() {
                return None;
            }
            for (t1, t2) in args1.into_iter().zip(args2.into_iter()) {
                union(t1, t2, substituted)?;
            }
            union(*ret1, *ret2, substituted)
        }
        (Ty::Generic(_), _) => Some(()),
        (_, Ty::Generic(_)) => Some(()),
        (_, Ty::UnBound(_)) => panic!("Unexpected unbound variable"),
        (_, _) => None,
    }
}

#[derive(Debug, Clone)]
pub struct ExClause {
    pub head: Ty,
    pub constraints: Vec<Ty>,
}
