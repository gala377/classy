/// Same as types_eq, only here temporaraly to make refactoring easier.
pub fn types_strictly_eq(db: &Database, t1: &Type, t2: &Type) -> Result<bool, QueryError> {
    match (t1, t2) {
        (Type::Alias(f1), Type::Alias(f2)) if f1 == f2 => {
            return Ok(true);
        }
        _ => {}
    }
    let (t1, t2) = match (t1, t2) {
        (&Type::Alias(for_type_1), &Type::Alias(for_type_2)) if for_type_1 == for_type_2 => {
            return Ok(true);
        }
        (&Type::Alias(for_type_1), &Type::Alias(for_type_2)) => {
            let t1 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type_1))?;
            let t2 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type_2))?;
            (t1, t2)
        }
        (&Type::Alias(for_type), t2) => {
            let t1 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type))?;
            (t1, t2)
        }
        (t1, &Type::Alias(for_type)) => {
            let t2 = db.resolve_alias_ref(crate::knowledge::TypeId(for_type))?;
            (t1, t2)
        }
        (t1, t2) => (t1, t2),
    };
    Ok(match (t1, t2) {
        (Type::Int, Type::Int) => true,
        (Type::UInt, Type::UInt) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::String, Type::String) => true,
        (Type::Float, Type::Float) => true,
        (Type::Unit, Type::Unit) => true,
        (Type::ToInfere, Type::ToInfere) => false,
        (Type::Divergent, _) => true,
        (_, Type::Divergent) => true,
        (Type::Struct { def: def1, .. }, Type::Struct { def: def2, .. }) => def1 == def2,
        (Type::Tuple(fields1), Type::Tuple(fields2)) => {
            if fields1.len() != fields2.len() {
                return Ok(false);
            }
            fields1
                .iter()
                .zip(fields2)
                .map(|(t1, t2)| types_strictly_eq(db, t1, t2))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .all(|eq| eq)
        }
        (
            Type::Function {
                args: args1,
                ret: ret1,
            },
            Type::Function {
                args: args2,
                ret: ret2,
            },
        ) => {
            if args1.len() != args2.len() {
                return Ok(false);
            }
            types_strictly_eq(db, ret1, ret2)?
                && args1
                    .iter()
                    .zip(args2)
                    .map(|(t1, t2)| types_strictly_eq(db, t1, t2))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .all(|eq| eq)
        }
        (Type::Generic(d1, n1), Type::Generic(d2, n2)) => d1 == d2 && n1 == n2,
        (
            Type::Scheme {
                prefex: prefex_1,
                typ: typ_1,
            },
            Type::Scheme {
                prefex: prefex_2,
                typ: typ_2,
            },
        ) => {
            // This could be incorrect, to test for scheme equivalence we need to
            // replace generics in typ_2 using generics in typ_1 and then compare this
            prefex_1.len() == prefex_2.len() && types_strictly_eq(db, typ_1, typ_2)?
        }
        (Type::App { typ: t1, args: a1 }, Type::App { typ: t2, args: a2 }) => {
            types_strictly_eq(db, t1, t2)?
                && a1
                    .iter()
                    .zip(a2)
                    .map(|(t1, t2)| types_strictly_eq(db, t1, t2))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .all(|eq| eq)
        }
        _ => false,
    })
}
