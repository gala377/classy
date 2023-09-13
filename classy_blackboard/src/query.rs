use crate::clauses::Ty;

pub struct TypeHolds(pub Ty);

pub struct Exists(pub Vec<Ty>, pub Ty);

impl Exists {
    pub fn new(unbound: Vec<Ty>, ty: Ty) -> Self {
        for (i, t) in unbound.iter().enumerate() {
            match t {
                Ty::UnBound(index) if *index != i => {
                    panic!("Expected unbound type variable from 0 to n in increasing order")
                }
                Ty::UnBound(_) => (),
                _ => panic!("Expected unbound type variable"),
            }
        }
        let mut seen = vec![false; unbound.len()];
        for t in ty.unbound() {
            if t >= unbound.len() {
                panic!("Expected unbound type variable from 0 to n in increasing order")
            }
            seen[t] = true;
        }
        assert!(
            seen.iter().all(|b| *b),
            "not all unbound variables are used"
        );
        Self { 0: unbound, 1: ty }
    }
}
