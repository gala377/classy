use std::{cell::Cell, rc::Rc};

pub type UniqueId = usize;

#[derive(Clone)]
pub struct IdProvider {
    id: Rc<Cell<usize>>,
}

impl IdProvider {
    pub fn new() -> Self {
        Self {
            id: Rc::new(Cell::new(1)),
        }
    }

    pub fn next(&self) -> UniqueId {
        let id = self.id.get();
        self.id.set(id + 1);
        id
    }
}
