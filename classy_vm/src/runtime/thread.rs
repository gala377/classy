use std::sync::{Arc, Mutex};

use crate::mem::{self, ptr::Ptr};
use crate::runtime::tlab::Tlab;

pub struct Thread {
    tlab: Tlab,
}

impl Thread {
    pub fn new(
        allocator: Arc<Mutex<mem::allocator::Allocator>>,
        initial_tlab_free_size: usize,
    ) -> Self {
        let id = std::thread::current().id();
        Thread {
            tlab: Tlab::new(id, allocator, initial_tlab_free_size),
        }
    }

    pub fn alloc<T>(&mut self) -> Ptr<T> {
        self.tlab.alloc()
    }

}
