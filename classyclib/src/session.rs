use crate::id_provider::{self, UniqueId};

/// Just a wrapper for IdProvider.
/// We wrap it here to type-code where we want to use new instance
/// and when we want to use shared instance.
#[derive(Clone)]
pub struct SharedIdProvider {
    id_provider: id_provider::IdProvider,
}

impl SharedIdProvider {
    /// This function is private on purpose. Only Session should be
    /// able to create an instance of this struct.
    fn new() -> Self {
        Self {
            id_provider: id_provider::IdProvider::new(),
        }
    }

    pub fn next(&self) -> UniqueId {
        self.id_provider.next()
    }

    pub fn last(&self) -> UniqueId {
        self.id_provider.last()
    }
}

pub struct Session {
    id_provider: SharedIdProvider,
}

impl Session {
    pub fn new() -> Self {
        Self {
            id_provider: SharedIdProvider::new(),
        }
    }

    pub fn id_provider(&self) -> SharedIdProvider {
        self.id_provider.clone()
    }
}
