use crate::id_provider;

pub struct Session {
    id_provider: id_provider::IdProvider,
}

impl Session {
    pub fn new() -> Self {
        Self {
            id_provider: id_provider::IdProvider::new(),
        }
    }

    pub fn id_provider(&self) -> id_provider::IdProvider {
        self.id_provider.clone()
    }
}
