pub struct DiscardValues;

impl<U> FromIterator<U> for DiscardValues {
    fn from_iter<T: IntoIterator<Item = U>>(_: T) -> Self {
        Self
    }
}
