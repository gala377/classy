pub struct DiscardValues;

impl<U> FromIterator<U> for DiscardValues {
    fn from_iter<T: IntoIterator<Item = U>>(iter: T) -> Self {
        Self
    }
}
