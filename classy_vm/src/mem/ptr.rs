
use std::ptr::NonNull;

pub struct Ptr<T>(pub Option<NonNull<T>>);

impl<T> Ptr<T> {
    #[inline(always)]
    pub fn new(ptr: *mut T) -> Self {
        Self(NonNull::new(ptr))
    } 

    #[inline(always)]
    pub fn new_non_null(ptr: NonNull<T>) -> Self {
        Self(Some(ptr))
    }

    #[inline(always)]
    pub fn null() -> Self {
        Self(None)
    }

    pub fn is_null(&self) -> bool {
        self.0.is_none()
    }

    pub fn inner(&self) -> Option<NonNull<T>> {
        self.0
    }

    pub unsafe fn cast<U>(&self) -> Ptr<U> {
        let ptr = match self.0 {
            None => return Ptr(None),
            Some(v) => v,
        };
        assert!(
            (ptr.as_ptr() as usize) % std::mem::align_of::<U>() == 0,
            "cannot cast pointer to type {} because the aligment is mismatched",
            std::any::type_name::<U>()
        );
        Ptr(Some(ptr.cast()))
    }
}

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Copy for Ptr<T> {}

pub struct NonNullPtr<T>(pub NonNull<T>);

impl<T> NonNullPtr<T> {

    #[inline(always)]
    pub fn new(ptr: NonNull<T>) -> Self {
        Self(ptr)
    } 

    pub fn inner(&self) -> NonNull<T> {
        self.0
    }

    pub unsafe fn cast<U>(&self) -> NonNullPtr<U> {
        let ptr = self.0;
        assert!(
            (ptr.as_ptr() as usize) % std::mem::align_of::<U>() == 0,
            "cannot cast pointer to type {} because the aligment is mismatched",
            std::any::type_name::<U>()
        );
        NonNullPtr(ptr.cast())
    }
}

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Copy for Ptr<T> {}


pub type ErasedPtr = Ptr<()>;

pub type ErasedNonNull = NonNullPtr<()>;