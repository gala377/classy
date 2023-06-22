use std::ptr::NonNull;

use crate::{
    mem::ptr::{ErasedNonNull, NonNullPtr},
    runtime::class::Class,
};

pub enum Flags {
    Forwards = 1,
    PermamentHeap = 1 << 1,
}

impl Flags {
    pub const fn empty() -> usize {
        0
    }
}

#[repr(C, align(8))]
pub struct Header {
    /// A class of this instance, a class of a class is class itself
    pub class: NonNullPtr<Class>,
    /// Flags associated with this object, like forwarding, permament heap etc.
    pub flags: usize,
    /// Data differs between different objects.
    /// For arrays it is a number of elements.
    /// For instances it is a number of fields.
    /// For ADTs it is a tag.
    pub data: usize,
}

impl Header {
    pub fn for_class(meta_class: NonNullPtr<Class>, fields_count: usize) -> Self {
        Self {
            class: meta_class,
            flags: Flags::empty(),
            data: fields_count,
        }
    }

    pub fn for_array(class: NonNullPtr<Class>, size: usize) -> Self {
        Self {
            class,
            flags: Flags::empty(),
            data: size,
        }
    }

    pub fn for_instance(class: NonNullPtr<Class>) -> Self {
        Self {
            class,
            flags: Flags::empty(),
            data: 0,
        }
    }

    pub unsafe fn data_from_nonnull<T>(ptr: NonNullPtr<T>) -> usize {
        (*ptr.get().cast::<Header>().sub(1)).data
    }

    pub fn permament_allocation(&self) -> bool {
        debug_assert!(self.forward_address().is_none());
        (self.flags & (Flags::PermamentHeap as usize)) != 0
    }

    pub fn set_permament_allocation(&mut self) {
        debug_assert!(
            self.forward_address().is_none(),
            "Trying to access flag on a forward address"
        );
        self.flags = self.flags | (Flags::PermamentHeap as usize)
    }

    pub fn forward_address(&self) -> Option<usize> {
        if (self.flags & (Flags::Forwards as usize)) != 0 {
            Some(self.flags & (!(Flags::Forwards as usize)))
        } else {
            None
        }
    }

    // Sets forward address on a header that can be retrived with
    // `forward_address` function. Forward address overrides any other
    // flags that were there.
    pub fn set_forward_address(&mut self, address: usize) {
        debug_assert_eq!(
            address & (Flags::Forwards as usize),
            0,
            "flagging bit of forward address has to be zero"
        );
        self.flags = address | (Flags::Forwards as usize);
    }

    pub fn class(header: NonNull<Header>) -> NonNullPtr<Class> {
        unsafe { (*header.as_ptr()).class }
    }

    pub fn object_ptr(header: NonNull<Header>) -> ErasedNonNull {
        unsafe { ErasedNonNull::new_unchecked(header.as_ptr().add(1) as *mut _) }
    }
}
