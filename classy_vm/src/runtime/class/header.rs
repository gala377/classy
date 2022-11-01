use crate::mem::ptr::NonNullPtr;

use super::Class;



pub struct Header {
    pub class: NonNullPtr<Class>,
}