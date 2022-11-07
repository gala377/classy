use crate::{
    mem::ptr::{ErasedNonNull, ErasedPtr, NonNullPtr},
    runtime::class::header::Header,
    runtime::trace::Tracer,
};

use super::Class;

#[repr(transparent)]
pub struct Array<T> {
    ptr: NonNullPtr<T>,
}

impl<T> Array<T> {
    pub unsafe fn from_ptr(arr: ErasedNonNull) -> Self {
        Self { ptr: arr.cast() }
    }

    pub fn len(&self) -> usize {
        unsafe { (*self.ptr.header().as_ptr()).data }
    }

    pub unsafe fn inner(&self) -> NonNullPtr<T> {
        self.ptr.clone()
    }

    pub fn class(&self) -> NonNullPtr<Class> {
        self.ptr.class()
    }

    pub fn element_class(&self) -> NonNullPtr<Class> {
        unsafe { 
            let el_type = (*self.class().get()).array_element_type();
            NonNullPtr::from_ptr(el_type)
        }
    }
}

impl<T> std::ops::Index<usize> for Array<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.len() {
            panic!(
                "Indexing past array bounds. Index {}. Size {}",
                index,
                self.len()
            );
        }
        unsafe { &*self.ptr.get().offset(index as isize) }
    }
}

impl<T> std::ops::IndexMut<usize> for Array<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index >= self.len() {
            panic!(
                "Indexing past array bounds. Index {}. Size {}",
                index,
                self.len()
            );
        }
        unsafe { &mut *self.ptr.get().offset(index as isize) }
    }
}

pub unsafe fn trace(arr: *mut (), tracer: &mut dyn Tracer) {
    let arr = ErasedNonNull::new_unchecked(arr);
    let cls = arr.class();
    debug_assert!((*cls.get()).kind.is_array(), "has to be");
    // trace class
    let cls_forward = tracer.trace_nonnull_pointer(cls.erase());
    let header = arr.header();
    (*header.as_ptr()).class = NonNullPtr::new_unchecked(cls_forward as *mut Class);
    // new location so we need to refresh.
    // future gala377: well not really, this memory is not going anywhere
    // until we finish copying... I think.
    let cls = arr.class();

    // trace elements
    let el_type = (*cls.get()).array_element_type();
    let size = (*arr.header().as_ptr()).data;
    let el_size = (*cls.get()).array_element_size();
    let el_type_ptr = el_type.inner().unwrap();
    if (*el_type_ptr.as_ptr()).is_reference_class() {
        for i in 0..size {
            let offset = i * el_size;
            let pointer_to_element = (arr.get() as *mut u8).add(offset) as *mut ErasedPtr;
            let element = *pointer_to_element;
            let forward = tracer.trace_pointer(element);
            pointer_to_element.write(ErasedPtr::new(forward));
        }
    } else {
        let el_trace = (*el_type_ptr.as_ptr()).trace;
        for i in 0..size {
            let offset = i * el_size;
            let obj = (arr.get() as *mut u8).add(offset) as *mut ();
            el_trace(obj, tracer);
        }
    };
}

pub unsafe fn actual_size(arr: *const ()) -> usize {
    let header = (arr as *const Header).sub(1);
    let cls = (*header).class.clone();
    let size = (*header).data;
    assert!((*cls.get()).kind.is_array(), "has to be");
    let el_size = (*cls.get()).array_element_size();
    el_size * size
}

// pub fn array_class_for(heap: &mut Heap, class: TypedPtr<Class>) -> Class {
//     let name = format!("[]{}", unsafe {
//         let name = (*class.get()).name.get();
//         (*name).as_rust_str()
//     });
//     let instance_align = unsafe { (*class.get()).instance_align };
//     Class {
//         name: class::string::allocate(heap, &name),
//         drop: None,
//         trace: array::trace,
//         instance_size: 0,
//         instance_align,
//         actual_instance_size: Some(array::actual_size),
//         kind: Kind::Array {
//             element_type: class,
//         },
//     }
// }
