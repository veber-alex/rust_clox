use std::{
    alloc::{handle_alloc_error, Layout},
    ptr::NonNull,
};

use crate::{
    object::{ObjKind, ObjPtr},
    vm::VM,
};

pub fn allocate_memory<T>(size: usize) -> *mut T {
    if size == 0 {
        return NonNull::dangling().as_ptr();
    }

    let layout = Layout::array::<T>(size).unwrap();

    // Safety: Layout size is not 0, we checked above
    let ptr = unsafe { std::alloc::alloc(layout) };
    if ptr.is_null() {
        handle_alloc_error(layout);
    }

    ptr.cast()
}

pub fn free_objects(vm: &mut VM) {
    let mut obj = vm.objects;
    while !obj.as_ptr().is_null() {
        let next = obj.next();
        free_object(obj);
        obj = next;
    }
}

fn free_object(obj: ObjPtr) {
    match obj.kind() {
        ObjKind::OBJ_STRING => {
            // Safety: obj is a valid ObjString due to kind check
            unsafe {
                let string = obj.as_string();
                let string_str = (*string).ptr;
                if (&*string_str).len() != 0 {
                    free_memory(string_str);
                }
                free_memory(string);
            }
        }
    }
}

// Safety: pointer must have been allocated with allocate_memory() and not null or dangling
unsafe fn free_memory<T: ?Sized>(ptr: *mut T) {
    // Safety: ptr is valid for deallcation because function contract
    unsafe {
        let layout = Layout::for_value(&*ptr);
        std::alloc::dealloc(ptr.cast(), layout);
    }
}
