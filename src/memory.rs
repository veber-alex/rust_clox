use std::{
    alloc::{handle_alloc_error, Layout},
    ptr::{self, NonNull},
};

use crate::{
    object::{ObjKind, ObjPtr},
    vm::VM,
};

pub fn grow_capacity(capacity: usize) -> usize {
    if capacity < 8 {
        8
    } else {
        capacity * 2
    }
}

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

pub fn reallocate_memory<T>(ptr: *mut T, old_size: usize, new_size: usize) -> *mut T {
    let layout = Layout::array::<T>(old_size).unwrap();

    if new_size == 0 {
        unsafe {
            std::alloc::dealloc(ptr.cast(), layout);
        }
        return ptr::null_mut();
    }

    // Safety: ptr was allocated with allocate memory and new_size is not 0
    let ptr = unsafe { std::alloc::realloc(ptr as *mut u8, layout, new_size) };

    if ptr.is_null() {
        handle_alloc_error(layout);
    }

    ptr.cast()
}

pub fn free_memory<T>(ptr: *mut T) {
    reallocate_memory(ptr, 1, 0);
}

pub fn free_array_memory<T>(ptr: *mut T, size: usize) {
    // Safety: all raw pointers are created with allocate_memory
    if ptr.is_null() || size == 0 {
        return;
    }
    reallocate_memory(ptr, size, 0);
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
                let len = (*string).len;
                free_array_memory((*string).ptr, len);
                free_memory(string);
            }
        }
    }
}
