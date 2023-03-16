use std::{
    alloc::{handle_alloc_error, Layout},
    mem,
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
    reallocate_memory(ptr::null_mut(), 0, size)
}

pub fn reallocate_memory<T>(ptr: *mut T, old_size: usize, new_size: usize) -> *mut T {
    if old_size == 0 {
        if new_size == 0 {
            return NonNull::dangling().as_ptr();
        }

        let layout = Layout::array::<T>(new_size).unwrap();

        // Safety: Layout size is not 0, we checked above
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            handle_alloc_error(layout);
        }

        return ptr.cast();
    }

    let layout = unsafe {
        Layout::from_size_align_unchecked(mem::size_of::<T>() * old_size, mem::align_of::<T>())
    };

    if new_size == 0 {
        unsafe {
            std::alloc::dealloc(ptr.cast(), layout);
        }
        return ptr::null_mut();
    }

    // Safety: ptr was allocated with allocate memory and new_size is not 0
    let ptr =
        unsafe { std::alloc::realloc(ptr as *mut u8, layout, new_size * mem::size_of::<T>()) };

    if ptr.is_null() {
        handle_alloc_error(layout);
    }

    ptr.cast()
}

pub fn free_memory<T>(ptr: *mut T) {
    reallocate_memory(ptr, 1, 0);
}

pub fn free_array_memory<T>(ptr: *mut T, size: usize) {
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
        ObjKind::OBJ_FUNCTION => {
            let function = obj.as_function();
            unsafe { (*function).chunk.free_chunk() }
            free_memory(function);
        }
        ObjKind::OBJ_NATIVE => {
            free_memory(obj.as_native());
        }
    }
}

pub struct Vector<T> {
    ptr: *mut T,
    capacity: usize,
    len: usize,
}

impl<T: Copy> Vector<T> {
    pub fn new() -> Self {
        Self {
            ptr: ptr::null_mut(),
            capacity: 0,
            len: 0,
        }
    }

    pub fn push(&mut self, value: T) {
        if self.capacity == self.len {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(old_capacity);
            self.ptr = reallocate_memory(self.ptr, old_capacity, self.capacity);
        }

        unsafe { *self.ptr.add(self.len) = value };
        self.len += 1;
    }

    pub fn get(&self, offset: usize) -> T {
        unsafe { *self.ptr.add(offset) }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn free(&mut self) {
        free_array_memory(self.ptr, self.capacity);
        *self = Self::new();
    }
}
