use std::ptr;

use crate::chunk::Chunk;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ObjPtr(*mut Obj);

impl ObjPtr {
    pub fn null() -> Self {
        Self(ptr::null_mut())
    }

    pub fn new(ptr: *mut Obj) -> Self {
        Self(ptr)
    }

    pub fn as_ptr(&self) -> *mut Obj {
        self.0
    }

    pub fn next(&self) -> Self {
        // Safety: object is not null and valid
        unsafe { (*self.0).next }
    }

    pub fn is_string(&self) -> bool {
        self.kind() == ObjKind::OBJ_STRING
    }

    pub fn as_string(&self) -> *mut ObjString {
        self.0.cast()
    }

    pub fn as_string_str<'a>(&self) -> &'a str {
        // Safety: Obj must be of type ObjString
        unsafe {
            let ptr = (*self.as_string()).ptr;
            let len = (*self.as_string()).len;
            &*(ptr::slice_from_raw_parts(ptr, len) as *const str)
        }
    }

    pub fn as_function(&self) -> *mut ObjFunction {
        self.0.cast()
    }

    pub fn kind(&self) -> ObjKind {
        // Safety: Obj is valid and alive
        unsafe { (*self.0).kind }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum ObjKind {
    OBJ_STRING,
    OBJ_FUNCTION,
}

#[repr(C)]
pub struct Obj {
    pub kind: ObjKind,
    pub next: ObjPtr,
}

#[repr(C)]
pub struct ObjFunction {
    obj: Obj,
    pub arity: i32,
    pub chunk: Chunk,
    pub name: *mut ObjString,
}

#[repr(C)]
pub struct ObjString {
    obj: Obj,
    pub ptr: *mut u8,
    pub len: usize,
    pub hash: u32,
}

pub fn hash_string(ptr: *const u8, len: usize) -> u32 {
    let mut hash: u32 = 2166136261;
    for i in 0..len {
        // Safety: ptr and len point to a valid string
        hash ^= unsafe { *ptr.add(i) as u32 };
        hash = hash.wrapping_mul(16777619);
    }

    hash
}
