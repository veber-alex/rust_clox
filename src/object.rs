use std::ptr;

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

    // Safety: Obj must be of type ObjString
    pub unsafe fn as_string(&self) -> *mut ObjString {
        self.0.cast()
    }

    // Safety: Obj must be of type ObjString
    pub unsafe fn as_string_str<'a>(&self) -> &'a str {
        // Safety: Obj must be of type ObjString
        unsafe { &*(*self.as_string()).ptr }
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
}

#[repr(C)]
pub struct Obj {
    pub kind: ObjKind,
    pub next: ObjPtr,
}

#[repr(C)]
pub struct ObjString {
    obj: Obj,
    pub ptr: *mut str,
}
