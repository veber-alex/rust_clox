use std::ptr::NonNull;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum ObjKind {
    OBJ_STRING,
}

use ObjKind::*;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Obj {
    pub kind: ObjKind,
}

#[repr(C)]
pub struct ObjString {
    pub obj: Obj,
    pub ptr: NonNull<str>,
}

pub fn copy_string(lexeme: &str) -> NonNull<ObjString> {
    let boxed = String::from(lexeme).into_boxed_str();
    let ptr = NonNull::new(Box::into_raw(boxed)).unwrap();

    allocate_string(ptr)
}

fn allocate_string(ptr: NonNull<str>) -> NonNull<ObjString> {
    let string: *mut ObjString = allocate_object(OBJ_STRING).cast();

    // Safety: string is a valid ObjString
    unsafe { (*string).ptr = ptr };

    NonNull::new(string).unwrap()
}

// TODO: Should this be generic?
fn allocate_object(kind: ObjKind) -> *mut Obj {
    let obj: *mut Obj = match kind {
        OBJ_STRING => Box::into_raw(Box::<ObjString>::new_uninit()).cast(),
    };

    // Safety: types are compatible due to layout
    unsafe { obj.write(Obj { kind }) };

    obj.cast()
}
