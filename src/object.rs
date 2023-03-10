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

pub fn get_kind(obj_ptr: NonNull<Obj>) -> ObjKind {
    // Safety: Obj is valid and alive
    unsafe { (*obj_ptr.as_ptr()).kind }
}

// Safety: Obj must be of type ObjString
pub unsafe fn as_objstring_str<'a>(obj_ptr: NonNull<Obj>) -> &'a str {
    let string: *mut ObjString = obj_ptr.as_ptr().cast();

    // Safety: string is ObjString and valid and alive
    unsafe { &*(*string).ptr.as_ptr() }
}

pub fn copy_string(lexeme: &str) -> NonNull<ObjString> {
    // FIXME: This is probably suboptimal perf
    let boxed = String::from(lexeme).into_boxed_str();
    let ptr = NonNull::new(Box::into_raw(boxed)).unwrap();

    allocate_string(ptr)
}

pub fn take_string(ptr: *mut u8, len: usize) -> NonNull<ObjString> {
    let ptr = NonNull::new(std::ptr::slice_from_raw_parts_mut(ptr, len) as *mut str).unwrap();
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

    obj
}
