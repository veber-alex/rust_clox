use std::ptr::NonNull;

use crate::object::{Obj, ObjKind, ObjString};

#[derive(Clone, Copy)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    Obj(NonNull<Obj>),
}

use ObjKind::*;
use Value::*;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number(l0), Number(r0)) => l0 == r0,
            (Boolean(l0), Boolean(r0)) => l0 == r0,
            (Nil, Nil) => true,
            // Safety: Obj is alive and valid
            (Obj(l0), Obj(r0)) => match unsafe { ((*l0.as_ptr()).kind, (*r0.as_ptr()).kind) } {
                (OBJ_STRING, OBJ_STRING) => {
                    let l1: *mut ObjString = l0.as_ptr().cast();
                    let r1: *mut ObjString = r0.as_ptr().cast();

                    unsafe { &*(*l1).ptr.as_ptr() == &*(*r1).ptr.as_ptr() }
                }
            },
            _ => false,
        }
    }
}

// FIXME: This should use Display as it will be user facing.
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil => write!(f, "Nil"),
            Number(number) => write!(f, "{number}"),
            Boolean(boolean) => write!(f, "{boolean}"),
            // Safety: Obj is alive and valid
            Obj(obj) => match unsafe { (*obj.as_ptr()).kind } {
                OBJ_STRING => {
                    let string: *mut ObjString = obj.as_ptr().cast();
                    // Safety: obj is a valid ObjString due to kind check
                    write!(f, "{}", unsafe { &*(*string).ptr.as_ptr() })
                }
            },
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        !matches!(value, Nil | Boolean(false))
    }
}
