use std::ptr::NonNull;

use crate::object::{as_objstring_str, get_kind, Obj, ObjKind};

#[derive(Clone, Copy)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    Obj(NonNull<Obj>),
}

use ObjKind::*;
use Value::*;

// TODO: will a custom method that takes self will be faster?
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Number(l0), Number(r0)) => l0 == r0,
            (Boolean(l0), Boolean(r0)) => l0 == r0,
            (Nil, Nil) => true,
            // Safety: Obj is alive and valid
            (Obj(l0), Obj(r0)) => match (get_kind(l0), get_kind(r0)) {
                // Safety: l0 and r0 are valid ObjString due to kind check
                (OBJ_STRING, OBJ_STRING) => unsafe { as_objstring_str(r0) == as_objstring_str(l0) },
            },
            _ => false,
        }
    }
}

// FIXME: This should use Display as it will be user facing.
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Nil => write!(f, "Nil"),
            Number(number) => write!(f, "{number}"),
            Boolean(boolean) => write!(f, "{boolean}"),
            // Safety: Obj is alive and valid
            Obj(obj) => match get_kind(obj) {
                OBJ_STRING => {
                    // Safety: obj is a valid ObjString due to kind check
                    write!(f, "{}", unsafe { as_objstring_str(obj) })
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
