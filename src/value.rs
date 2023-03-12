use crate::object::{ObjKind, ObjPtr};

#[derive(Clone, Copy)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    Obj(ObjPtr),
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
            (Obj(l0), Obj(r0)) => match (l0.kind(), r0.kind()) {
                // Safety: l0 and r0 are valid ObjString due to kind check
                (OBJ_STRING, OBJ_STRING) => unsafe { l0.as_string() == r0.as_string() },
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
            Obj(obj) => match obj.kind() {
                OBJ_STRING => {
                    // Safety: obj is a valid ObjString due to kind check
                    write!(f, "{}", unsafe { obj.as_string_str() })
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
