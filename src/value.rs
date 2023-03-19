use std::fmt;

use crate::object::{ObjFunction, ObjKind, ObjPtr};

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
            (Obj(l0), Obj(r0)) => l0.as_ptr() == r0.as_ptr(),
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Nil => write!(f, "Nil"),
            Number(number) => write!(f, "{number}"),
            Boolean(boolean) => write!(f, "{boolean}"),
            Obj(obj) => match obj.kind() {
                OBJ_STRING => write!(f, "{}", obj.as_string_str()),
                OBJ_FUNCTION => print_function(obj.as_function(), f),
                OBJ_CLOSURE => unsafe { print_function((*obj.as_closure()).function, f) },
                OBJ_NATIVE => write!(f, "<native fn>"),
            },
        }
    }
}

fn print_function(
    function: *mut ObjFunction,
    f: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    unsafe {
        let name = (*function).name;
        if name.is_null() {
            return write!(f, "<script>");
        }
        let name = ObjPtr::new(name.cast()).as_string_str();
        write!(f, "<fn {}>", name)
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        !matches!(value, Nil | Boolean(false))
    }
}
