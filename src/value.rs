// FIXME: This PartialEq will not be good later
#[derive(Clone, Copy, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Number(number) => write!(f, "{number}"),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        !matches!(value, Value::Nil | Value::Boolean(false))
    }
}
