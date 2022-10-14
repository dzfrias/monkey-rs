use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Null,
    ReturnVal(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::ReturnVal(obj) => write!(f, "{}", *obj),
            Self::Null => write!(f, "null"),
        }
    }
}
