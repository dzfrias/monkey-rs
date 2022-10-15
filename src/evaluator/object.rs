use super::env::Env;
use crate::ast::*;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Null,
    ReturnVal(Box<Object>),
    Function {
        params: Vec<Identifier>,
        body: Block,
        env: Rc<RefCell<Env>>,
    },
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::ReturnVal(obj) => write!(f, "{}", *obj),
            Self::Null => write!(f, "null"),
            Self::String(s) => write!(f, "{s}"),
            Self::Function { params, body, .. } => {
                let joined = params
                    .iter()
                    .map(|ident| ident.to_string() + ", ")
                    .collect::<String>();
                write!(
                    f,
                    "fn({}) {{\n {body} \n}}",
                    joined
                        .strip_suffix(", ")
                        .expect("Should always have a trailing ', '")
                )
            }
        }
    }
}
