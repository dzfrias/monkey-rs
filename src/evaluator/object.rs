use super::env::Env;
use crate::ast::*;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use thiserror::Error;

pub type BuiltinFunc = fn(Vec<Object>) -> EvalResult;

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
    Builtin {
        function: BuiltinFunc,
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
            Self::Builtin { .. } => write!(f, "[builtin function]"),
        }
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum RuntimeError {
    #[error("cannot perform `{op}` on `{right}`")]
    InvalidPrefixOperand { op: PrefixOp, right: Object },
    #[error("cannot perform `{op}` between `{right}` and `{left}`")]
    InvalidInfixOperands {
        op: InfixOp,
        left: String,
        right: String,
    },
    #[error("integer overflow occured in the expression: `{x} {op} {y}`")]
    IntegerOverflow { op: InfixOp, x: i64, y: i64 },
    #[error("division by zero occured in the expression: `{x} {op} {y}`")]
    DivisionByZero { op: InfixOp, x: i64, y: i64 },
    #[error("variable not found: `{name}`")]
    VariableNotFound { name: String },
    #[error("not enough arguments to function call: got `{got}`, want `{expected}`")]
    NotEnoughArguments { expected: i32, got: i32 },
    #[error("not a function: {0}")]
    NotAFunction(Object),
    #[error("wrong argument type: got `{got}` want `{want}`")]
    WrongArgType { got: String, want: String },
}

pub type EvalResult = Result<Object, RuntimeError>;
