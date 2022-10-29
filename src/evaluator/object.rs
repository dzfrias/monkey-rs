use super::env::Env;
use crate::ast::*;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;
use thiserror::Error;

macro_rules! type_signature {
    ($($t:ident)|+) => {
        {
            let mut hash_set = std::collections::HashSet::new();
            $(hash_set.insert(Type::$t);)*
            TypeSignature(hash_set)
        }
    };
}
pub(crate) use type_signature;

pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);
pub const NULL: Object = Object::Null;

pub type BuiltinFunc = fn(Vec<Object>) -> EvalResult;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Object>),
    HashMap(HashMap<Object, Object>),
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

impl Object {
    pub fn monkey_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Bool(_) => Type::Bool,
            Self::String(_) => Type::String,
            Self::Array(_) => Type::Array,
            Self::HashMap(_) => Type::HashMap,
            Self::Null => Type::Null,
            Self::Function { .. } => Type::Function,
            Self::Builtin { .. } => Type::Builtin,
            Self::ReturnVal(val) => val.monkey_type(),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::ReturnVal(obj) => write!(f, "{}", *obj),
            Self::Null => write!(f, "null"),
            Self::String(s) => write!(f, "\"{s}\""),
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
            Self::Array(arr) => {
                let joined = arr
                    .iter()
                    .map(|elem| elem.to_string() + ", ")
                    .collect::<String>();
                write!(
                    f,
                    "[{}]",
                    joined
                        .strip_suffix(", ")
                        .expect("Should always have trailing ', '")
                )
            }
            Self::HashMap(hashmap) => {
                let pairs = hashmap
                    .iter()
                    .map(|(key, val)| key.to_string() + ": " + &val.to_string())
                    .collect::<Vec<String>>();
                write!(f, "{{{}}}", pairs.join(", "))
            }
        }
    }
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(i) => i.hash(state),
            Self::Bool(b) => b.hash(state),
            Self::String(s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Int,
    Bool,
    String,
    Array,
    HashMap,
    Null,
    Function,
    Builtin,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Array => write!(f, "Array"),
            Self::HashMap => write!(f, "Hashmap"),
            Self::Null => write!(f, "<null>"),
            Self::Function => write!(f, "Function"),
            Self::Builtin => write!(f, "[builtin]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSignature(pub HashSet<Type>);

impl fmt::Display for TypeSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let joined = self
            .0
            .iter()
            .map(|elem| elem.to_string() + " | ")
            .collect::<String>();
        write!(
            f,
            "{}",
            joined
                .strip_suffix(" | ")
                .expect("Should always have trailing ' | '")
        )
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum RuntimeError {
    #[error("cannot perform `{op}` on `{right}`")]
    InvalidPrefixOperand { op: PrefixOp, right: Type },
    #[error("cannot perform `{op}` between `{right}` and `{left}`")]
    InvalidInfixOperands {
        op: InfixOp,
        left: Type,
        right: Type,
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
    WrongArgType { got: Type, want: TypeSignature },
    #[error("index operator not supported between `{left}` and `{index}`")]
    IndexOperatorNotSupported { left: Type, index: Type },
    #[error("invalid index: {idx}")]
    InvalidIndex { idx: i64 },
}

pub type EvalResult = Result<Object, RuntimeError>;
