use super::object::*;

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin {
            function: monkey_len,
        }),
        "first" => Some(Object::Builtin {
            function: monkey_first,
        }),
        "last" => Some(Object::Builtin {
            function: monkey_last,
        }),
        "rest" => Some(Object::Builtin {
            function: monkey_rest,
        }),
        "push" => Some(Object::Builtin {
            function: monkey_push,
        }),
        _ => None,
    }
}

macro_rules! validate_args_len {
    ($args:expr, $expected:expr) => {
        if $args.len() != $expected {
            return Err(RuntimeError::NotEnoughArguments {
                expected: $expected as i32,
                got: $args.len() as i32,
            });
        }
    };
}

fn monkey_len(args: Vec<Object>) -> EvalResult {
    validate_args_len!(args, 1);
    match &args[0] {
        Object::String(s) => Ok(Object::Int(s.len() as i64)),
        Object::Array(arr) => Ok(Object::Int(arr.len() as i64)),
        _ => Err(RuntimeError::WrongArgType {
            got: args[0].monkey_type(),
            want: type_signature!(Array, String),
        }),
    }
}

fn monkey_first(args: Vec<Object>) -> EvalResult {
    validate_args_len!(args, 1);
    match &args[0] {
        Object::Array(arr) => Ok(arr.first().unwrap_or(&NULL).clone()),
        Object::String(s) => match s.chars().next() {
            Some(ch) => Ok(Object::String(ch.to_string())),
            None => Ok(Object::Null),
        },
        _ => Err(RuntimeError::WrongArgType {
            got: args[0].monkey_type(),
            want: type_signature!(Array, String),
        }),
    }
}

fn monkey_last(args: Vec<Object>) -> EvalResult {
    validate_args_len!(args, 1);
    match &args[0] {
        Object::Array(arr) => Ok(arr.last().unwrap_or(&NULL).clone()),
        Object::String(s) => match s.chars().last() {
            Some(ch) => Ok(Object::String(ch.to_string())),
            None => Ok(Object::Null),
        },
        _ => Err(RuntimeError::WrongArgType {
            got: args[0].monkey_type(),
            want: type_signature!(Array, String),
        }),
    }
}

fn monkey_rest(args: Vec<Object>) -> EvalResult {
    validate_args_len!(args, 1);
    match &args[0] {
        Object::Array(arr) => Ok(Object::Array(arr[1..].to_vec())),
        Object::String(s) => Ok(Object::String(s[1..].to_owned())),
        _ => Err(RuntimeError::WrongArgType {
            got: args[0].monkey_type(),
            want: type_signature!(Array, String),
        }),
    }
}

fn monkey_push(args: Vec<Object>) -> EvalResult {
    validate_args_len!(args, 2);
    match &args[0] {
        Object::Array(arr) => {
            let mut new = arr.clone();
            new.push(args[1].clone());
            Ok(Object::Array(new))
        }
        _ => Err(RuntimeError::WrongArgType {
            got: args[0].monkey_type(),
            want: type_signature!(Array),
        }),
    }
}
