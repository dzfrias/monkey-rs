use super::object::{EvalResult, Object, RuntimeError};

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin {
            function: monkey_len,
        }),
        _ => None,
    }
}

fn monkey_len(args: Vec<Object>) -> EvalResult {
    if args.len() != 1 {
        return Err(RuntimeError::NotEnoughArguments {
            expected: 1,
            got: args.len() as i32,
        });
    }
    match &args[0] {
        Object::String(s) => Ok(Object::Int(s.len() as i64)),
        _ => Err(RuntimeError::WrongArgType {
            got: args[0].to_string(),
            want: "String".to_owned(),
        }),
    }
}
