pub mod builtins;
pub mod env;
pub mod object;

use crate::ast::{self, Expr, Stmt};
use env::Env;
use object::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

#[derive(Debug)]
pub struct Evaluator {
    env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Env::new())),
        }
    }

    pub fn eval(&mut self, program: ast::Program) -> EvalResult {
        let mut result = NULL;
        for stmt in program.0 {
            result = self.eval_stmt(stmt)?;
            if let Object::ReturnVal(obj) = result {
                return Ok(*obj);
            }
        }
        Ok(result)
    }

    fn eval_stmts(&mut self, stmts: ast::Block) -> EvalResult {
        let mut result = NULL;
        for stmt in stmts.0 {
            result = self.eval_stmt(stmt)?;
            if let Object::ReturnVal(_) = result {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> EvalResult {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::Return { expr } => Ok(Object::ReturnVal(Box::new(self.eval_expr(expr)?))),
            Stmt::Let { ident, expr } => {
                let name = ident.0;
                let val = self.eval_expr(expr)?;
                self.env.borrow_mut().set(name, val);
                Ok(NULL)
            }
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> EvalResult {
        match expr {
            Expr::Identifier(ast::Identifier(name)) => self.eval_ident(&name),
            Expr::IntegerLiteral(i) => Ok(Object::Int(i)),
            Expr::BooleanLiteral(b) => Ok(bool_to_obj(b)),
            Expr::StringLiteral(s) => Ok(Object::String(s)),
            Expr::ArrayLiteral(elems) => {
                let objs = self.eval_expressions(elems)?;
                Ok(Object::Array(objs))
            }
            Expr::HashLiteral(hash) => Ok(self.eval_hash_literal(hash)?),
            Expr::Index { expr, index } => {
                let left = self.eval_expr(*expr)?;
                let index = self.eval_expr(*index)?;
                self.eval_index_expr(left, index)
            }
            Expr::Infix { left, op, right } => {
                let left_val = self.eval_expr(*left)?;
                let right_val = self.eval_expr(*right)?;
                self.eval_infix_expr(op, left_val, right_val)
            }
            Expr::Prefix { op, expr } => {
                let right = self.eval_expr(*expr)?;
                self.eval_prefix_expr(op, right)
            }
            Expr::If {
                condition,
                consequence,
                alternative,
            } => {
                let truthy = is_truthy(self.eval_expr(*condition)?);
                self.eval_if_expr(truthy, consequence, alternative)
            }
            Expr::Function { params, body } => Ok(Object::Function {
                params,
                body,
                env: Rc::clone(&self.env),
            }),
            Expr::Call { func, args } => {
                let function = self.eval_expr(*func)?;
                let arguments = self.eval_expressions(args)?;
                self.eval_call_expr(function, arguments)
            }
        }
    }

    fn eval_prefix_expr(&self, op: ast::PrefixOp, right: Object) -> EvalResult {
        match op {
            ast::PrefixOp::Bang => self.eval_bang_op(right),
            ast::PrefixOp::Minus => self.eval_minus_op(right),
            ast::PrefixOp::Plus => self.eval_plus_op(right),
        }
    }

    fn eval_bang_op(&self, right: Object) -> EvalResult {
        match right {
            TRUE => Ok(FALSE),
            FALSE => Ok(TRUE),
            NULL => Ok(TRUE),
            _ => Err(RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Bang,
                right: right.monkey_type(),
            }),
        }
    }

    fn eval_minus_op(&self, right: Object) -> EvalResult {
        match right {
            Object::Int(i) => Ok(Object::Int(-i)),
            _ => Err(RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Minus,
                right: right.monkey_type(),
            }),
        }
    }

    fn eval_plus_op(&self, right: Object) -> EvalResult {
        match right {
            Object::Int(_) => Ok(right),
            _ => Err(RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Plus,
                right: right.monkey_type(),
            }),
        }
    }

    fn eval_infix_expr(&self, op: ast::InfixOp, left: Object, right: Object) -> EvalResult {
        match (&left, &right) {
            (Object::Int(x), Object::Int(y)) => self.eval_int_infix_expr(op, *x, *y),
            (Object::String(s1), Object::String(s2)) => self.eval_string_infix_expr(op, s1, s2),
            _ => match op {
                ast::InfixOp::Eq => Ok(bool_to_obj(left == right)),
                ast::InfixOp::NotEq => Ok(bool_to_obj(left != right)),
                _ => Err(RuntimeError::InvalidInfixOperands {
                    op,
                    left: left.monkey_type(),
                    right: right.monkey_type(),
                }),
            },
        }
    }

    fn eval_int_infix_expr(&self, op: ast::InfixOp, x: i64, y: i64) -> EvalResult {
        macro_rules! check_overflow {
            ($op:ident) => {
                x.$op(y)
                    .map_or(Err(RuntimeError::IntegerOverflow { op, x, y }), |x| {
                        Ok(Object::Int(x))
                    })
            };
        }
        macro_rules! check_zero_div {
            ($op:ident) => {
                x.$op(y)
                    .map_or(Err(RuntimeError::DivisionByZero { op, x, y }), |x| {
                        Ok(Object::Int(x))
                    })
            };
        }

        match op {
            ast::InfixOp::Plus => check_overflow!(checked_add),
            ast::InfixOp::Minus => check_overflow!(checked_sub),
            ast::InfixOp::Asterisk => check_overflow!(checked_mul),
            ast::InfixOp::Slash => check_zero_div!(checked_div),
            ast::InfixOp::Modulo => check_zero_div!(checked_rem),
            ast::InfixOp::Eq => Ok(bool_to_obj(x == y)),
            ast::InfixOp::NotEq => Ok(bool_to_obj(x != y)),
            ast::InfixOp::Gt => Ok(bool_to_obj(x > y)),
            ast::InfixOp::Lt => Ok(bool_to_obj(x < y)),
            ast::InfixOp::Ge => Ok(bool_to_obj(x >= y)),
            ast::InfixOp::Le => Ok(bool_to_obj(x <= y)),
        }
    }

    fn eval_if_expr(
        &mut self,
        condition: bool,
        consequence: ast::Block,
        alternative: Option<ast::Block>,
    ) -> EvalResult {
        if condition {
            self.eval_stmts(consequence)
        } else if let Some(alt) = alternative {
            self.eval_stmts(alt)
        } else {
            Ok(NULL)
        }
    }

    fn eval_ident(&mut self, name: &str) -> EvalResult {
        let val = self.env.borrow().get(name.to_owned());
        if let Some(val) = val {
            Ok(val)
        } else if let Some(builtin) = builtins::get_builtin(name) {
            Ok(builtin)
        } else {
            Err(RuntimeError::VariableNotFound {
                name: name.to_owned(),
            })
        }
    }

    fn eval_expressions(&mut self, exprs: Vec<Expr>) -> Result<Vec<Object>, RuntimeError> {
        let mut results = Vec::new();
        for expr in exprs {
            results.push(self.eval_expr(expr)?);
        }
        Ok(results)
    }

    fn eval_call_expr(&mut self, func: Object, args: Vec<Object>) -> EvalResult {
        match func {
            Object::Function { params, body, env } => {
                if params.len() != args.len() {
                    return Err(RuntimeError::NotEnoughArguments {
                        expected: params.len() as i32,
                        got: args.len() as i32,
                    });
                }
                let outer_env = Rc::clone(&self.env);
                // Define the environment for the function to be called
                let func_env = {
                    let mut scope = Env::new_enclosed(Rc::clone(&env));
                    for (arg, param) in args.iter().zip(params) {
                        scope.set(param.0, arg.clone());
                    }
                    scope
                };

                // Call the function with the function environment
                self.env = Rc::new(RefCell::new(func_env));
                let result = self.eval_stmts(body)?;
                // Reset back to outer scope
                self.env = outer_env;
                Ok(result)
            }
            Object::Builtin { function } => function(args),
            _ => Err(RuntimeError::NotAFunction(func)),
        }
    }

    fn eval_string_infix_expr(&self, op: ast::InfixOp, s1: &str, s2: &str) -> EvalResult {
        match op {
            ast::InfixOp::Plus => Ok(Object::String(s1.to_owned() + s2)),
            ast::InfixOp::Eq => Ok(bool_to_obj(s1 == s2)),
            ast::InfixOp::NotEq => Ok(bool_to_obj(s1 != s2)),
            _ => Err(RuntimeError::InvalidInfixOperands {
                op,
                left: Type::String,
                right: Type::String,
            }),
        }
    }

    fn eval_index_expr(&self, left: Object, index: Object) -> EvalResult {
        match (&left, &index) {
            (Object::Array(arr), Object::Int(i)) => {
                if *i < 0 {
                    return Ok(NULL);
                }
                let idx =
                    usize::try_from(*i).map_err(|_| RuntimeError::InvalidIndex { idx: *i })?;
                if idx > arr.len() - 1 {
                    Ok(NULL)
                } else {
                    Ok(arr[idx].clone())
                }
            }
            (Object::HashMap(hashmap), _) => match index {
                Object::Int(_) | Object::Bool(_) | Object::String(_) => match hashmap.get(&index) {
                    Some(obj) => Ok(obj.clone()),
                    None => Ok(NULL),
                },
                _ => Err(RuntimeError::IndexOperatorNotSupported {
                    left: Type::HashMap,
                    index: index.monkey_type(),
                }),
            },
            _ => Err(RuntimeError::IndexOperatorNotSupported {
                left: left.monkey_type(),
                index: index.monkey_type(),
            }),
        }
    }

    fn eval_hash_literal(&mut self, hashmap: Vec<(Expr, Expr)>) -> EvalResult {
        let mut pairs = HashMap::new();
        for (key, value) in hashmap {
            pairs.insert(self.eval_expr(key)?, self.eval_expr(value)?);
        }
        Ok(Object::HashMap(pairs))
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        TRUE => true,
        FALSE => false,
        NULL => false,
        Object::Int(i) if i == 0 => false,
        Object::String(s) if s.is_empty() => false,
        _ => true,
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

fn bool_to_obj(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    macro_rules! test_eval {
        ($inputs:expr, $expected:expr) => {
            for (input, expect) in $inputs.iter().zip($expected) {
                let lexer = Lexer::new(input);
                let parser = Parser::new(lexer);
                let program = parser
                    .parse_program()
                    .expect("Should have no parser errors");
                let mut eval = Evaluator::new();

                assert_eq!(
                    expect,
                    eval.eval(program).expect("Should evaluate with no error")
                );
            }
        };
    }

    macro_rules! rt_err_eval {
        ($inputs:expr, $errs:expr) => {
            for (input, err) in $inputs.iter().zip($errs) {
                let lexer = Lexer::new(input);
                let parser = Parser::new(lexer);
                let program = parser
                    .parse_program()
                    .expect("Should have no parser errors");
                let mut eval = Evaluator::new();

                assert_eq!(
                    err,
                    eval.eval(program)
                        .expect_err("Should evaluate with an error")
                );
            }
        };
    }

    #[test]
    fn eval_integer() {
        let inputs = ["1", "4", "32903439", "-5", "+15"];
        let expected = [
            Object::Int(1),
            Object::Int(4),
            Object::Int(32903439),
            Object::Int(-5),
            Object::Int(15),
        ];
        test_eval!(inputs, expected);
    }

    #[test]
    fn eval_bang_op() {
        let inputs = ["!true", "!false", "!!true", "!!false"];
        let expected = [FALSE, TRUE, TRUE, FALSE];
        test_eval!(inputs, expected);
    }

    #[test]
    fn eval_infix_int_expr() {
        let inputs = [
            "1 + 2",
            "3 - 4",
            "4 * 5",
            "20 / 4",
            "10 % 2",
            "20 == 20",
            "15 != 5",
            "10 > 4",
            "10 < 7",
            "10 >= 10",
            "30 <= 8",
            "(3 + 5) * 2 == 16",
        ];
        let expected = [
            Object::Int(3),
            Object::Int(-1),
            Object::Int(20),
            Object::Int(5),
            Object::Int(0),
            TRUE,
            TRUE,
            TRUE,
            FALSE,
            TRUE,
            FALSE,
            TRUE,
        ];
        test_eval!(inputs, expected);
    }

    #[test]
    fn eval_bool_infix_expr() {
        let inputs = ["true == true", "true == false", "3 * 5 == 15", "4 / 2 != 2"];
        let expected = [TRUE, FALSE, TRUE, FALSE];
        test_eval!(inputs, expected);
    }

    #[test]
    fn invalid_prefix_operand_runtime_error() {
        let inputs = ["+true", "-false", "!5"];
        let errs = [
            RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Plus,
                right: Type::Bool,
            },
            RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Minus,
                right: Type::Bool,
            },
            RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Bang,
                right: Type::Int,
            },
        ];

        rt_err_eval!(inputs, errs);
    }

    #[test]
    fn invalid_infix_operands_runtime_error() {
        let inputs = ["1 + true", "true >= false", "false > 1", "3 / true"];
        let errs = [
            RuntimeError::InvalidInfixOperands {
                op: ast::InfixOp::Plus,
                left: Type::Int,
                right: Type::Bool,
            },
            RuntimeError::InvalidInfixOperands {
                op: ast::InfixOp::Ge,
                left: Type::Bool,
                right: Type::Bool,
            },
            RuntimeError::InvalidInfixOperands {
                op: ast::InfixOp::Gt,
                left: Type::Bool,
                right: Type::Int,
            },
            RuntimeError::InvalidInfixOperands {
                op: ast::InfixOp::Slash,
                left: Type::Int,
                right: Type::Bool,
            },
        ];

        rt_err_eval!(inputs, errs);
    }

    #[test]
    fn integer_overflow_runtime_error() {
        let inputs = [
            "9223372036854775807 + 1",
            "-9223372036854775807 - 2",
            "9223372036854775807 * 2",
        ];
        let errs = [
            RuntimeError::IntegerOverflow {
                op: ast::InfixOp::Plus,
                x: 9223372036854775807,
                y: 1,
            },
            RuntimeError::IntegerOverflow {
                op: ast::InfixOp::Minus,
                x: -9223372036854775807,
                y: 2,
            },
            RuntimeError::IntegerOverflow {
                op: ast::InfixOp::Asterisk,
                x: 9223372036854775807,
                y: 2,
            },
        ];

        rt_err_eval!(inputs, errs);
    }

    #[test]
    fn divide_by_zero_runtime_error() {
        let inputs = ["1 / 0", "2 % 0"];
        let errs = [
            RuntimeError::DivisionByZero {
                op: ast::InfixOp::Slash,
                x: 1,
                y: 0,
            },
            RuntimeError::DivisionByZero {
                op: ast::InfixOp::Modulo,
                x: 2,
                y: 0,
            },
        ];

        rt_err_eval!(inputs, errs);
    }

    #[test]
    fn eval_if_expr() {
        let inputs = [
            "if (1) { true }",
            "if (false) { 3 } else { 4 }",
            "if (true) { false }",
            "if (false) { 77 }",
        ];
        let expected = [TRUE, Object::Int(4), FALSE, NULL];

        test_eval!(inputs, expected);
    }

    #[test]
    fn eval_let_stmt() {
        let inputs = ["let x = 5; x", "let y = 3", "let x = !true; x"];
        let expected = [Object::Int(5), NULL, Object::Bool(false)];

        test_eval!(inputs, expected)
    }

    #[test]
    fn ident_not_found() {
        let inputs = ["let x = 5; y", "not_found"];
        let errs = [
            RuntimeError::VariableNotFound {
                name: "y".to_owned(),
            },
            RuntimeError::VariableNotFound {
                name: "not_found".to_owned(),
            },
        ];

        rt_err_eval!(inputs, errs);
    }

    #[test]
    fn eval_func_literal() {
        let input = ["fn(x) { x + 2; };"];
        let expected = [Object::Function {
            params: vec![ast::Identifier::from("x")],
            body: ast::Block(vec![Stmt::Expr(Expr::Infix {
                left: Box::new(Expr::Identifier(ast::Identifier::from("x"))),
                op: ast::InfixOp::Plus,
                right: Box::new(Expr::IntegerLiteral(2)),
            })]),
            env: Rc::new(RefCell::new(Env::new())),
        }];

        test_eval!(input, expected);
    }

    #[test]
    fn eval_functions() {
        let inputs = [
            "let identity = fn(x) { x }; identity(5)",
            "let identity = fn(x) { return x; }; identity(5)",
            "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5));",
            "fn(x) { x }(5)",
        ];
        let expected = [
            Object::Int(5),
            Object::Int(5),
            Object::Int(20),
            Object::Int(5),
        ];

        test_eval!(inputs, expected);
    }

    #[test]
    fn function_inner_scope_is_not_global() {
        let input = ["let func = fn() { let inner = 3; }; func(); inner"];
        let err = [RuntimeError::VariableNotFound {
            name: "inner".to_owned(),
        }];

        rt_err_eval!(input, err);
    }

    #[test]
    fn function_can_use_outer_scope() {
        let input = ["let x = 3; let f = fn() { x }; f()"];
        let expected = [Object::Int(3)];

        test_eval!(input, expected);
    }

    #[test]
    fn eval_strings() {
        let input = ["\"Hello World!\""];
        let expected = [Object::String("Hello World!".to_owned())];

        test_eval!(input, expected);
    }

    #[test]
    fn eval_string_infix_ops() {
        let inputs = [
            "\"Hi\" == \"Hi\"",
            "\"Hello\" != \"World\"",
            "\"Hello \" + \"World\"",
        ];
        let expected = [TRUE, TRUE, Object::String("Hello World".to_owned())];

        test_eval!(inputs, expected);
    }

    #[test]
    fn eval_len_builtin_function() {
        let inputs = ["len(\"hello world\")", "len(\"four\")"];
        let expected = [Object::Int(11), Object::Int(4)];

        test_eval!(inputs, expected);
    }

    #[test]
    fn len_errors_with_wrong_args() {
        let inputs = ["len(3)", "len(\"hi\", \"hello world\")"];
        let errs = [
            RuntimeError::WrongArgType {
                got: Type::Int,
                want: type_signature!(Array | String),
            },
            RuntimeError::NotEnoughArguments {
                expected: 1,
                got: 2,
            },
        ];

        rt_err_eval!(inputs, errs);
    }

    #[test]
    fn eval_array_literal() {
        let inputs = ["[1, 2, 3]", "[1 + 1, 2 * 2, true]"];
        let expected = [
            Object::Array(vec![Object::Int(1), Object::Int(2), Object::Int(3)]),
            Object::Array(vec![Object::Int(2), Object::Int(4), Object::Bool(true)]),
        ];

        test_eval!(inputs, expected)
    }

    #[test]
    fn eval_array_index_expr() {
        let inputs = [
            "[1, 2, 3][0]",
            "let i = 0; [1, 2][i + 1]",
            "let arr = [1, 2, 3]; arr[2]",
            "[1, 2, 3][3]",
            "[1, 2, 3][-1]",
        ];
        let expected = [Object::Int(1), Object::Int(2), Object::Int(3), NULL, NULL];

        test_eval!(inputs, expected);
    }

    #[test]
    fn eval_hashmap_literal() {
        let inputs = ["{\"one\": 10 - 9, 3 + 3: 7}"];
        let hash = {
            let mut hashmap = HashMap::new();
            hashmap.insert(Object::String("one".to_owned()), Object::Int(1));
            hashmap.insert(Object::Int(6), Object::Int(7));
            hashmap
        };

        test_eval!(inputs, [Object::HashMap(hash)])
    }

    #[test]
    fn eval_hashmap_index_expr() {
        let inputs = [
            "{\"foo\": 5}[\"foo\"]",
            "{\"foo\": 5}[\"bar\"]",
            "{3: true}[3]",
        ];
        let expected = [Object::Int(5), NULL, Object::Bool(true)];

        test_eval!(inputs, expected);
    }
}
