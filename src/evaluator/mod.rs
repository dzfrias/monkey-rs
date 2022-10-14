pub mod object;

use crate::ast::{self, Expr, Stmt};
use object::Object;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum RuntimeError {
    #[error("cannot perform `{op}` on `{right}`")]
    InvalidPrefixOperand { op: ast::PrefixOp, right: Object },
    #[error("cannot perform `{op}` between `{right}` and `{left}`")]
    InvalidInfixOperands {
        op: ast::InfixOp,
        left: Object,
        right: Object,
    },
    #[error("integer overflow occured in the expression: `{x} {op} {y}`")]
    IntegerOverflow { op: ast::InfixOp, x: i64, y: i64 },
    #[error("division by zero occured in the expression: `{x} {op} {y}`")]
    DivisionByZero { op: ast::InfixOp, x: i64, y: i64 },
}

type EvalResult = Result<Object, RuntimeError>;

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, program: ast::Program) -> EvalResult {
        let mut result = NULL;
        for stmt in program.0 {
            result = self.eval_stmt(stmt)?;
            if let Object::ReturnVal(obj) = result {
                return Ok(*obj);
            }
        }
        Ok(result)
    }

    fn eval_stmts(&self, stmts: ast::Block) -> EvalResult {
        let mut result = NULL;
        for stmt in stmts.0 {
            result = self.eval_stmt(stmt)?;
            if let Object::ReturnVal(_) = result {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_stmt(&self, stmt: Stmt) -> EvalResult {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::Return { expr } => Ok(Object::ReturnVal(Box::new(self.eval_expr(expr)?))),
            _ => todo!("evaluating `{:?}`", stmt),
        }
    }

    fn eval_expr(&self, expr: Expr) -> EvalResult {
        match expr {
            Expr::IntegerLiteral(i) => Ok(Object::Int(i)),
            Expr::BooleanLiteral(b) => Ok(bool_to_obj(b)),
            Expr::Infix { left, op, right } => {
                self.eval_infix_expr(op, self.eval_expr(*left)?, self.eval_expr(*right)?)
            }
            Expr::Prefix { op, expr } => self.eval_prefix_expr(op, self.eval_expr(*expr)?),
            Expr::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expr(
                is_truthy(self.eval_expr(*condition)?),
                consequence,
                alternative,
            ),
            _ => todo!("evaluating `{:?}`", expr),
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
                right,
            }),
        }
    }

    fn eval_minus_op(&self, right: Object) -> EvalResult {
        match right {
            Object::Int(i) => Ok(Object::Int(-i)),
            _ => Err(RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Minus,
                right,
            }),
        }
    }

    fn eval_plus_op(&self, right: Object) -> EvalResult {
        match right {
            Object::Int(_) => Ok(right),
            _ => Err(RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Plus,
                right,
            }),
        }
    }

    fn eval_infix_expr(&self, op: ast::InfixOp, left: Object, right: Object) -> EvalResult {
        if let (Object::Int(x), Object::Int(y)) = (&left, &right) {
            self.eval_int_infix_expr(op, *x, *y)
        } else {
            match op {
                ast::InfixOp::Eq => Ok(bool_to_obj(left == right)),
                ast::InfixOp::NotEq => Ok(bool_to_obj(left != right)),
                _ => Err(RuntimeError::InvalidInfixOperands { op, left, right }),
            }
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
        &self,
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
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        TRUE => true,
        FALSE => false,
        NULL => false,
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
                let eval = Evaluator::new();

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
                let eval = Evaluator::new();

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
                right: TRUE,
            },
            RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Minus,
                right: FALSE,
            },
            RuntimeError::InvalidPrefixOperand {
                op: ast::PrefixOp::Bang,
                right: Object::Int(5),
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
                left: Object::Int(1),
                right: TRUE,
            },
            RuntimeError::InvalidInfixOperands {
                op: ast::InfixOp::Ge,
                left: TRUE,
                right: FALSE,
            },
            RuntimeError::InvalidInfixOperands {
                op: ast::InfixOp::Gt,
                left: FALSE,
                right: Object::Int(1),
            },
            RuntimeError::InvalidInfixOperands {
                op: ast::InfixOp::Slash,
                left: Object::Int(3),
                right: TRUE,
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
    fn eval_return() {
        let inputs = [
            "return 10; 9",
            "return 10;",
            "return 2 * 5;",
            "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
        ];
        let expected = [
            Object::Int(10),
            Object::Int(10),
            Object::Int(10),
            Object::Int(10),
        ];

        test_eval!(inputs, expected);
    }
}
