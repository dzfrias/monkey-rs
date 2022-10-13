pub mod object;

use crate::ast::{self, Expr, Stmt};
use object::Object;

type EvalResult = Result<Object, String>;

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
        let mut result = Err("".to_owned());
        for stmt in program.0 {
            result = self.eval_stmt(stmt);
        }
        result
    }

    fn eval_stmt(&self, stmt: Stmt) -> EvalResult {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(expr),
            _ => todo!(),
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
            _ => todo!(),
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
            _ => Err(format!("Cannot perform `!` on `{right}`")),
        }
    }

    fn eval_minus_op(&self, right: Object) -> EvalResult {
        match right {
            Object::Int(i) => Ok(Object::Int(-i)),
            _ => Err(format!("Cannot perform `-` on `{right}`")),
        }
    }

    fn eval_plus_op(&self, right: Object) -> EvalResult {
        match right {
            Object::Int(_) => Ok(right),
            _ => Err(format!("Cannot perform `+` on {right}")),
        }
    }

    fn eval_infix_expr(&self, op: ast::InfixOp, left: Object, right: Object) -> EvalResult {
        if let (Object::Int(x), Object::Int(y)) = (&left, &right) {
            self.eval_int_infix_expr(op, *x, *y)
        } else {
            match op {
                ast::InfixOp::Eq => Ok(bool_to_obj(left == right)),
                ast::InfixOp::NotEq => Ok(bool_to_obj(left != right)),
                _ => Err(format!("Cannot perform `{op}` on `{left}` and `{right}`")),
            }
        }
    }

    fn eval_int_infix_expr(&self, op: ast::InfixOp, x: i64, y: i64) -> EvalResult {
        macro_rules! checked_op {
            ($op:ident) => {
                x.$op(y).map_or(
                    Err(format!(
                        "Integer overflow occured in the expression: `{x} {op} {y}`"
                    )),
                    |x| Ok(Object::Int(x)),
                )
            };
        }
        match op {
            ast::InfixOp::Plus => checked_op!(checked_add),
            ast::InfixOp::Minus => checked_op!(checked_sub),
            ast::InfixOp::Asterisk => checked_op!(checked_mul),
            ast::InfixOp::Slash => checked_op!(checked_div),
            ast::InfixOp::Modulo => checked_op!(checked_rem),
            ast::InfixOp::Eq => Ok(bool_to_obj(x == y)),
            ast::InfixOp::NotEq => Ok(bool_to_obj(x != y)),
            ast::InfixOp::Gt => Ok(bool_to_obj(x > y)),
            ast::InfixOp::Lt => Ok(bool_to_obj(x < y)),
            ast::InfixOp::Ge => Ok(bool_to_obj(x >= y)),
            ast::InfixOp::Le => Ok(bool_to_obj(x <= y)),
        }
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

                assert_eq!(expect, eval.eval(program).expect("Should not return none"))
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
}
