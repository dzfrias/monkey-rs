pub mod object;

use crate::ast::{self, Expr, Stmt};
use object::Object;

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, program: ast::Program) -> Option<Object> {
        let mut result = None;
        for stmt in program.0 {
            result = self.eval_stmt(stmt);
        }
        result
    }

    fn eval_stmt(&self, stmt: Stmt) -> Option<Object> {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(expr),
            _ => None,
        }
    }

    fn eval_expr(&self, expr: Expr) -> Option<Object> {
        match expr {
            Expr::IntegerLiteral(i) => Some(Object::Int(i)),
            Expr::BooleanLiteral(b) => Some(bool_to_obj(b)),
            Expr::Infix { left, op, right } => {
                Some(self.eval_infix_expr(op, self.eval_expr(*left)?, self.eval_expr(*right)?))
            }
            Expr::Prefix { op, expr } => Some(self.eval_prefix_expr(op, self.eval_expr(*expr)?)),
            _ => None,
        }
    }

    fn eval_prefix_expr(&self, op: ast::PrefixOp, right: Object) -> Object {
        match op {
            ast::PrefixOp::Bang => self.eval_bang_op(right),
            ast::PrefixOp::Minus => self.eval_minus_op(right),
            ast::PrefixOp::Plus => right,
        }
    }

    fn eval_bang_op(&self, right: Object) -> Object {
        match right {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        }
    }

    fn eval_minus_op(&self, right: Object) -> Object {
        match right {
            Object::Int(i) => Object::Int(-i),
            _ => Object::Null,
        }
    }

    fn eval_infix_expr(&self, op: ast::InfixOp, left: Object, right: Object) -> Object {
        if let (Object::Int(x), Object::Int(y)) = (&left, &right) {
            self.eval_int_infix_expr(op, *x, *y)
        } else {
            match op {
                ast::InfixOp::Eq => bool_to_obj(left == right),
                ast::InfixOp::NotEq => bool_to_obj(left != right),
                _ => NULL,
            }
        }
    }

    fn eval_int_infix_expr(&self, op: ast::InfixOp, x: i64, y: i64) -> Object {
        match op {
            ast::InfixOp::Plus => Object::Int(x + y),
            ast::InfixOp::Minus => Object::Int(x - y),
            ast::InfixOp::Asterisk => Object::Int(x * y),
            ast::InfixOp::Slash => Object::Int(x / y),
            ast::InfixOp::Modulo => Object::Int(x % y),
            ast::InfixOp::Eq => bool_to_obj(x == y),
            ast::InfixOp::NotEq => bool_to_obj(x != y),
            ast::InfixOp::Gt => bool_to_obj(x > y),
            ast::InfixOp::Lt => bool_to_obj(x < y),
            ast::InfixOp::Ge => bool_to_obj(x >= y),
            ast::InfixOp::Le => bool_to_obj(x <= y),
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
        let inputs = ["!true", "!false", "!5", "!!true", "!!false", "!!5"];
        let expected = [FALSE, TRUE, FALSE, TRUE, FALSE, TRUE];
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
        let inputs = [
            "true == true",
            "true == false",
            "3 * 5 == 15",
            "4 / 2 != 2",
            "3 <= true",
            "true > false",
        ];
        let expected = [TRUE, FALSE, TRUE, FALSE, NULL, NULL];
        test_eval!(inputs, expected);
    }
}
