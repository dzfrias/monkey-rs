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
            Expr::BooleanLiteral(b) => {
                if b {
                    Some(TRUE)
                } else {
                    Some(FALSE)
                }
            }
            Expr::Prefix { op, expr } => Some(self.eval_prefix_expr(op, self.eval_expr(*expr)?)),
            _ => None,
        }
    }

    fn eval_prefix_expr(&self, op: ast::PrefixOp, right: Object) -> Object {
        match op {
            ast::PrefixOp::Bang => self.eval_bang_op(right),
            ast::PrefixOp::Minus => self.eval_minus_op(right),
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
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn eval_integer() {
        let inputs = ["1", "4", "32903439", "-5"];
        let expected = [1, 4, 32903439, -5];

        for (input, expect) in inputs.iter().zip(expected) {
            let lexer = Lexer::new(input);
            let parser = Parser::new(lexer);
            let program = parser
                .parse_program()
                .expect("Should have no parser errors");
            let eval = Evaluator::new();

            assert_eq!(
                Object::Int(expect),
                eval.eval(program).expect("Should not return None")
            )
        }
    }

    #[test]
    fn eval_bool() {
        let inputs = ["true", "false"];
        let expected = [TRUE, FALSE];

        for (input, expect) in inputs.iter().zip(expected) {
            let lexer = Lexer::new(input);
            let parser = Parser::new(lexer);
            let program = parser
                .parse_program()
                .expect("Should have no parser errors");
            let eval = Evaluator::new();

            assert_eq!(expect, eval.eval(program).expect("Should not return None"))
        }
    }

    #[test]
    fn eval_bang_op() {
        let inputs = ["!true", "!false", "!5", "!!true", "!!false", "!!5"];
        let expected = [FALSE, TRUE, FALSE, TRUE, FALSE, TRUE];

        for (input, expect) in inputs.iter().zip(expected) {
            let lexer = Lexer::new(input);
            let parser = Parser::new(lexer);
            let program = parser
                .parse_program()
                .expect("Should have no parser errors");
            let eval = Evaluator::new();

            assert_eq!(expect, eval.eval(program).expect("Should not return None"))
        }
    }
}
