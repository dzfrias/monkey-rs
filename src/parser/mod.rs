#![allow(dead_code)]

use crate::ast::{self, Expr, Stmt};
use crate::lexer::Lexer;
use crate::token::Token;
use std::fmt;

#[derive(Debug)]
enum OperatorPrec {
    Lowest = 1,
    Equals = 2,
    Comparison = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

#[derive(Debug, PartialEq)]
pub struct ParserError(String);

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parser error: {}", self.0)
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    errors: Vec<ParserError>,

    current_tok: Token,
    peek_tok: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            errors: Vec::new(),
            current_tok: Token::EOF,
            peek_tok: Token::EOF,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program: ast::Program = Vec::new();

        loop {
            if self.current_tok == Token::EOF {
                break;
            }

            if let Some(stmt) = self.parse_statement() {
                program.push(stmt);
            }

            self.next_token();
        }

        program
    }

    pub fn errors(&self) -> &[ParserError] {
        self.errors.as_ref()
    }

    fn next_token(&mut self) {
        self.current_tok = self.peek_tok.clone();
        self.peek_tok = self.lexer.next_token();
    }

    fn push_error(&mut self, reason: &str) {
        self.errors.push(ParserError(reason.to_owned()));
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.current_tok {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        let ident = if let Token::Ident(ident) = &self.peek_tok {
            ast::Identifier(ident.to_owned())
        } else {
            self.push_error("Expected identifier");
            return None;
        };
        // To Ident token
        self.next_token();

        if self.peek_tok != Token::Assign {
            self.push_error("Expected assignment ('=' sign)");
            return None;
        }

        // To expression token(s)
        self.next_token();
        while self.current_tok != Token::Semicolon {
            self.next_token();
        }

        Some(Stmt::Let {
            ident,
            expr: Expr::Blank,
        })
    }

    fn parse_return_statement(&mut self) -> Option<Stmt> {
        // To expression token(s)
        self.next_token();
        while self.current_tok != Token::Semicolon {
            self.next_token();
        }

        Some(Stmt::Return { expr: Expr::Blank })
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expr(OperatorPrec::Lowest);
        // Optional semicolon
        if self.peek_tok == Token::Semicolon {
            self.next_token();
        }
        if let Some(expression) = expr {
            Some(Stmt::Expr(expression))
        } else {
            None
        }
    }

    fn parse_expr(&mut self, precendence: OperatorPrec) -> Option<Expr> {
        match self.current_tok {
            Token::Ident(_) => self.parse_ident(),
            Token::Int(_) => self.parse_integer(),
            Token::Bang | Token::Minus => self.parse_prefix_expr(),
            _ => {
                self.push_error(
                    format!("No prefix operator {:?} found", self.current_tok).as_ref(),
                );
                None
            }
        }
    }

    fn parse_ident(&mut self) -> Option<Expr> {
        if let Token::Ident(name) = &self.current_tok {
            Some(Expr::Identifier(ast::Identifier(name.to_owned())))
        } else {
            None
        }
    }

    fn parse_integer(&mut self) -> Option<Expr> {
        if let Token::Int(int) = &self.current_tok {
            let result = match int.parse() {
                Ok(int) => int,
                Err(_) => {
                    self.push_error("Integer parsing failed");
                    return None;
                }
            };
            Some(Expr::IntegerLiteral(result))
        } else {
            None
        }
    }

    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix_op = match self.current_tok {
            Token::Minus => ast::PrefixOp::Minus,
            Token::Bang => ast::PrefixOp::Bang,
            _ => return None,
        };
        // To expression
        self.next_token();
        let expr = match self.parse_expr(OperatorPrec::Prefix) {
            Some(expr) => expr,
            None => return None,
        };
        Some(Expr::Prefix {
            op: prefix_op,
            expr: Box::new(expr),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn no_parse_errs(parser: Parser) {
        assert_eq!(Vec::<ParserError>::new(), parser.errors);
    }

    #[test]
    fn parser_parses_let_stmt() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        no_parse_errs(parser);
        assert_eq!(3, program.len());

        let idents = ["x", "y", "foobar"];
        for (i, expect_ident) in idents.iter().enumerate() {
            let stmt = &program[i];
            assert!(matches!(stmt, Stmt::Let { .. }));
            if let Stmt::Let { ident, .. } = stmt {
                assert_eq!(expect_ident.to_owned(), ident.0);
            }
        }
    }

    #[test]
    fn parser_throws_err_with_no_ident() {
        let input = "let = 5;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        parser.parse_program();

        assert!(parser.errors.len() > 0);
    }

    #[test]
    fn parser_throws_err_with_no_eq_sign() {
        let input = "let x 5;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        parser.parse_program();

        assert_eq!(
            vec![ParserError("Expected assignment ('=' sign)".to_owned())],
            parser.errors
        );
    }

    #[test]
    fn parse_return_statement() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();
        no_parse_errs(parser);
        assert_eq!(3, program.len());

        for stmt in program {
            assert!(matches!(stmt, Stmt::Return { .. }));
        }
    }

    #[test]
    fn parse_ident_expr() {
        let mut lexer = Lexer::new("foobar;");
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        no_parse_errs(parser);

        assert_eq!(1, program.len());
        assert_eq!(
            vec![Stmt::Expr(Expr::Identifier(ast::Identifier(
                "foobar".to_owned()
            )))],
            program
        )
    }

    #[test]
    fn parse_integer_literal() {
        let mut lexer = Lexer::new("5;");
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        no_parse_errs(parser);

        assert_eq!(1, program.len());
        assert_eq!(vec![Stmt::Expr(Expr::IntegerLiteral(5))], program);
    }

    #[test]
    fn parse_integer_errors_with_int_over_i64_limit() {
        let mut lexer = Lexer::new("92233720368547758073290;");
        let mut parser = Parser::new(&mut lexer);
        parser.parse_program();

        assert_eq!(
            vec![ParserError("Integer parsing failed".to_owned())],
            parser.errors
        );
    }

    #[test]
    fn parse_prefix_expr() {
        let inputs = vec!["!5", "-15"];
        let expected = vec![(ast::PrefixOp::Bang, 5), (ast::PrefixOp::Minus, 15)];

        for (i, input) in inputs.iter().enumerate() {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);

            assert_eq!(1, program.len());
            assert_eq!(
                Stmt::Expr(Expr::Prefix {
                    op: expected[i].0.clone(),
                    expr: Box::new(Expr::IntegerLiteral(expected[i].1))
                }),
                program[0]
            )
        }
    }
}
