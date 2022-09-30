#![allow(dead_code)]

use crate::ast::{self, Expr, Stmt};
use crate::lexer::Lexer;
use crate::token::Token;
use std::fmt;

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
            _ => None,
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
        // TODO: Parse expressions
        while self.current_tok != Token::Semicolon {
            self.next_token();
        }

        Some(Stmt::Let {
            ident,
            expr: Expr::Blank,
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
            #[allow(irrefutable_let_patterns)]
            if let Stmt::Let { ident, .. } = stmt {
                assert_eq!(expect_ident.to_owned(), ident.0);
            }
        }
    }

    #[test]
    fn parser_throws_err_with_no_ident() {
        let input = "let = 5";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            vec![ParserError("Expected identifier".to_owned())],
            parser.errors
        );
        assert_eq!(Vec::new() as ast::Program, program);
    }

    #[test]
    fn parser_throws_err_with_no_eq_sign() {
        let input = "let x 5";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            vec![ParserError("Expected assignment ('=' sign)".to_owned())],
            parser.errors
        );
        assert_eq!(Vec::new() as ast::Program, program);
    }
}
