use crate::ast::{self, Expr, Stmt};
use crate::lexer::Lexer;
use crate::token::Token;
use std::fmt;

#[derive(Debug, PartialEq, PartialOrd)]
enum Precendence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precendence {
    fn from_token(token: &Token) -> Self {
        match token {
            Token::Eq | Token::NotEq => Precendence::Equals,
            Token::Lt | Token::Gt => Precendence::LessGreater,
            Token::Plus | Token::Minus => Precendence::Sum,
            Token::Slash | Token::Asterisk => Precendence::Product,
            Token::Lparen => Precendence::Call,
            _ => Precendence::Lowest,
        }
    }
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
    /// Creates a new `Parser` with the first tokens read in.
    /// # Examples
    /// ```
    /// use monkey_rs::parser::Parser;
    /// use monkey_rs::lexer::Lexer;
    ///
    /// let input = "let x = 4;";
    /// let mut lexer = Lexer::new(input);
    /// let mut parser = Parser::new(&mut lexer);
    /// ```
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

    /// Parses the targeted program and into an [ast::Program](crate::ast::Program). This  will be
    /// a vector of [statements](crate::ast::Stmt).
    ///
    /// # Examples
    /// ```
    /// use monkey_rs::parser::Parser;
    /// use monkey_rs::lexer::Lexer;
    /// use monkey_rs::ast;
    ///
    /// let input = "2 + 3";
    /// let mut lexer = Lexer::new(input);
    /// let mut parser = Parser::new(&mut lexer);
    /// let program = parser.parse_program();
    /// assert_eq!(1, program.0.len());
    /// assert_eq!(
    ///     ast::Stmt::Expr(ast::Expr::Infix {
    ///         left: Box::new(ast::Expr::IntegerLiteral(2)),
    ///         op: ast::InfixOp::Plus,
    ///         right: Box::new(ast::Expr::IntegerLiteral(3)),
    ///     }),
    ///     program.0[0]
    /// );
    /// ```
    pub fn parse_program(&mut self) -> ast::Program {
        let mut program: ast::Program = ast::Block(Vec::new());

        while self.current_tok != Token::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.0.push(stmt);
            }
            self.next_token();
        }

        program
    }

    /// Gets the [errors](crate::parser::ParserError) that the parser ran into after parsing.
    /// # Examples
    /// ```
    /// use monkey_rs::parser::Parser;
    /// use monkey_rs::lexer::Lexer;
    /// use monkey_rs::ast;
    ///
    /// let input = "(2 + 3";
    /// let mut lexer = Lexer::new(input);
    /// let mut parser = Parser::new(&mut lexer);
    /// parser.parse_program();
    /// assert!(parser.errors().len() > 0);
    /// ```
    pub fn errors(&self) -> &[ParserError] {
        self.errors.as_ref()
    }

    fn next_token(&mut self) -> &mut Self {
        self.current_tok = self.peek_tok.clone();
        self.peek_tok = self.lexer.next_token();
        self
    }

    fn expect_peek(&mut self, token: Token) -> Option<&mut Self> {
        if self.peek_tok != token {
            self.push_error(format!("Expected {:?}", token).as_str());
            None
        } else {
            self.next_token();
            Some(self)
        }
    }

    fn push_error(&mut self, reason: &str) {
        self.errors.push(ParserError(reason.to_owned()));
    }

    fn peek_prec(&self) -> Precendence {
        Precendence::from_token(&self.peek_tok)
    }

    fn current_prec(&self) -> Precendence {
        Precendence::from_token(&self.current_tok)
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
        self.next_token().expect_peek(Token::Assign)?;
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
        // TODO: Parse expression
        while self.current_tok != Token::Semicolon {
            self.next_token();
        }

        Some(Stmt::Return { expr: Expr::Blank })
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expr(Precendence::Lowest)?;
        // Optional semicolon
        if self.peek_tok == Token::Semicolon {
            self.next_token();
        }
        Some(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self, precendence: Precendence) -> Option<Expr> {
        let mut left_exp = match self.current_tok {
            Token::Ident(_) => self
                .parse_ident_expr()
                .expect("Should not happen if current token is Ident"),
            Token::Int(_) => self.parse_integer()?,
            Token::True | Token::False => self
                .parse_bool()
                .expect("Should not happen if current token is True or False"),
            Token::Bang | Token::Minus => self.parse_prefix_expr()?,
            Token::Lparen => self.parse_grouped_expr()?,
            Token::If => self.parse_if_expr()?,
            Token::Function => self.parse_function_expr()?,
            _ => {
                self.push_error(
                    format!("No prefix operator {:?} found", self.current_tok).as_ref(),
                );
                return None;
            }
        };

        while self.peek_tok != Token::Semicolon && precendence < self.peek_prec() {
            left_exp = match self.peek_tok {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => self.next_token().parse_infix_expr(left_exp)?,
                Token::Lparen => self.next_token().parse_call_expr(left_exp)?,
                _ => return Some(left_exp),
            };
        }
        Some(left_exp)
    }

    fn parse_ident(&mut self) -> Option<ast::Identifier> {
        if let Token::Ident(name) = &self.current_tok {
            Some(ast::Identifier(name.to_owned()))
        } else {
            None
        }
    }

    fn parse_ident_expr(&mut self) -> Option<Expr> {
        Some(Expr::Identifier(self.parse_ident()?))
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

    fn parse_bool(&mut self) -> Option<Expr> {
        match self.current_tok {
            Token::True | Token::False => {
                Some(Expr::BooleanLiteral(self.current_tok == Token::True))
            }
            _ => None,
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        if self.current_tok != Token::Lparen {
            return None;
        }
        let expr = self.next_token().parse_expr(Precendence::Lowest)?;
        self.expect_peek(Token::Rparen)?;
        Some(expr)
    }

    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix_op = match self.current_tok {
            Token::Minus => ast::PrefixOp::Minus,
            Token::Bang => ast::PrefixOp::Bang,
            _ => return None,
        };
        let expr = self.next_token().parse_expr(Precendence::Prefix)?;
        Some(Expr::Prefix {
            op: prefix_op,
            expr: Box::new(expr),
        })
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Option<Expr> {
        macro_rules! translate_tokens {
            ($($token:ident),+) => {
                match self.current_tok {
                    $(Token::$token => ast::InfixOp::$token,)*
                    _ => return None,
                }
            };
        }
        let infix_op = translate_tokens!(Plus, Minus, Slash, Asterisk, Eq, NotEq, Lt, Gt);
        let precendence = self.current_prec();
        let right = self.next_token().parse_expr(precendence)?;
        Some(Expr::Infix {
            left: Box::new(left),
            op: infix_op,
            right: Box::new(right),
        })
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        let condition = self
            .expect_peek(Token::Lparen)?
            .next_token() // To expresion
            .parse_expr(Precendence::Lowest)?;
        let consequence = self
            .expect_peek(Token::Rparen)?
            .expect_peek(Token::Lbrace)?
            .parse_block_stmt();
        let mut alternative = ast::Block(Vec::new());
        if self.peek_tok == Token::Else {
            self.next_token().expect_peek(Token::Lbrace)?;
            alternative = self.parse_block_stmt();
        }

        return Some(Expr::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        });
    }

    fn parse_block_stmt(&mut self) -> ast::Block {
        let mut statements = ast::Block(Vec::new());
        self.next_token();
        while self.current_tok != Token::Rbrace && self.current_tok != Token::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                statements.0.push(stmt);
            }
            self.next_token();
        }
        statements
    }

    fn parse_function_expr(&mut self) -> Option<Expr> {
        let params = self.expect_peek(Token::Lparen)?.parse_function_params()?;
        let body = self.expect_peek(Token::Lbrace)?.parse_block_stmt();
        Some(Expr::Function { params, body })
    }

    fn parse_function_params(&mut self) -> Option<Vec<ast::Identifier>> {
        let mut idents = Vec::new();
        if self.peek_tok == Token::Rparen {
            self.next_token();
            return Some(idents);
        }
        let ident = match self.next_token().parse_ident() {
            Some(id) => id,
            None => {
                self.push_error("Expected Ident in function params");
                return None;
            }
        };
        idents.push(ident);

        while self.peek_tok == Token::Comma {
            let ident = match self.next_token().next_token().parse_ident() {
                Some(id) => id,
                None => {
                    self.push_error("Expected Ident in function params");
                    return None;
                }
            };
            idents.push(ident);
        }

        self.expect_peek(Token::Rparen);

        Some(idents)
    }

    fn parse_call_expr(&mut self, function: Expr) -> Option<Expr> {
        let args: Vec<Expr> = if self.peek_tok == Token::Rparen {
            self.next_token();
            Vec::new()
        } else {
            let mut args = Vec::new();
            self.next_token();
            args.push(self.parse_expr(Precendence::Lowest)?);
            while self.peek_tok == Token::Comma {
                self.next_token().next_token();
                args.push(self.parse_expr(Precendence::Lowest)?);
            }
            self.expect_peek(Token::Rparen)?;
            args
        };

        Some(Expr::Call {
            func: Box::new(function),
            args,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn no_parse_errs(parser: Parser) {
        assert_eq!(Vec::<ParserError>::new(), parser.errors);
    }

    fn errs_contain(parser: &Parser, err_msg: &str) {
        assert!(parser.errors.contains(&ParserError(err_msg.to_owned())));
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
        assert_eq!(3, program.0.len());

        let idents = ["x", "y", "foobar"];
        for (stmt, expect_ident) in program.0.iter().zip(idents) {
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
        errs_contain(&parser, "Expected identifier");
    }

    #[test]
    fn parser_throws_err_with_no_eq_sign() {
        let input = "let x 5;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        parser.parse_program();

        errs_contain(&parser, "Expected Assign");
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
        assert_eq!(3, program.0.len());

        for stmt in program.0 {
            assert!(matches!(stmt, Stmt::Return { .. }));
        }
    }

    #[test]
    fn parse_ident_expr() {
        let mut lexer = Lexer::new("foobar;");
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        no_parse_errs(parser);

        assert_eq!(1, program.0.len());
        assert_eq!(
            Stmt::Expr(Expr::Identifier(ast::Identifier("foobar".to_owned()))),
            program.0[0]
        )
    }

    #[test]
    fn parse_integer_literal() {
        let mut lexer = Lexer::new("5;");
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        no_parse_errs(parser);

        assert_eq!(1, program.0.len());
        assert_eq!(Stmt::Expr(Expr::IntegerLiteral(5)), program.0[0]);
    }

    #[test]
    fn parse_integer_errors_with_int_over_i64_limit() {
        let mut lexer = Lexer::new("92233720368547758073290;");
        let mut parser = Parser::new(&mut lexer);
        parser.parse_program();

        errs_contain(&parser, "Integer parsing failed");
    }

    #[test]
    fn parse_prefix_expr() {
        let inputs = ["!5", "-15"];
        let expected = [(ast::PrefixOp::Bang, 5), (ast::PrefixOp::Minus, 15)];

        for (input, expect) in inputs.iter().zip(expected) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);

            assert_eq!(1, program.0.len());
            assert_eq!(
                Stmt::Expr(Expr::Prefix {
                    op: expect.0.clone(),
                    expr: Box::new(Expr::IntegerLiteral(expect.1))
                }),
                program.0[0]
            )
        }
    }

    #[test]
    fn parse_infix_expr() {
        macro_rules! gen_input {
            ($($op:expr),+) => {
                [$(format!("5 {} 5", $op)),*]
            };
        }
        macro_rules! gen_expected {
            ($($op:ident),+) => {
                [$((5, ast::InfixOp::$op, 5)),*]
            };
        }
        let inputs = gen_input!("+", "-", "*", "/", ">", "<", "==", "!=");
        let expected = gen_expected!(Plus, Minus, Asterisk, Slash, Gt, Lt, Eq, NotEq);

        for (input, expect) in inputs.iter().zip(expected) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);

            assert_eq!(1, program.0.len());
            assert_eq!(
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::IntegerLiteral(expect.0)),
                    op: expect.1,
                    right: Box::new(Expr::IntegerLiteral(expect.2))
                }),
                program.0[0]
            );
        }
    }

    #[test]
    fn parse_infix_expr_with_precendence() {
        let inputs = [
            "-a * b",
            "5 < 4 == 3 > 4",
            "a * b / c",
            "!-a",
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
        ];
        let expected = [
            "((-a) * b);",
            "((5 < 4) == (3 > 4));",
            "((a * b) / c);",
            "(!(-a));",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
        ];

        for (input, expect) in inputs.iter().zip(expected) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);
            assert_eq!(1, program.0.len());

            assert_eq!(expect, program.0[0].to_string());
        }
    }

    #[test]
    fn parse_bool_literal() {
        let inputs = ["true", "false", "3 == true", "!true"];
        let expected = [
            Expr::BooleanLiteral(true),
            Expr::BooleanLiteral(false),
            Expr::Infix {
                left: Box::new(Expr::IntegerLiteral(3)),
                op: ast::InfixOp::Eq,
                right: Box::new(Expr::BooleanLiteral(true)),
            },
            Expr::Prefix {
                op: ast::PrefixOp::Bang,
                expr: Box::new(Expr::BooleanLiteral(true)),
            },
        ];

        for (input, expect) in inputs.iter().zip(expected) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);
            assert_eq!(1, program.0.len());
            assert_eq!(Stmt::Expr(expect), program.0[0]);
        }
    }

    #[test]
    fn parse_grouped_expr() {
        let inputs = ["1 + (2 + 3) + 4", "(5 + 5) * 2", "-(5 + 5)"];
        let expected = ["((1 + (2 + 3)) + 4);", "((5 + 5) * 2);", "(-(5 + 5));"];

        for (input, expect) in inputs.iter().zip(expected) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);
            assert_eq!(1, program.0.len());
            assert_eq!(expect, program.0[0].to_string());
        }
    }

    #[test]
    fn parse_grouped_expr_needs_closed_paren() {
        let input = "1 + (4 + 5";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        parser.parse_program();
        errs_contain(&parser, "Expected Rparen");
    }

    #[test]
    fn parse_if_expr() {
        let inputs = ["if (x < y) { x }", "if (x < y) { x } else { y }"];
        let expected_alts = [
            Vec::new(),
            vec![Stmt::Expr(Expr::Identifier(ast::Identifier(
                "y".to_owned(),
            )))],
        ];

        for (input, alt) in inputs.iter().zip(expected_alts) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);

            assert_eq!(1, program.0.len());
            match &program.0[0] {
                Stmt::Expr(Expr::If {
                    condition,
                    consequence,
                    alternative,
                }) => {
                    assert_eq!(
                        &Box::new(Expr::Infix {
                            left: Box::new(Expr::Identifier(ast::Identifier("x".to_owned()))),
                            op: ast::InfixOp::Lt,
                            right: Box::new(Expr::Identifier(ast::Identifier("y".to_owned())))
                        }),
                        condition,
                    );
                    assert_eq!(
                        vec![Stmt::Expr(Expr::Identifier(ast::Identifier(
                            "x".to_owned()
                        )))],
                        consequence.0
                    );
                    assert_eq!(alt, alternative.0);
                }
                _ => panic!("Did not parse an if expression"),
            }
        }
    }

    #[test]
    fn parse_func_expr() {
        let inputs = ["fn(x, y) { x + y; }", "fn() { x + y; }"];
        let expected_params = [vec!["x", "y"], Vec::new()];

        for (input, expected) in inputs.iter().zip(expected_params) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);

            assert_eq!(1, program.0.len());
            if let Stmt::Expr(Expr::Function { params, body }) = &program.0[0] {
                let expect_params: Vec<ast::Identifier> = expected
                    .iter()
                    .map(|x| ast::Identifier(x.to_owned().to_owned()))
                    .collect();
                assert_eq!(&expect_params, params);
                assert_eq!(
                    vec![Stmt::Expr(Expr::Infix {
                        left: Box::new(Expr::Identifier(ast::Identifier("x".to_owned()))),
                        op: ast::InfixOp::Plus,
                        right: Box::new(Expr::Identifier(ast::Identifier("y".to_owned())))
                    })],
                    body.0
                )
            } else {
                panic!("Did not parse function expression");
            }
        }
    }

    #[test]
    fn parse_call_expr() {
        let input = "add(1, 2 * 3)";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        no_parse_errs(parser);

        assert_eq!(1, program.0.len());
        if let Stmt::Expr(Expr::Call { func, args }) = &program.0[0] {
            assert_eq!(
                &Box::new(Expr::Identifier(ast::Identifier("add".to_owned()))),
                func
            );
            assert_eq!(
                &vec![
                    Expr::IntegerLiteral(1),
                    Expr::Infix {
                        left: Box::new(Expr::IntegerLiteral(2)),
                        op: ast::InfixOp::Asterisk,
                        right: Box::new(Expr::IntegerLiteral(3))
                    }
                ],
                args
            )
        } else {
            panic!("Did not parse call expression");
        }
    }

    #[test]
    fn parse_call_expr_operator_precedence() {
        let inputs = [
            "a + add(b * c) + d",
            "add(a + b + c * d / f + g)",
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        ];
        let expected = [
            "((a + add((b * c))) + d);",
            "add((((a + b) + ((c * d) / f)) + g));",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
        ];

        for (input, expect) in inputs.iter().zip(expected) {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            no_parse_errs(parser);

            assert_eq!(1, program.0.len());
            assert_eq!(expect, program.0[0].to_string())
        }
    }
}
