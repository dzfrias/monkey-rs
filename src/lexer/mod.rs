#![allow(dead_code)]

use crate::token::Token;

#[derive(Debug)]
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    read_pos: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut lex = Self {
            input,
            pos: 0,
            read_pos: 0,
            ch: '\0',
        };
        lex.read_char();
        lex
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '\0' => Token::EOF,

            '0'..='9' => {
                return Token::Int(self.read_number());
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let literal = self.read_identifier();
                return match literal {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    _ => Token::Ident(literal.to_owned()),
                };
            }

            _ => Token::Illegal,
        };
        self.read_char();
        token
    }

    fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.read_pos).unwrap_or('\0');
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> i32 {
        let pos = self.pos;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        let str_num = &self.input[pos..self.pos];
        str_num
            .parse()
            .expect("Could not parse to i32, should not happen")
    }

    fn read_identifier(&mut self) -> &str {
        let pos = self.pos;
        while matches!(self.ch, 'a'..='z' | 'A'..='Z' | '_') {
            self.read_char();
        }
        &self.input[pos..self.pos]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);";
        let mut lex = Lexer::new(input);

        let all_expected = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
        ];

        for expected in all_expected {
            let tok = lex.next_token();
            assert_eq!(expected, tok);
        }
    }
}
