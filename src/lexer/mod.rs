#![allow(dead_code)]

use crate::token::Token;

/// A lexer that can an input sting and turn it into a stream of Monkey
/// [tokens](crate::token::Token).
#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    read_pos: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer` with an input and reads the first character.
    /// # Examples
    /// ```
    /// use monkey_rs::lexer::Lexer;
    /// let mut lexer = Lexer::new("1234");
    /// ```
    pub fn new(input: &'a str) -> Self {
        let mut lex = Self {
            input,
            pos: 0,
            read_pos: 0,
            ch: '\0',
        };
        lex.read_char();
        lex
    }

    /// Return the next token found in the lexer's input. It returns
    /// [Token::EOF](crate::token::Token::EOF) when the end of the input is exceeded or reached.
    ///
    /// # Examples
    /// ```
    /// use monkey_rs::lexer::Lexer;
    /// use monkey_rs::token::Token;
    ///
    /// let mut lexer = Lexer::new("let x = 42;");
    /// assert_eq!(lexer.next_token(), Token::Let);
    /// assert_eq!(lexer.next_token(), Token::Ident("x".to_owned()));
    /// assert_eq!(lexer.next_token(), Token::Assign);
    /// assert_eq!(lexer.next_token(), Token::Int("42".to_owned()));
    /// assert_eq!(lexer.next_token(), Token::Semicolon);
    /// assert_eq!(lexer.next_token(), Token::EOF);
    /// ```
    pub fn next_token(&mut self) -> Token {
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

            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    let literal = self.read_identifier();
                    return match literal.as_str() {
                        "fn" => Token::Function,
                        "let" => Token::Let,
                        _ => Token::Ident(literal.to_owned()),
                    };
                }
                Token::Illegal
            }
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

    fn read_number(&mut self) -> String {
        let pos = self.pos;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        self.slice(pos, self.pos)
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;
        while self.ch.is_alphabetic() || self.ch == '_' {
            self.read_char();
        }
        self.slice(pos, self.pos)
    }

    fn slice(&self, start: usize, end: usize) -> String {
        let substr = self.input.chars().take(end).skip(start).collect::<String>();
        substr
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let 五 = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(五, ten);";
        let mut lex = Lexer::new(input);

        let all_expected = vec![
            Token::Let,
            Token::Ident("五".to_owned()),
            Token::Assign,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::Int("10".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::Lparen,
            Token::Ident("五".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::Rparen,
            Token::Semicolon,
        ];

        for expected in all_expected {
            let tok = lex.next_token();
            assert_eq!(expected, tok);
        }
    }
}
