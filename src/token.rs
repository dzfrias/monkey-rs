#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers
    Ident(String),
    Int(i32),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}
