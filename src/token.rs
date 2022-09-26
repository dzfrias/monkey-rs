/// A token in the Monkey language.
#[derive(Debug, PartialEq)]
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

    // Pairs
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}
