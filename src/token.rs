/// A token in the Monkey language.
#[derive(Debug, PartialEq)]
pub enum Token {
    /// An illegal character.
    Illegal,
    /// A token for when the end of the input is reached.
    EOF,

    /// An identifier, supports all alphabetic unicode characters, "_", and numbers.
    Ident(String),
    /// An integer.
    Int(String),

    /// An operator token represented by "=".
    Assign,
    /// An operator token represented by "+".
    Plus,
    /// An operator token represented by "-".
    Minus,
    /// An operator token represented by "!".
    Bang,
    /// An operator token represented by "*".
    Asterisk,
    /// An operator token represented by "/".
    Slash,

    /// An operator token represented by "<".
    Lt,
    /// An operator token represented by ">".
    Gt,
    /// An operator token represented by "==".
    Eq,
    /// An operator token represented by "!=".
    NotEq,

    /// A token represented by ",".
    Comma,
    /// A token represented by ";".
    Semicolon,

    /// A token represented by "(".
    Lparen,
    /// A token represented by ")".
    Rparen,
    /// A token represented by "{".
    Lbrace,
    /// A token represented by "}".
    Rbrace,

    /// A keyword token represented by "fn".
    Function,
    /// A keyword token represented by "let".
    Let,
    /// A keyword token represented by "true".
    True,
    /// A keyword token represented by "false".
    False,
    /// A keyword token represented by "if".
    If,
    /// A keyword token represented by "else".
    Else,
    /// A keyword token represented by "return".
    Return,
}
