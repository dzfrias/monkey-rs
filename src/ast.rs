use std::fmt;

/// A statement represents the possible statement types in the monkey langauge.
/// There are only three, as monkey uses [expressions](Expr) for most of its
/// control flow.
#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    /// A statement of the form: `let <ident> = <expr>;`
    Let { ident: Identifier, expr: Expr },
    /// A statement of the form: `return <expr>;`
    Return { expr: Expr },
    /// A wrapper for [expressions](Expr), makes it possible to put
    /// expressions on their own line
    Expr(Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let { ident, expr } => write!(f, "let {} = {};", ident, expr),
            Stmt::Return { expr } => write!(f, "return {};", expr),
            Stmt::Expr(expr) => write!(f, "{};", expr),
        }
    }
}

/// An expression in the monkey language.
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    /// An identifier
    Identifier(Identifier),
    /// An 64 bit integer literal
    IntegerLiteral(i64),
    /// A boolean literal
    BooleanLiteral(bool),
    /// Operators prefixing other expressions, like ! or -
    Prefix { op: PrefixOp, expr: Box<Expr> },
    /// Operators between two expressions, like + or /
    Infix {
        left: Box<Expr>,
        op: InfixOp,
        right: Box<Expr>,
    },
    /// An if condition of the form: `if (<expr>) { <block> } else { <block> }`.
    /// The else block is optional
    If {
        condition: Box<Expr>,
        consequence: Block,
        alternative: Block,
    },
    /// A function of the form: `fn(<param1>, ..., <paramN>) { <block> }`.
    /// Functions are first class
    Function {
        params: Vec<Identifier>,
        body: Block,
    },
    /// A function call of the form: `<expr>(<arg1>, ..., <argN>) { <block> }`.
    /// The expression can be anything that evaluates to a function.
    Call { func: Box<Expr>, args: Vec<Expr> },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Identifier(Identifier(name)) => write!(f, "{name}"),
            Expr::IntegerLiteral(int) => write!(f, "{int}"),
            Expr::BooleanLiteral(b) => write!(f, "{b}"),
            Expr::Prefix { op, expr } => write!(f, "({op}{expr})"),
            Expr::Infix { left, op, right } => write!(f, "({left} {op} {right})"),
            Expr::If {
                condition,
                consequence,
                alternative,
            } => {
                if alternative.0.is_empty() {
                    write!(f, "if {condition} {{ {consequence} }}")
                } else {
                    write!(
                        f,
                        "if {condition} {{ {consequence} }} else {{ {alternative} }}"
                    )
                }
            }
            Expr::Function { params, body } => {
                let joined = params
                    .iter()
                    .map(|ident| ident.to_string() + ", ")
                    .collect::<String>();
                write!(
                    f,
                    "fn({}) {{ {body} }}",
                    joined
                        .strip_suffix(", ")
                        .expect("Should always have a trailing ', '")
                )
            }
            Expr::Call { func, args } => {
                let joined = args
                    .iter()
                    .map(|expr| expr.to_string() + ", ")
                    .collect::<String>();
                write!(
                    f,
                    "{func}({})",
                    joined
                        .strip_suffix(", ")
                        .expect("Should always have a trailing ', '")
                )
            }
        }
    }
}

/// A prefix operator that goes before an expression.
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum PrefixOp {
    /// Negative operator
    Minus,
    /// Not operator
    Bang,
    /// Positive operator
    Plus,
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrefixOp::Minus => write!(f, "-"),
            PrefixOp::Bang => write!(f, "!"),
            PrefixOp::Plus => write!(f, "+"),
        }
    }
}

/// An infix operator that goes between two expressions.
#[derive(Debug, PartialEq, Eq)]
pub enum InfixOp {
    /// Addition operator
    Plus,
    /// Subtraction operator
    Minus,
    /// Multiplication operator
    Asterisk,
    /// Division operator
    Slash,
    /// Greater-than operator
    Gt,
    /// Less-than operator
    Lt,
    /// Equal operator
    Eq,
    /// Inequality operator
    NotEq,
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InfixOp::Plus => write!(f, "+"),
            InfixOp::Minus => write!(f, "-"),
            InfixOp::Asterisk => write!(f, "*"),
            InfixOp::Slash => write!(f, "/"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Eq => write!(f, "=="),
            InfixOp::NotEq => write!(f, "!="),
        }
    }
}

/// An identifier in the monkey language.
#[derive(Debug, PartialEq, Eq)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A block representing a series of statements.
#[derive(Debug, PartialEq, Eq)]
pub struct Block(pub Vec<Stmt>);
impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let to_str = self
            .0
            .iter()
            .map(|stmt| stmt.to_string() + " ")
            .collect::<String>();
        write!(
            f,
            "{}",
            to_str
                .strip_suffix(' ')
                .expect("Should always have a trailing ' '")
        )
    }
}

/// A special type of block at the root of the program.
pub type Program = Block;
