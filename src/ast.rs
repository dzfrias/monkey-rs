use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let { ident: Identifier, expr: Expr },
    Return { expr: Expr },
    // Wrapper for expressions, makes it possible to put expressions on their own line
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

#[derive(Debug, PartialEq)]
pub enum Expr {
    Blank,
    Identifier(Identifier),
    IntegerLiteral(i64),
    Prefix { op: PrefixOp, expr: Box<Expr> },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Blank => write!(f, "BLANK"),
            Expr::Identifier(Identifier(name)) => write!(f, "{name}"),
            Expr::IntegerLiteral(int) => write!(f, "{int}"),
            Expr::Prefix { op, expr } => write!(f, "{op}{expr}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOp {
    Minus,
    Bang,
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrefixOp::Minus => write!(f, "-"),
            PrefixOp::Bang => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type Block = Vec<Stmt>;

pub type Program = Block;
