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
    BooleanLiteral(bool),
    Prefix {
        op: PrefixOp,
        expr: Box<Expr>,
    },
    Infix {
        left: Box<Expr>,
        op: InfixOp,
        right: Box<Expr>,
    },
    If {
        condition: Box<Expr>,
        consequence: Block,
        alternative: Block,
    },
    Function {
        params: Vec<Identifier>,
        body: Block,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Blank => write!(f, "BLANK"),
            Expr::Identifier(Identifier(name)) => write!(f, "{name}"),
            Expr::IntegerLiteral(int) => write!(f, "{int}"),
            Expr::BooleanLiteral(b) => write!(f, "{b}"),
            Expr::Prefix { op, expr } => write!(f, "({op}{expr})"),
            Expr::Infix { left, op, right } => write!(f, "({left} {op} {right})"),
            // TODO: Print out if and function expr
            Expr::If { .. } => todo!(),
            Expr::Function { .. } => todo!(),
            Expr::Call { func, args } => {
                let mut joined = String::new();
                for expr in args {
                    joined.push_str(expr.to_string().as_str());
                    joined.push_str(", ");
                }
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
pub enum InfixOp {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Gt,
    Lt,
    Eq,
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

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type Block = Vec<Stmt>;

pub type Program = Block;
