#![allow(dead_code)]

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let { ident: Identifier, expr: Expr },
    Return { expr: Expr },
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Blank,
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

pub type Block = Vec<Stmt>;

pub type Program = Block;
