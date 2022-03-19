//! This module contains the AST definitions for the Nessie Language.
//! The AST is a tree-like structure that represents the structure of the
//! source code and potentially contains semantic information.

use crate::lexer::Span;


#[derive(Debug)]
pub struct Program {
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Option<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// An integer literal
    Int(i32),
    /// A binary operator expression.
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    /// A unary operator expression.
    Unary(UnaryOp, Box<Expr>),
    /// An expression wrapped in parentheses.
    Paren(Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr {
    pub ty: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Int,
}

