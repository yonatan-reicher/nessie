//! This module contains the AST definitions for the Nessie Language.
//! The AST is a tree-like structure that represents the structure of the
//! source code and potentially contains semantic information.

use crate::r#type::Type;
use crate::token::Span;
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// An integer literal
    Int(i32),
    /// A boolean true literal
    True,
    /// A boolean false literal
    False,
    /// A string literal
    String(Rc<str>),
    /// A binary operator expression.
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    /// A unary operator expression.
    Unary(UnaryOp, Box<Expr>),
    /// An expression wrapped in parentheses.
    Paren(Box<Expr>),
    /// A `let x = e in E` expression.
    Let {
        name: Rc<str>,
        unique_name: Option<UniqueName>,
        binding: Box<Expr>,
        expr: Box<Expr>,
    },
    /// A variable
    Var(Rc<str>, Option<UniqueName>),
    /// A conditional expression
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniqueName {
    /// The original name in the source code
    pub name: Rc<str>,
    /// The amount of identifiers this one is shadowing
    pub shadow_count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Int operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Boolean operators
    And,
    Or,
    Xor,
    // String operators
    Concat,
    // Comparison operators
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    // Int operators
    Neg,
    // Boolean operators
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr {
    pub ty: TypeExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExprKind {
    Int,
    Bool,
}
