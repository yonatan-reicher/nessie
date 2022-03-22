//! This module typechecks abstract syntax trees and resolve certain things
//! like identifiers

use crate::lexer::Span;
use crate::ast::*;
use crate::r#type::Type;


#[derive(Debug)]
pub enum TypeErrorKind {
    OperatorTypeMissmatch {
        expected: Type,
        found: Type,
    },
    ProgramTypeUnknown,
}

impl std::fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TypeErrorKind::*;
        match self {
            OperatorTypeMissmatch { expected, found } => {
                write!(f,
                    "Operator argument should have type {} but had type {}",
                    expected, found,
                )
            }
            ProgramTypeUnknown => {
                write!(f, "The program's type could not be infered")
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "line {} col {} to line {} col {}: {}",
            self.span.start.line, self.span.start.column,
            self.span.end.line, self.span.end.column,
            self.kind
        )
    }
}

impl std::error::Error for TypeError { }


pub fn typecheck(program: &mut Program) -> Result<(), Vec<TypeError>> {
    Env::new().typecheck(program)
}


struct Env {
    pub errors: Vec<TypeError>,
}

struct OperatorSignature {
    pub args: Type,
    pub ret: Type,
}

impl OperatorSignature {
    pub fn new(args: Type, ret: Type) -> Self {
        Self { args, ret }
    }
}

fn unary_op_types(op: UnaryOp) -> OperatorSignature {
    use UnaryOp::*;
    let f = OperatorSignature::new;

    match op {
        Neg => f(Type::INT, Type::INT),
        Not => f(Type::BOOL, Type::BOOL),
    }
}

fn binary_op_types(op: BinaryOp) -> OperatorSignature {
    use BinaryOp::*;
    let f = OperatorSignature::new;

    match op {
        Add => f(Type::INT, Type::INT),
        Sub => f(Type::INT, Type::INT),
        Mul => f(Type::INT, Type::INT),
        Div => f(Type::INT, Type::INT),
        Mod => f(Type::INT, Type::INT),
        And => f(Type::BOOL, Type::BOOL),
        Or  => f(Type::BOOL, Type::BOOL),
        Xor => f(Type::BOOL, Type::BOOL),
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
        }
    }

    pub fn typecheck(mut self, program: &mut Program)
    -> Result<(), Vec<TypeError>> {
        let _ = self.visit(&mut program.body);

        if program.body.ty.is_none() {
            self.errors.push(TypeError {
                kind: TypeErrorKind::ProgramTypeUnknown,
                span: program.body.span,
            });
        }
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }

    pub fn visit(&mut self, expr: &mut Expr) -> Result<(), ()> {
        match &mut expr.kind {
            ExprKind::True | ExprKind::False => {
                expr.ty = Some(Type::BOOL.clone());
            }
            ExprKind::Int(_) => {
                expr.ty = Some(Type::INT.clone());
            }
            ExprKind::Paren(e) => {
                self.visit(e)?;
                expr.ty = e.ty.clone();
            }
            ExprKind::Unary(op, e) => {
                let OperatorSignature { args, ret } = unary_op_types(*op);
                expr.ty = Some(ret);
                let _ = self.expect_visit(e, args);
            }   
            ExprKind::BinaryOp(op, l, r) => {
                let OperatorSignature { args, ret } = binary_op_types(*op);
                expr.ty = Some(ret);
                let _ = self.expect_visit(l, args.clone());
                let _ = self.expect_visit(r, args);
            }
        }
        Ok(())
    }

    pub fn expect_visit(&mut self, expr: &mut Expr, ty: Type) -> Result<(), ()> {
        // First try calculating the expression's type
        self.visit(expr)?;
        // Then check it.
        // Only check if the type is different, because even if the
        // expression's type could not be infered, we already know it
        match &expr.ty {
            Some(expr_ty) if expr_ty != &ty => {
                self.errors.push(TypeError {
                    kind: TypeErrorKind::OperatorTypeMissmatch {
                        expected: ty.clone(),
                        found: expr_ty.clone(),
                    },
                    span: expr.span,
                });
            },
            _ => (),
        }

        expr.ty = Some(ty);
        
        Ok(())
    }
}

