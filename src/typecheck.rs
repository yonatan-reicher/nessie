//! This module typechecks abstract syntax trees and resolve certain things
//! like identifiers

use crate::ast::*;
use crate::r#type::Type;
use crate::source_error::SourceError;
use crate::token::Span;
use std::collections::HashMap;
use std::rc::Rc;
use vec1::*;

#[derive(Debug)]
pub enum TypeErrorKind {
    OperatorTypeMissmatch { expected: Type, found: Type },
    ProgramTypeUnknown,
    UndefinedVariable(Rc<str>),
}

impl std::fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TypeErrorKind::*;
        match self {
            OperatorTypeMissmatch { expected, found } => {
                write!(
                    f,
                    "Operator argument should have type {} but had type {}",
                    expected, found,
                )
            }
            ProgramTypeUnknown => {
                write!(f, "The program's type could not be infered")
            }
            UndefinedVariable(name) => {
                write!(f, "The variable '{name}' does not exist in this context")
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
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for TypeError {}

impl SourceError for TypeError {
    fn get_span(&self) -> Span {
        self.span
    }
}

pub fn typecheck(program: &mut Program) -> Result<(), Vec<TypeError>> {
    Env::new().typecheck(program)
}

struct Env {
    errors: Vec<TypeError>,
    locals: HashMap<Rc<str>, Vec1<Local>>,
}

#[derive(Debug)]
struct Local {
    ty: Type,
}

enum OperatorSignature {
    Simple { args: Type, ret: Type },
    Comparison,
}

impl OperatorSignature {
    pub fn simple(args: Type, ret: Type) -> Self {
        Self::Simple { args, ret }
    }
}

fn unary_op_types(op: UnaryOp) -> OperatorSignature {
    use UnaryOp::*;
    let f = OperatorSignature::simple;

    match op {
        Neg => f(Type::INT, Type::INT),
        Not => f(Type::BOOL, Type::BOOL),
    }
}

fn binary_op_types(op: BinaryOp) -> OperatorSignature {
    use BinaryOp::*;
    let f = OperatorSignature::simple;

    match op {
        Add => f(Type::INT, Type::INT),
        Sub => f(Type::INT, Type::INT),
        Mul => f(Type::INT, Type::INT),
        Div => f(Type::INT, Type::INT),
        Mod => f(Type::INT, Type::INT),
        And => f(Type::BOOL, Type::BOOL),
        Or => f(Type::BOOL, Type::BOOL),
        Xor => f(Type::BOOL, Type::BOOL),
        Lt => f(Type::INT, Type::BOOL),
        Gt => f(Type::INT, Type::BOOL),
        Le => f(Type::INT, Type::BOOL),
        Ge => f(Type::INT, Type::BOOL),
        Eq => OperatorSignature::Comparison,
        Ne => OperatorSignature::Comparison,
        Concat => f(Type::STRING, Type::STRING),
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            locals: HashMap::new(),
        }
    }

    pub fn typecheck(mut self, program: &mut Program) -> Result<(), Vec<TypeError>> {
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

    /// Visits an expression but ignores it's errors if it's type was not infered
    fn visit_or_rollback(&mut self, expr: &mut Expr) -> Result<(), ()> {
        let old_len = self.errors.len();
        let res = self.visit(expr);
        if expr.ty.is_none() && self.errors.len() != old_len {
            // Rollback
            self.errors.truncate(old_len);
        }
        res
    }

    fn visit(&mut self, expr: &mut Expr) -> Result<(), ()> {
        match &mut expr.kind {
            ExprKind::True | ExprKind::False => {
                expr.ty = Some(Type::BOOL.clone());
            }
            ExprKind::Int(_) => {
                expr.ty = Some(Type::INT.clone());
            }
            ExprKind::String(_) => {
                expr.ty = Some(Type::STRING.clone());
            }
            ExprKind::Paren(e) => {
                self.visit(e)?;
                expr.ty = e.ty.clone();
            }
            ExprKind::Unary(op, e) => match unary_op_types(*op) {
                OperatorSignature::Comparison => {
                    expr.ty = Some(Type::BOOL.clone());
                    let _ = self.visit(e);
                }
                OperatorSignature::Simple { args, ret } => {
                    expr.ty = Some(ret.clone());
                    let _ = self.expect_visit(e, args);
                }
            },
            ExprKind::Binary(op, l, r) => {
                match binary_op_types(*op) {
                    OperatorSignature::Simple { args, ret } => {
                        expr.ty = Some(ret);
                        let _ = self.expect_visit(l, args.clone());
                        let _ = self.expect_visit(r, args);
                    }
                    OperatorSignature::Comparison => {
                        expr.ty = Some(Type::BOOL.clone());
                        // Try visiting the left node and then the right node
                        // If the left node is not infered, then try the right
                        // one and come back to the left one
                        let _ = self.visit_or_rollback(l);
                        if let Some(l_type) = l.ty.clone() {
                            let _ = self.expect_visit(r, l_type);
                        } else {
                            let _ = self.visit(r);
                            if let Some(r_type) = r.ty.clone() {
                                let _ = self.expect_visit(l, r_type);
                            }
                        }
                    }
                }
            }
            ExprKind::Let {
                name,
                unique_name,
                binding,
                expr: body,
            } => {
                self.visit(binding)?;
                *unique_name = Some(self.declare_local(
                    name,
                    Local {
                        ty: binding.ty.clone().unwrap(),
                    },
                ));
                self.visit(body)?;
                self.undeclare_local(name);
                expr.ty = body.ty.clone();
                // set the unique name
            }
            ExprKind::Var(name, unique_name) => {
                if let Some(local) = self.locals.get(name) {
                    expr.ty = Some(local.last().ty.clone());
                    *unique_name = self.get_unique_name(name.clone());
                } else {
                    self.errors.push(TypeError {
                        kind: TypeErrorKind::UndefinedVariable(name.clone()),
                        span: expr.span,
                    })
                }
            }
            ExprKind::If { cond, then, else_ } => {
                let _ = self.expect_visit(cond, Type::BOOL);
                self.visit(then)?;
                self.expect_visit(else_, then.ty.clone().unwrap())?;
                expr.ty = then.ty.clone();
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
            }
            _ => (),
        }

        expr.ty = Some(ty);

        Ok(())
    }

    fn declare_local(&mut self, name: &Rc<str>, local: Local) -> UniqueName {
        let shadow_count = if let Some(with_the_same_name) = self.locals.get_mut(name) {
            with_the_same_name.push(local);
            with_the_same_name.len() - 1
        } else {
            self.locals.insert(name.clone(), vec1![local]);
            0
        };
        UniqueName {
            name: name.clone(),
            shadow_count,
        }
    }

    fn undeclare_local(&mut self, name: &Rc<str>) {
        if let Some(vars) = self.locals.get_mut(name) {
            let res = vars.pop();
            if res.is_ok() {
                return;
            }
        }
        self.locals.remove(name);
    }

    fn make_unique_name(&self, name: Rc<str>) -> UniqueName {
        let shadow_count = self.locals.get(&name).map(Vec1::len).unwrap_or(0);
        UniqueName { name, shadow_count }
    }

    fn get_unique_name(&self, name: Rc<str>) -> Option<UniqueName> {
        let shadow_count = self.locals.get(&name)?.len() - 1;
        Some(UniqueName { name, shadow_count })
    }
}
