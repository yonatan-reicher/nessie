//! This module typechecks abstract syntax trees and resolve certain things
//! like identifiers

use crate::ast::*;
use crate::r#type::Type;
use crate::reporting::annotation::{Located, Region};
use crate::reporting::error::TypeError as Error;
use std::collections::HashMap;
use std::rc::Rc;
use vec1::*;

type EKind = ExprKind;

#[derive(Debug, Clone)]
pub struct Env {
    errors: Vec<Located<Error>>,
    locals: HashMap<Rc<str>, Vec1<Local>>,
    type_locals: HashMap<Rc<str>, Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
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
        Neg => f(Type::Int, Type::Int),
        Not => f(Type::Bool, Type::Bool),
    }
}

fn binary_op_types(op: BinaryOp) -> OperatorSignature {
    use BinaryOp::*;
    let f = OperatorSignature::simple;

    match op {
        Add => f(Type::Int, Type::Int),
        Sub => f(Type::Int, Type::Int),
        Mul => f(Type::Int, Type::Int),
        Div => f(Type::Int, Type::Int),
        Mod => f(Type::Int, Type::Int),
        And => f(Type::Bool, Type::Bool),
        Or => f(Type::Bool, Type::Bool),
        Xor => f(Type::Bool, Type::Bool),
        Lt => f(Type::Int, Type::Bool),
        Gt => f(Type::Int, Type::Bool),
        Le => f(Type::Int, Type::Bool),
        Ge => f(Type::Int, Type::Bool),
        Eq => OperatorSignature::Comparison,
        Ne => OperatorSignature::Comparison,
        Concat => f(Type::String, Type::String),
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            locals: HashMap::new(),
            type_locals: [
                (Rc::from("int"), Type::Int),
                (Rc::from("bool"), Type::Bool),
                (Rc::from("string"), Type::String),
            ]
            .iter()
            .cloned()
            .collect(),
        }
    }

    fn report_error(&mut self, region: Region, error: Error) {
        self.errors.push(Located {
            region,
            value: error,
        });
    }

    pub fn eval_type_expr(&mut self, expr: &Located<TypeExpr>) -> Result<Type, ()> {
        match &expr.value {
            TypeExpr::Var(name) => {
                if let Some(ty) = self.type_locals.get(name) {
                    Ok(ty.clone())
                } else {
                    self.report_error(expr.region, Error::UndefinedVariable(name.clone()));
                    Err(())
                }
            }
            TypeExpr::Function(left, right) => {
                let left = self.eval_type_expr(left);
                let right = self.eval_type_expr(right);
                Ok(Type::Function {
                    arg: Rc::new(left?),
                    ret: Rc::new(right?),
                })
            }
            TypeExpr::Paren(expr) => self.eval_type_expr(expr),
        }
    }

    pub fn typecheck(&mut self, program: &mut Program) -> Result<(), Vec<Located<Error>>> {
        let _ = self.visit(&mut program.body);

        // if program.body.ty.is_none() {
        //     self.report_error(program.body.span, Error::ProgramTypeUnknown);
        // }

        if self.errors.is_empty() {
            Ok(())
        } else {
            let errors = std::mem::replace(&mut self.errors, vec![]);
            Err(errors)
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
                expr.ty = Some(Type::Bool.clone());
            }
            ExprKind::Int(_) => {
                expr.ty = Some(Type::Int.clone());
            }
            ExprKind::String(_) => {
                expr.ty = Some(Type::String.clone());
            }
            ExprKind::Paren(e) => {
                self.visit(e)?;
                expr.ty = e.ty.clone();
            }
            ExprKind::Unary(op, e) => match unary_op_types(*op) {
                OperatorSignature::Comparison => {
                    expr.ty = Some(Type::Bool.clone());
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
                        expr.ty = Some(Type::Bool.clone());
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
            }
            ExprKind::Var(name, unique_name) => {
                if let Some(local) = self.locals.get(name) {
                    expr.ty = Some(local.last().ty.clone());
                    *unique_name = self.get_unique_name(name.clone());
                } else {
                    self.report_error(expr.span, Error::UndefinedVariable(name.clone()));
                }
            }
            ExprKind::If { cond, then, else_ } => {
                let _ = self.expect_visit(cond, Type::Bool);
                self.visit(then)?;
                self.expect_visit(else_, then.ty.clone().unwrap())?;
                expr.ty = then.ty.clone();
            }
            EKind::Function {
                arg:
                    NameDeclaration {
                        name: arg_name,
                        unique_name: arg_unique_name,
                        type_expr: arg_type_expr,
                        ty: arg_type,
                    },
                recursion_var,
                body,
            } => {
                // Just until there is real type inference
                if arg_type_expr.is_none() {
                    self.report_error(expr.span, Error::UnknownType);
                    Err(())?;
                }
                let _ = arg_type.insert(
                    self.eval_type_expr(arg_type_expr.as_ref().unwrap())?
                        .clone(),
                );
                *arg_unique_name = Some(self.declare_local(
                    arg_name,
                    Local {
                        ty: arg_type.clone().unwrap(),
                    },
                ));
                *recursion_var = Some(self.declare_local(
                    &Rc::from("recurse"),
                    Local {
                        ty: Type::Function {
                            arg: Rc::new(arg_type.clone().unwrap()),
                            // Just for now! Assume the return type is `int`.
                            ret: Rc::new(Type::Int.clone()),
                        },
                    },
                ));

                self.visit(body)?;
                self.undeclare_local(arg_name);
                expr.ty = Some(Type::Function {
                    arg: Rc::new(arg_type.clone().unwrap()),
                    ret: Rc::new(body.ty.clone().unwrap()),
                });
            }
            EKind::App { func, arg } => {
                let _ = self.visit(func);
                match func.ty.as_ref() {
                    Some(Type::Function {
                        arg: arg_type,
                        ret: ret_type,
                    }) => {
                        let _ = self.expect_visit(arg, arg_type.as_ref().clone());
                        expr.ty = Some(ret_type.as_ref().clone());
                    }
                    Some(_) => {
                        self.report_error(expr.span, Error::NotAFunction(func.ty.clone().unwrap()));
                        Err(())?;
                    }
                    None => {
                        Err(())?;
                    }
                }
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
                self.report_error(
                    expr.span,
                    Error::OperatorTypeMissmatch {
                        expected: ty.clone(),
                        actual: expr_ty.clone(),
                    },
                );
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

    fn get_unique_name(&self, name: Rc<str>) -> Option<UniqueName> {
        let shadow_count = self.locals.get(&name)?.len() - 1;
        Some(UniqueName { name, shadow_count })
    }

    pub fn declare(&mut self, name: Rc<str>, ty: Type) -> UniqueName {
        self.declare_local(&name, Local { ty })
    }
}
