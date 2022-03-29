use crate::ast::*;
use crate::chunk::{Chunk, Instruction};
use crate::value::Value;
use crate::r#type::{Type, TypeKind};


pub enum CompileErrorKind {
}

fn unary_op_instruction(op: UnaryOp) -> Instruction {
    match op {
        UnaryOp::Neg => Instruction::Neg,
        UnaryOp::Not => Instruction::Not,
    }
}

fn binary_op_instruction(op: BinaryOp, arg_type: &Type) -> Instruction {
    type I = Instruction;
    type B = BinaryOp;
    match (op, &arg_type.kind) {
        (B::Add, _) => I::Add,
        (B::Sub, _) => I::Sub,
        (B::Mul, _) => I::Mul,
        (B::Div, _) => I::Div,
        (B::Mod, _) => I::Mod,
        (B::Or , _)=> I::Or,
        (B::And, _) => I::And,
        (B::Xor, _) => I::Xor,
        (B::Eq, TypeKind::Int) => I::IntEq,
        (B::Eq, TypeKind::Bool) => I::BoolEq,
        (B::Eq, TypeKind::String) => I::StringEq,
        (B::Ne, TypeKind::Int) => I::IntNe,
        (B::Ne, TypeKind::Bool) => I::BoolNe,
        (B::Ne, TypeKind::String) => I::StringNe,
        (B::Lt, _) => I::Lt,
        (B::Le, _) => I::Le,
        (B::Gt, _) => I::Gt,
        (B::Ge, _) => I::Ge,
        (B::Concat, _) => I::Concat,
    }
}


pub fn compile(program: &Program) -> Chunk {
    let mut chunk = Chunk::new();
    emit_expr(&program.body, &mut chunk);
    // chunk.write(Instruction::Return, program.body.span.end.line);
    chunk
}

pub fn emit_expr(expr: &Expr, chunk: &mut Chunk) {
    match &expr.kind {
        &ExprKind::Int(int) => {
            let constant = chunk.write_constant(Value { int });
            chunk.write(Instruction::Constant(constant), expr.span.line);
        }
        ExprKind::True => {
            chunk.write(Instruction::True, expr.span.line);
        }
        ExprKind::False => {
            chunk.write(Instruction::False, expr.span.line);
        }
        ExprKind::String(string) => {
            unsafe {
                let constant = chunk.write_constant(Value::new_string(string));
                chunk.write(Instruction::Constant(constant), expr.span.line);
            }
        }
        ExprKind::Paren(e) => {
            emit_expr(&e, chunk)
        }
        ExprKind::Unary(op, e) => {
            emit_expr(&e, chunk);
            chunk.write(unary_op_instruction(*op), expr.span.line);
        }
        ExprKind::Binary(op, l, r) => {
            emit_expr(&l, chunk);
            emit_expr(&r, chunk);
            chunk.write(binary_op_instruction(*op, l.ty.as_ref().unwrap()), expr.span.line);
        }
    }
}

