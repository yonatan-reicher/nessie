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
    match (op, &arg_type.kind) {
        (BinaryOp::Add, _) => Instruction::Add,
        (BinaryOp::Sub, _) => Instruction::Sub,
        (BinaryOp::Mul, _) => Instruction::Mul,
        (BinaryOp::Div, _) => Instruction::Div,
        (BinaryOp::Mod, _) => Instruction::Mod,
        (BinaryOp::Or , _)=> Instruction::Or,
        (BinaryOp::And, _) => Instruction::And,
        (BinaryOp::Xor, _) => Instruction::Xor,
        (BinaryOp::Eq, TypeKind::Int) => Instruction::IntEq,
        (BinaryOp::Eq, TypeKind::Bool) => Instruction::BoolEq,
        (BinaryOp::Ne, TypeKind::Int) => Instruction::IntNe,
        (BinaryOp::Ne, TypeKind::Bool) => Instruction::BoolNe,
        (BinaryOp::Lt, _) => Instruction::Lt,
        (BinaryOp::Le, _) => Instruction::Le,
        (BinaryOp::Gt, _) => Instruction::Gt,
        (BinaryOp::Ge, _) => Instruction::Ge,
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

