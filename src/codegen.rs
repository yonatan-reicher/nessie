use crate::ast::*;
use crate::chunk::{Chunk, Instruction};
use crate::value::Value;


fn unary_op_instruction(op: UnaryOp) -> Instruction {
    match op {
        UnaryOp::Neg => Instruction::Neg,
        UnaryOp::Not => Instruction::Not,
    }
}

fn binary_op_instruction(op: BinaryOp) -> Instruction {
    match op {
        BinaryOp::Add => Instruction::Add,
        BinaryOp::Sub => Instruction::Sub,
        BinaryOp::Mul => Instruction::Mul,
        BinaryOp::Div => Instruction::Div,
        BinaryOp::Mod => Instruction::Mod,
        BinaryOp::Or => Instruction::Or,
        BinaryOp::And => Instruction::And,
        BinaryOp::Xor => Instruction::Xor,
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
            chunk.write(Instruction::Constant(constant), expr.span.start.line);
        }
        &ExprKind::Bool(bool) => {
            let constant = chunk.write_constant(Value { bool });
            chunk.write(Instruction::Constant(constant), expr.span.start.line);
        }
        ExprKind::Paren(e) => {
            emit_expr(&e, chunk)
        }
        ExprKind::Unary(op, e) => {
            emit_expr(&e, chunk);
            chunk.write(unary_op_instruction(*op), expr.span.start.line);
        }
        ExprKind::BinaryOp(op, l, r) => {
            emit_expr(&l, chunk);
            emit_expr(&r, chunk);
            chunk.write(binary_op_instruction(*op), expr.span.start.line);
        }
    }
}

