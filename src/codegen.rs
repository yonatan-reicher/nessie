use crate::ast::*;
use crate::chunk::{Chunk, Instruction};


fn unary_op_instruction(op: UnaryOp) -> Instruction {
    match op {
        UnaryOp::Neg => Instruction::Neg,
    }
}

fn binary_op_instruction(op: BinaryOp) -> Instruction {
    match op {
        BinaryOp::Add => Instruction::Add,
        BinaryOp::Sub => Instruction::Sub,
        BinaryOp::Mul => Instruction::Mul,
        BinaryOp::Div => Instruction::Div,
        BinaryOp::Mod => Instruction::Mod,
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
        &ExprKind::Int(i) => {
            let constant = chunk.write_constant(i);
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

