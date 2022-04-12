use crate::ast::*;
use crate::chunk::{Chunk, Instruction};
use crate::r#type::{Type, TypeKind};
use crate::value::Value;
use std::collections::HashMap;

pub enum CompileErrorKind {}

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
        (B::Or, _) => I::Or,
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
    Compiler::new().compile(program)
}

#[derive(Debug)]
struct Compiler {
    variables: HashMap<UniqueName, Location>,
    frame_offset: usize,
}

/// A location that a value can be at, at runtime
#[derive(Debug)]
enum Location {
    FrameOffset(usize),
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            variables: HashMap::new(),
            frame_offset: 0,
        }
    }

    pub fn compile(&mut self, program: &Program) -> Chunk {
        let mut chunk = Chunk::new();
        self.emit_expr(&program.body, &mut chunk);
        chunk
    }

    pub fn emit_expr(&mut self, expr: &Expr, chunk: &mut Chunk) {
        type EKind = ExprKind;

        let frame_offset = self.frame_offset;
        match &expr.kind {
            &EKind::Int(int) => {
                let constant = chunk.write_constant(Value { int });
                chunk.write(Instruction::Constant(constant), expr.span.line);
            }
            EKind::True => {
                chunk.write(Instruction::True, expr.span.line);
            }
            EKind::False => {
                chunk.write(Instruction::False, expr.span.line);
            }
            EKind::String(string) => unsafe {
                let constant = chunk.write_constant(Value::new_string(string));
                chunk.write(Instruction::Constant(constant), expr.span.line);
            },
            EKind::Paren(e) => self.emit_expr(&e, chunk),
            EKind::Unary(op, e) => {
                self.emit_expr(&e, chunk);
                chunk.write(unary_op_instruction(*op), expr.span.line);
            }
            EKind::Binary(op, l, r) => {
                // Write the two operands to the stack
                self.emit_expr(&l, chunk);
                self.emit_expr(&r, chunk);
                chunk.write(
                    binary_op_instruction(*op, l.ty.as_ref().unwrap()),
                    expr.span.line,
                );
                // After the binary operation, the stack has been reduced by one
                self.frame_offset -= 1;
            }
            EKind::Let {
                name: _,
                unique_name,
                binding,
                expr: e,
            } => {
                // Place the result of `binding` on the stack.
                // That location will be the address of the new local variable.
                self.add_local(unique_name.clone().unwrap());
                self.emit_expr(&binding, chunk);
                // Then return the body
                self.emit_expr(&e, chunk);
                self.emit_stack_drop_above(binding.ty.as_ref().unwrap(), binding.span.line, chunk);
            }
            EKind::Var(_, unique_name) => {
                match self.variables[unique_name.as_ref().unwrap()] {
                    Location::FrameOffset(offset) => {
                        self.emit_stack_get(
                            expr.ty.as_ref().unwrap(),
                            offset,
                            expr.span.line,
                            chunk,
                        );
                    }
                }
            }
            EKind::If {
                cond,
                then,
                else_,
            } => {
                self.emit_expr(&cond, chunk);
                let cond_jmp_index = chunk.instructions().len();
                chunk.write(Instruction::JumpIfFalse(0), expr.span.line);
                let (then_len, jmp_index) = {
                    let instructions_start_length = chunk.instructions().len();
                    self.emit_expr(&then, chunk);
                    let jmp_index = chunk.instructions().len();
                    chunk.write(Instruction::Jump(0), expr.span.line);
                    (chunk.instructions().len() - instructions_start_length, jmp_index)
                };
                let else_len = {
                    let instructions_start_length = chunk.instructions().len();
                    self.emit_expr(&else_, chunk);
                    chunk.instructions().len() - instructions_start_length
                };
                chunk.instructions_mut()[cond_jmp_index] = Instruction::JumpIfFalse(then_len as u16);
                chunk.instructions_mut()[jmp_index] = Instruction::Jump(else_len as u16);
            }
        }
        self.frame_offset = frame_offset + 1;
    }

    fn add_local(&mut self, unique_name: UniqueName) {
        self.variables.insert(unique_name, Location::FrameOffset(self.frame_offset));
    }

    fn emit_stack_drop_above(&mut self, ty: &Type, line: usize, chunk: &mut Chunk) {
        match &ty.kind {
            TypeKind::Bool | TypeKind::Int => {
                chunk.write(Instruction::PrimitiveDropAbove, line);
            }
            TypeKind::String => {
                chunk.write(Instruction::StringDropAbove, line);
            }
        }
        self.frame_offset -= 1;
    }

    fn emit_stack_get(&self, ty: &Type, offset: usize, line: usize, chunk: &mut Chunk) {
        match &ty.kind {
            TypeKind::Bool | TypeKind::Int => {
                chunk.write(Instruction::PrimitiveGetLocal(offset as _), line);
            }
            TypeKind::String => {
                chunk.write(Instruction::StringGetLocal(offset as _), line);
            }
        }
    }
}
