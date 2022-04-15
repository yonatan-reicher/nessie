use crate::ast::*;
use crate::chunk::{Chunk, Instruction};
use crate::r#type::{Type, TypeKind};
use crate::value::{Function, Value};
use std::collections::HashMap;
use std::mem;

pub enum CompileErrorKind {}

pub fn compile(program: &Program) -> Chunk {
    Compiler::new().compile(program)
}

type EKind = ExprKind;

type TKind = TypeKind;

type I = Instruction;

fn unary_op_instruction(op: UnaryOp) -> Instruction {
    match op {
        UnaryOp::Neg => I::Neg,
        UnaryOp::Not => I::Not,
    }
}

fn binary_op_instruction(op: BinaryOp, arg_type: &Type) -> Instruction {
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
        (B::Eq, TKind::Int) => I::IntEq,
        (B::Eq, TKind::Bool) => I::BoolEq,
        (B::Eq, TKind::String) => I::StringEq,
        (B::Eq, TKind::Function { .. }) => I::PtrEq,
        (B::Ne, TKind::Int) => I::IntNe,
        (B::Ne, TKind::Bool) => I::BoolNe,
        (B::Ne, TKind::String) => I::StringNe,
        (B::Ne, TKind::Function { .. }) => I::PtrNe,
        (B::Lt, _) => I::Lt,
        (B::Le, _) => I::Le,
        (B::Gt, _) => I::Gt,
        (B::Ge, _) => I::Ge,
        (B::Concat, _) => I::Concat,
    }
}

#[derive(Debug)]
struct Compiler {
    /// The current function being compiled. At the top level, this is just a
    /// chunk.
    compile_to: CompileTo,
    /// The locations variables declared in the current stack frame.   
    frame_variables: HashMap<UniqueName, FrameOffset>,
    /// The current offset of the top of the stack frame.
    frame_offset: FrameOffset,
}

#[derive(Debug)]
enum CompileTo {
    Chunk(Chunk),
    Function(Function),
}

type FrameOffset = usize;

/// A location that a value can be at, at runtime
#[derive(Debug)]
enum Location {
    FrameOffset(FrameOffset),
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            compile_to: CompileTo::Chunk(Chunk::new()),
            frame_variables: HashMap::new(),
            frame_offset: 0,
        }
    }

    fn chunk(&self) -> &Chunk {
        match &self.compile_to {
            CompileTo::Chunk(chunk) => chunk,
            CompileTo::Function(function) => &function.chunk,
        }
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        match &mut self.compile_to {
            CompileTo::Chunk(chunk) => chunk,
            CompileTo::Function(function) => &mut function.chunk,
        }
    }

    fn chunk_offset(&self) -> usize {
        self.chunk().instructions().len()
    }

    fn add_local(&mut self, unique_name: UniqueName) {
        self.frame_variables.insert(unique_name, self.frame_offset);
    }

    fn backpatch(&mut self, offset: usize, instruction: Instruction) {
        self.chunk_mut().instructions_mut()[offset] = instruction;
    }

    fn emit_stack_get(&mut self, ty: &Type, offset: usize, line: usize) {
        // TODO: respond if offset is bigger u16
        let offset = offset as u16;
        let instruction = match &ty.kind {
            TKind::Bool | TKind::Int => I::PrimitiveGetLocal(offset),
            TKind::String | TKind::Function { .. } => I::PtrGetLocal(offset),
        };
        self.chunk_mut().write(instruction, line);
        self.frame_offset += 1;
    }

    fn emit_stack_drop_above(&mut self, ty: &Type, line: usize) {
        let instruction = match &ty.kind {
            TKind::Bool | TKind::Int => I::PrimitiveDropAbove,
            TKind::String | TKind::Function { .. } => I::PtrDropAbove,
        };
        self.chunk_mut().write(instruction, line);
        self.frame_offset -= 1;
    }

    pub fn emit_expr(&mut self, expr: &Expr) {
        let frame_offset = self.frame_offset;
        match &expr.kind {
            &EKind::Int(int) => {
                let constant = self.chunk_mut().write_constant(Value::new_int(int));
                self.chunk_mut() .write(I::Constant(constant), expr.span.line);
            }
            EKind::True => {
                self.chunk_mut().write(I::True, expr.span.line);
            }
            EKind::False => {
                self.chunk_mut().write(I::False, expr.span.line);
            }
            EKind::String(string) => unsafe {
                let constant = self .chunk_mut() .write_constant(Value::new_string(string));
                self.chunk_mut().write(I::Constant(constant), expr.span.line);
            },
            EKind::Paren(e) => self.emit_expr(&e),
            EKind::Unary(op, e) => {
                self.emit_expr(&e);
                self.chunk_mut()
                    .write(unary_op_instruction(*op), expr.span.line);
            }
            EKind::Binary(op, l, r) => {
                // Write the two operands to the stack
                self.emit_expr(&l);
                self.emit_expr(&r);
                self.chunk_mut().write(
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
                self.emit_expr(&binding);
                // Then return the body
                self.emit_expr(&e);
                self.emit_stack_drop_above(binding.ty.as_ref().unwrap(), binding.span.line);
            }
            EKind::Var(_, unique_name) => {
                let offset = self.frame_variables[unique_name.as_ref().unwrap()];
                self.emit_stack_get(expr.ty.as_ref().unwrap(), offset, expr.span.line);
            }
            EKind::If { cond, then, else_ } => {
                // First - write the condition
                self.emit_expr(&cond);
                let cond_jmp_offset = self.chunk_offset();
                // This will be backpatched later
                self.chunk_mut().write(I::JumpIfFalse(0), expr.span.line);
                // Second - write the then branch
                let then_start_offset = self.chunk_offset();
                self.emit_expr(&then);
                let then_jmp_offset = self.chunk_offset();
                self.chunk_mut().write(I::Jump(0), expr.span.line);
                // The then branch is finished, so we can now write the
                // offset of the condition's jump
                let then_len = self.chunk_offset() - then_start_offset;
                self.backpatch(cond_jmp_offset, I::JumpIfFalse(then_len as u16));
                // Lastly - write the else branch
                let else_start_offset = self.chunk_offset();
                self.emit_expr(&else_);
                let else_len = self.chunk_offset() - else_start_offset;
                self.backpatch(then_jmp_offset, I::Jump(else_len as u16));
            }
            EKind::Function {
                arg_name: _,
                unique_arg_name,
                body,
            } => {
                // Create the function
                // TODO: add a closure
                let function = Function::new();
                let mut local_variables = HashMap::new();
                local_variables.insert(unique_arg_name.clone().unwrap(), 0);
                // Point the compiler to this function and then switch back here
                let old_compile_to = mem::replace(&mut self.compile_to, CompileTo::Function(function));
                let old_local_variables = mem::replace(&mut self.frame_variables, HashMap::new());
                let old_stack_offset = mem::replace(&mut self.frame_offset, 0);
                // Emit the body at the function's chunk
                self.emit_expr(&body);
                // Switch back
                let function = mem::replace(&mut self.compile_to, old_compile_to);
                let function = match function {
                    CompileTo::Function(function) => function,
                    _ => panic!("something went wrong during function compilation"),
                };
                self.frame_variables = old_local_variables;
                self.frame_offset = old_stack_offset;

                // Load the function as a constant
                let value = unsafe { Value::new_function(function) };
                let constant = self.chunk_mut().write_constant(value);
                self.chunk_mut().write(I::Constant(constant), expr.span.line);
            }
        }
        self.frame_offset = frame_offset + 1;
    }

    pub fn compile(mut self, program: &Program) -> Chunk {
        self.emit_expr(&program.body);
        match self.compile_to {
            CompileTo::Chunk(chunk) => chunk,
            _ => panic!("something went wrong during program compilation"),
        }
    }
}
