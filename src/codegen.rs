use crate::ast::*;
use crate::chunk::{Chunk, Instruction};
use crate::r#type::{Type, TypeKind};
use crate::value::prelude::*;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use vec1::Vec1;

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

#[derive(Debug, Clone)]
enum CompileTo {
    Chunk(Chunk),
    Closure {
        chunk: Chunk,
        captured_variables: Vec<UniqueName>,
    },
}

type FrameOffset = usize;

#[derive(Debug, Default, Clone)]
struct Frame {
    /// The current function being compiled. At the top level, this is just a
    /// chunk.
    compile_to: CompileTo,
    /// The locations variables declared in the current stack frame.   
    locals: HashMap<UniqueName, FrameOffset>,
    /// The current offset of the top of the stack frame.
    offset: FrameOffset,
}

#[derive(Debug, Default, Clone)]
pub struct Compiler {
    frames: Vec1<Frame>,
}

impl Default for CompileTo {
    fn default() -> Self {
        CompileTo::Chunk(Chunk::new())
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    /// The current frame being compiled.
    fn frame(&self) -> &Frame {
        self.frames.last()
    }

    /// Mutable reference to the current frame being compiled.
    fn frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut()
    }

    /// The offset from the top of the stack frame to the current value.
    fn offset(&self) -> FrameOffset {
        self.frame().offset
    }

    /// The offset from the top of the stack frame to the current value.
    fn offset_mut(&mut self) -> &mut FrameOffset {
        &mut self.frame_mut().offset
    }

    /// The current chunk being compiled.
    fn chunk(&self) -> &Chunk {
        match &self.frame().compile_to {
            CompileTo::Chunk(chunk) => chunk,
            CompileTo::Closure { chunk, .. } => chunk,
        }
    }

    /// Mutable reference to the current chunk being compiled.
    fn chunk_mut(&mut self) -> &mut Chunk {
        match &mut self.frame_mut().compile_to {
            CompileTo::Chunk(chunk) => chunk,
            CompileTo::Closure { chunk, .. } => chunk,
        }
    }

    fn chunk_offset(&self) -> usize {
        self.chunk().instructions().len()
    }

    /// Should be called before emitting the code to push it's initial value.
    fn add_local(&mut self, unique_name: UniqueName) {
        let offset = self.offset();
        self.frame_mut().locals.insert(unique_name, offset);
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
        self.frame_mut().offset += 1;
    }

    fn emit_stack_drop_above(&mut self, ty: &Type, line: usize) {
        let instruction = match &ty.kind {
            TKind::Bool | TKind::Int => I::PrimitiveDropAbove,
            TKind::String | TKind::Function { .. } => I::PtrDropAbove,
        };
        self.chunk_mut().write(instruction, line);
        self.frame_mut().offset -= 1;
    }

    fn free_unique_names(&self, expr: &Expr) -> HashMap<UniqueName, Type> {
        match &expr.kind {
            EKind::Int(_) | EKind::True | EKind::False | EKind::String(_) => HashMap::new(),
            EKind::Var(_, unique_name) => {
                let mut names = HashMap::new();
                names.insert(unique_name.clone().unwrap(), expr.ty.clone().unwrap());
                names
            }
            EKind::Unary(_, e) | ExprKind::Paren(e) => self.free_unique_names(e),
            EKind::Binary(_, left, right)
            | EKind::App {
                func: left,
                arg: right,
            } => {
                let mut names = self.free_unique_names(left);
                names.extend(self.free_unique_names(right));
                names
            }
            EKind::Let {
                name: _,
                unique_name,
                binding,
                expr,
            } => {
                let mut names = self.free_unique_names(expr);
                names.remove(unique_name.as_ref().unwrap());
                names.extend(self.free_unique_names(binding));
                names
            }
            EKind::If { cond, then, else_ } => {
                let mut names = self.free_unique_names(cond);
                names.extend(self.free_unique_names(then));
                names.extend(self.free_unique_names(else_));
                names
            }
            EKind::Function { arg, body } => {
                let mut names = self.free_unique_names(body);
                names.remove(arg.unique_name.as_ref().unwrap());
                names
            }
        }
    }

    pub fn emit_expr(&mut self, expr: &Expr) {
        let frame_offset = self.frame().offset;
        match &expr.kind {
            &EKind::Int(int) => {
                let constant = self.chunk_mut().write_constant(Value::new_int(int));
                self.chunk_mut()
                    .write(I::Constant(constant), expr.span.line);
            }
            EKind::True => {
                self.chunk_mut().write(I::True, expr.span.line);
            }
            EKind::False => {
                self.chunk_mut().write(I::False, expr.span.line);
            }
            EKind::String(string) => unsafe {
                let constant = self.chunk_mut().write_constant(Value::new_string(string));
                self.chunk_mut()
                    .write(I::Constant(constant), expr.span.line);
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
                self.frame_mut().offset -= 1;
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
                let offset = self.frame().locals[unique_name.as_ref().unwrap()];
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
                arg:
                    NameDeclaration {
                        unique_name: unique_arg_name,
                        ty: arg_type,
                        ..
                    },
                body,
            } => {
                let captured_variables: Vec<_> =
                    self.free_unique_names(&expr).into_iter().collect();

                // Compile a new stack frame
                self.frames.push({
                    let mut frame = Frame {
                        locals: HashMap::new(),
                        offset: captured_variables.len() + 1,
                        compile_to: CompileTo::Closure {
                            chunk: Chunk::new(),
                            captured_variables: captured_variables
                                .iter()
                                .map(|x| x.0.clone())
                                .collect(),
                        },
                    };
                    frame.locals.insert(unique_arg_name.clone().unwrap(), 0);
                    for (i, (name, _)) in captured_variables.iter().enumerate() {
                        frame.locals.insert(name.clone(), i + 1);
                    }
                    frame
                });

                // Emit the body at the function's chunk
                self.emit_expr(&body);
                // Drop the captured variables and the parameter
                for (_, ty) in captured_variables.iter().rev() {
                    self.emit_stack_drop_above(ty, expr.span.line);
                }
                self.emit_stack_drop_above(arg_type.as_ref().unwrap(), body.span.line);

                // Return the function
                let frame = self.frames.pop().unwrap();
                let chunk = match frame.compile_to {
                    CompileTo::Closure { chunk, .. } => chunk,
                    _ => panic!("Expected closure"),
                };

                // Load the function as a constant
                if captured_variables.is_empty() {
                    let value = unsafe { Value::new_function(NessieFn { chunk }) };
                    let constant = self.chunk_mut().write_constant(value);
                    self.chunk_mut()
                        .write(I::Constant(constant), expr.span.line);
                } else {
                    // first, load the captured variables in order
                    for (name, ty) in &captured_variables {
                        let offset = self.frame().locals[name];
                        self.emit_stack_get(ty, offset, expr.span.line);
                    }
                    // then, load the function
                    let value = unsafe {
                        Value::new_closure(Closure {
                            chunk: Rc::new(chunk),
                            captured: vec![],
                        })
                    };
                    let constant = self.chunk_mut().write_constant(value);
                    self.chunk_mut()
                        .write(I::Constant(constant), expr.span.line);
                    // Finally, produce the closure
                    self.chunk_mut()
                        .write(I::Closure(captured_variables.len() as u16), expr.span.line);
                }
            }
            EKind::App { func, arg } => {
                // Then - write the argument
                self.emit_expr(&arg);
                // First - write the function
                self.emit_expr(&func);
                // Then - call the function
                self.chunk_mut().write(I::Call, expr.span.line);
            }
        }
        *self.offset_mut() = frame_offset + 1;
    }

    pub fn compile(&mut self, program: &Program) -> Chunk {
        self.emit_expr(&program.body);
        *self.offset_mut() -= 1;
        let chunk = std::mem::replace(&mut self.frame_mut().compile_to, CompileTo::default());
        match chunk {
            CompileTo::Chunk(chunk) => chunk,
            _ => panic!("something went wrong during program compilation"),
        }
    }

    pub fn declare(&mut self, unique_name: UniqueName, ty: Type) {
        self.add_local(unique_name);
        *self.offset_mut() += 1;
    }
}
