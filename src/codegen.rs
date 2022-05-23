use crate::reporting::annotation::Line;
use crate::ast::*;
use crate::chunk::{Chunk, Instruction};
use crate::r#type::prelude::*;
use crate::value::prelude::*;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use vec1::Vec1;

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
        (B::Eq, TKind::Function { .. }) => todo!("dont allow this"),
        (B::Ne, TKind::Int) => I::IntNe,
        (B::Ne, TKind::Bool) => I::BoolNe,
        (B::Ne, TKind::String) => I::StringNe,
        (B::Ne, TKind::Function { .. }) => todo!("dont allow this"),
        (B::Lt, _) => I::Lt,
        (B::Le, _) => I::Le,
        (B::Gt, _) => I::Gt,
        (B::Ge, _) => I::Ge,
        (B::Concat, _) => I::Concat,
        (_, TKind::ClosureSource) => todo!("dont allow this"),
    }
}

type FrameOffset = usize;

#[derive(Debug, Default)]
struct Frame {
    /// The current chunk being compiled.
    chunk: Chunk,
    /// The locations variables declared in the current stack frame.   
    locals: HashMap<UniqueName, FrameOffset>,
    /// The current offset of the top of the stack frame.
    offset: FrameOffset,
}

#[derive(Debug, Default)]
pub struct Compiler {
    frames: Vec1<Frame>,
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
        &self.frame().chunk
    }

    /// Mutable reference to the current chunk being compiled.
    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.frame_mut().chunk
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

    fn emit_stack_get(&mut self, ty: &Type, offset: usize, line: Line) {
        // TODO: respond if offset is bigger u16
        let offset = offset as u16;
        let instruction = match ty.category() {
            Category::Primitive => I::PrimitiveGetLocal(offset),
            Category::Pointer => I::PtrGetLocal(offset),
        };
        self.chunk_mut().write(instruction, line);
        self.frame_mut().offset += 1;
    }

    fn emit_stack_drop_above(&mut self, ty: &Type, line: Line) {
        for &inst in ty.drop_above() {
            self.chunk_mut().write(inst, line);
        }
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
            EKind::Function { arg, recursion_var, body } => {
                let mut names = self.free_unique_names(body);
                names.remove(arg.unique_name.as_ref().unwrap());
                names.remove(recursion_var.as_ref().unwrap());
                names
            }
        }
    }

    pub fn emit_expr(&mut self, expr: &Expr) {
        let frame_offset = self.frame().offset;
        match &expr.kind {
            &EKind::Int(int) => {
                let constant = unsafe {
                    self.chunk_mut().write_constant(Value::new_int(int), &Type::INT)
                };
                self.chunk_mut()
                    .write(I::PrimitiveConstant(constant), expr.span.0.line);
            }
            EKind::True => {
                self.chunk_mut().write(I::True, expr.span.0.line);
            }
            EKind::False => {
                self.chunk_mut().write(I::False, expr.span.0.line);
            }
            EKind::String(string) => {
                let constant = unsafe { 
                    self.chunk_mut().write_constant(Value::new_string(string), &Type::STRING)
                };
                self.chunk_mut()
                    .write(I::PtrConstant(constant), expr.span.0.line);
            },
            EKind::Paren(e) => self.emit_expr(&e),
            EKind::Unary(op, e) => {
                self.emit_expr(&e);
                self.chunk_mut()
                    .write(unary_op_instruction(*op), expr.span.0.line);
            }
            EKind::Binary(op, l, r) => {
                // Write the two operands to the stack
                self.emit_expr(&l);
                self.emit_expr(&r);
                self.chunk_mut().write(
                    binary_op_instruction(*op, l.ty.as_ref().unwrap()),
                    expr.span.0.line,
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
                self.emit_stack_drop_above(binding.ty.as_ref().unwrap(), binding.span.0.line);
            }
            EKind::Var(_, unique_name) => {
                let offset = self.frame().locals[unique_name.as_ref().unwrap()];
                self.emit_stack_get(expr.ty.as_ref().unwrap(), offset, expr.span.0.line);
            }
            EKind::If { cond, then, else_ } => {
                // First - write the condition
                self.emit_expr(&cond);
                let cond_jmp_offset = self.chunk_offset();
                // This will be backpatched later
                self.chunk_mut().write(I::JumpIfFalse(0), expr.span.0.line);
                // Second - write the then branch
                let then_start_offset = self.chunk_offset();
                self.emit_expr(&then);
                let then_jmp_offset = self.chunk_offset();
                self.chunk_mut().write(I::Jump(0), expr.span.0.line);
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
                recursion_var,
                body,
            } => {
                let captured_variables: Vec<_> =
                    self.free_unique_names(&expr).into_iter().collect();

                // First compile the destructor
                let mut drop_captured = Vec::new();
                // When the destructor is called, there is some arbitrary value
                // at the top of the stack and bellow it, at the base of the
                // call frame, all the captured variables.
                for (_, ty) in captured_variables.iter() {
                    drop_captured.extend(ty.drop_above());
                }
                // Move it to a shared reference
                let drop_captured: Rc<[_]> = Rc::from(drop_captured);

                // Compile a new stack frame
                self.frames.push({
                    let mut frame = Frame::default();
                    *frame.chunk.name_mut() = Some(format!("{} => ...", unique_arg_name.as_ref().unwrap().name));
                    // make place for the argument, recursion variable and
                    // the captured variables
                    frame.offset += captured_variables.len() + 2;
                    // declare the parameter and the captured variables as
                    // locals
                    frame.locals.insert(unique_arg_name.clone().unwrap(), 0);
                    frame.locals.insert(recursion_var.clone().unwrap(), 1);
                    for (i, (name, _)) in captured_variables.iter().enumerate() {
                        frame.locals.insert(name.clone(), i + 2);
                    }
                    frame
                });

                // Emit the body at the function's chunk
                self.emit_expr(&body);
                // Drop the captured variables and the parameter
                for _ in captured_variables.iter().rev() {
                    // self.emit_stack_drop_above(ty, expr.span.0.line);
                    // hack: we want to clear the stack without dropping the
                    // values (because they are owned by the closure and have
                    // are weakly referenced on the stack). We use a primitive
                    // drop for this.
                    self.chunk_mut().write(I::PrimitiveDropAbove, expr.span.0.line);
                }
                // Drop the recursion variable.
                self.emit_stack_drop_above(expr.ty.as_ref().unwrap(), expr.span.0.line);
                // Drop the argument.
                self.emit_stack_drop_above(arg_type.as_ref().unwrap(), body.span.0.line);

                // Get the compiled chunk
                let chunk = self.frames.pop().unwrap().chunk;

                // Load the function as a constant
                if captured_variables.is_empty() {
                    let value = unsafe { Value::new_function(NessieFn { chunk }) };
                    let constant = unsafe {
                        self.chunk_mut().write_constant(value, &expr.ty.as_ref().unwrap())
                    };
                    self.chunk_mut()
                        .write(I::PtrConstant(constant), expr.span.0.line);
                } else {
                    // first, load the captured variables in order
                    for (name, ty) in &captured_variables {
                        let offset = self.frame().locals[name];
                        self.emit_stack_get(ty, offset, expr.span.0.line);
                    }
                    // then, load the function
                    let value = unsafe {
                        Value::new_closure_source(ClosureSource {
                            chunk: Rc::new(chunk),
                            drop_captured,
                        })
                    };
                    let constant = unsafe {
                        self.chunk_mut().write_constant(value, &Type::CLOSURE_SOURCE)
                    };
                    self.chunk_mut()
                        .write(I::PtrConstant(constant), expr.span.0.line);
                    // Finally, produce the closure
                    self.chunk_mut()
                        .write(I::Closure(captured_variables.len() as u16), expr.span.0.line);
                }
            }
            EKind::App { func, arg } => {
                // Then - write the argument
                self.emit_expr(&arg);
                // First - write the function
                self.emit_expr(&func);
                // Then - call the function
                self.chunk_mut().write(I::Call, expr.span.0.line);
            }
        }
        *self.offset_mut() = frame_offset + 1;
    }

    pub fn compile(&mut self, program: &Program) -> Chunk {
        self.emit_expr(&program.body);
        *self.offset_mut() -= 1;
        let frame = mem::replace(self.frames.first_mut(), Frame::default());
        self.frame_mut().locals = frame.locals;
        frame.chunk
    }

    pub fn declare(&mut self, unique_name: UniqueName, _ty: Type) {
        self.add_local(unique_name);
        *self.offset_mut() += 1;
    }
}
