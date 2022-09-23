use crate::chunk::{Chunk, Instruction};
use crate::disassemble::{self, disassemble_instruction};
use crate::value::prelude::*;

type I = Instruction;

/// A virtual machine is a stack-based interpreter for a chunk of bytecode.
#[derive(Debug, Default)]
pub struct VM {
    pub stack: Vec<Value>,
    pub frame_start: usize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frame_start: 0,
        }
    }

    unsafe fn run_native_function(&mut self, function: &NativeFn) {
        // Ignore the recursion variable.
        self.stack.pop();
        // Call the real function.
        (function.function)(self.stack.last().unwrap().clone());
    }

    unsafe fn run_nessie_function(&mut self, function: &NessieFn) {
        self.run(&function.chunk);
    }

    unsafe fn run_closure(&mut self, closure: &Closure) {
        // Push the closure's captured variables.
        self.stack.extend_from_slice(&closure.captured);
        self.run(&closure.chunk);
    }

    unsafe fn run_function(&mut self, function: ManualRc<Function>) {
        // Start a new stack frame.
        let old_frame_start = self.frame_start;
        self.frame_start = self.stack.len() - 1;
        // Push the function itself for recursion.
        // The function is responsible for popping itself off the stack.
        self.stack.push(Value { function });

        match function.get() {
            Function::Nessie(nessie_fn) => self.run_nessie_function(nessie_fn),
            Function::Closure(closure) => self.run_closure(closure),
            Function::Native(native_fn) => self.run_native_function(native_fn),
        }

        // Return to the previous stack frame.
        self.frame_start = old_frame_start;
    }

    /// Executes the virtual machine.
    pub fn eval(&mut self, chunk: &Chunk) -> Value {
        self.run(chunk);
        self.stack.pop().unwrap()
    }

    /// Executes a single instruction.
    pub fn run_single(&mut self, instruction: Instruction, chunk: &Chunk, ip: &mut usize) {
        macro_rules! binary {
            ($x: tt, $field:ident, $return_field:ident) => {{
                unsafe {
                    let b = self.stack.pop().unwrap().$field;
                    let a = self.stack.pop().unwrap().$field;
                    self.stack.push(Value { $return_field: a $x b });
                }
            }}
        }

        macro_rules! unary {
            ($x: tt, $field:ident, $return_field:ident) => {{
                unsafe {
                    let a = self.stack.pop().unwrap().$field;
                    self.stack.push(Value { $return_field: $x a });
                }
            }}
        }

        macro_rules! ptr_binary {
            ($return_function:ident, |$x:ident, $y:ident| $expr:expr, $field:ident.$($get:tt)+) => {{
                unsafe {
                    let mut b = self.stack.pop().unwrap().$field;
                    let mut a = self.stack.pop().unwrap().$field;
                    let $x = a.$($get)+;
                    let $y = b.$($get)+;
                    self.stack.push(Value::$return_function($expr));
                    a.dec_ref();
                    b.dec_ref();
                }
            }};
            ($return_function:ident, $x: tt, $field:ident.$($get:tt)+) => {{
                unsafe {
                    let mut b = self.stack.pop().unwrap().$field;
                    let mut a = self.stack.pop().unwrap().$field;
                    self.stack.push(Value::$return_function(
                            a.$($get)+ $x b.$($get)+
                    ));
                    a.dec_ref();
                    b.dec_ref();
                }
            }};
        }

        macro_rules! drop_above {
            ($field: ident) => {{
                let mut value = self.stack.remove(self.stack.len() - 2);
                unsafe { value.$field.dec_ref() };
            }};
        }

        #[cfg(debug_assertions)]
        let old_ip = *ip;

        // Execute the instruction.
        match instruction {
            I::PrimitiveConstant(constant) => {
                let value = chunk.constants()[constant as usize];
                self.stack.push(value);
            }
            I::PtrConstant(constant) => {
                let mut value = chunk.constants()[constant as usize];
                // Increment the reference count.
                unsafe { value.ptr.inc_ref() };
                self.stack.push(value);
            }
            I::True => {
                self.stack.push(Value { boolean: true });
            }
            I::False => {
                self.stack.push(Value { boolean: false });
            }
            // Int operations.
            I::Add => binary!(+, int, int),
            I::Sub => binary!(-, int, int),
            I::Mul => binary!(*, int, int),
            I::Div => binary!(/, int, int),
            I::Mod => binary!(%, int, int),
            I::Neg => unary!(-, int, int),
            // Bool operations.
            I::And => binary!(&&, boolean, boolean),
            I::Or => binary!(||, boolean, boolean),
            I::Xor => binary!(^, boolean, boolean),
            I::Not => unary!(!, boolean, boolean),
            // Comparison operations.
            I::IntEq => binary!(==, int, boolean),
            I::IntNe => binary!(!=, int, boolean),
            I::BoolEq => binary!(==, boolean, boolean),
            I::BoolNe => binary!(!=, boolean, boolean),
            I::StringEq => ptr_binary!(new_boolean, ==, string.get()),
            I::StringNe => ptr_binary!(new_boolean, !=, string.get()),
            I::Lt => binary!(<, int, boolean),
            I::Le => binary!(<=, int, boolean),
            I::Gt => binary!(>, int, boolean),
            I::Ge => binary!(>=, int, boolean),
            I::Concat => ptr_binary!(new_string, |x, y| &(x.to_string() + y), string.get()),
            // Variables.
            I::PrimitiveDropAbove => {
                self.stack.remove(self.stack.len() - 2);
            }
            I::StringDropAbove => drop_above!(string),
            I::ClosureSourceDropAbove => drop_above!(closure_source),
            I::FunctionDropAbove => drop_above!(function),
            I::PrimitiveGetLocal(offset) => {
                let value = self.stack[self.frame_start + offset as usize].clone();
                self.stack.push(value);
            }
            I::PtrGetLocal(offset) => {
                let mut value = self.stack[self.frame_start + offset as usize].clone();
                unsafe { value.ptr.inc_ref() };
                self.stack.push(value);
            }
            I::Jump(offset) => {
                *ip += offset as usize;
            }
            I::JumpIfFalse(offset) => {
                let value = self.stack.pop().unwrap();
                unsafe {
                    if !value.boolean {
                        *ip += offset as usize;
                    }
                }
            }
            I::Call => unsafe {
                let value = self.stack.pop().unwrap();
                self.run_function(value.function);
            },
            I::Closure(captured_len) => {
                let mut value = unsafe { self.stack.pop().unwrap().closure_source };

                let closure_source = unsafe { value.get().clone() };
                let captured = self
                    .stack
                    .drain(self.stack.len() - captured_len as usize..)
                    .collect();
                let closure = Closure::new(closure_source, captured);
                self.stack.push(unsafe { Value::new_closure(closure) });

                unsafe { value.dec_ref() };
            }
        }
        *ip += 1;

        #[cfg(debug_assertions)]
        {
            let mut buf = Vec::new();
            disassemble_instruction(&mut buf, chunk, old_ip, &format!("{:?}", &self.stack))
                .unwrap();
            print!("{}", String::from_utf8(buf).unwrap());
        }
    }

    fn run_instructions(&mut self, instructions: &[Instruction], chunk: &Chunk) {
        #[cfg(debug_assertions)]
        {
            let mut buf = Vec::new();
            disassemble::chunk_header(&mut buf, chunk).unwrap();
            print!("{}", String::from_utf8(buf).unwrap());
            dbg!(&instructions);
            dbg!(&self.stack);
        }

        let mut ip = 0;
        while ip < instructions.len() {
            self.run_single(instructions[ip], chunk, &mut ip);
        }

        #[cfg(debug_assertions)]
        {
            print!("== end of {} == \n", chunk.name().unwrap_or("<unknown>"));
        }
    }

    pub fn run(&mut self, chunk: &Chunk) {
        self.run_instructions(chunk.instructions(), chunk);
    }

    fn drop_values(values: &[Value], drop_instructions: &[Instruction], chunk: &Chunk) {
        // Create a virtual machine to run the instructions.
        // Currently, allocating a new VM is cheap.
        let mut vm = VM::new();
        // Push the constants along with some dummy top value.
        vm.stack.extend(values.iter().rev());
        vm.stack.push(Value::new_int(0));
        // Run the drop instructions.
        vm.run_instructions(drop_instructions, chunk);
    }

    pub(crate) fn drop_chunk(chunk: &Chunk) {
        VM::drop_values(chunk.constants(), &chunk.constants_drop(), chunk);
    }

    pub(crate) fn drop_closure(closure: &Closure) {
        VM::drop_values(&closure.captured, &closure.drop_captured, &closure.chunk);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::r#type::Type;
    use crate::reporting::Report;
    use indoc::indoc;
    use std::io::{stdout, Write};

    /// This should be used instead of writing to stdout directly,
    /// because while println! are captured by the test runner,
    /// stdout is not.
    fn disassemble(chunk: &Chunk) {
        use crate::disassemble::disassemble;

        let mut stream = Vec::new();
        disassemble(&mut stream, chunk).unwrap();
        println!("{}", String::from_utf8(stream).unwrap());
    }

    fn prog(code: &str) -> Chunk {
        let tokens = crate::lexer::lex(code)
            .map_err(|e| {
                write!(stdout(), "{}", Report::from(&e).with_source(code));
            })
            .unwrap();
        let mut program = crate::parser::parse(&tokens)
            .map_err(|e| {
                for e in e {
                    write!(stdout(), "{}", Report::from(&e).with_source(code));
                }
                panic!();
            })
            .unwrap();
        crate::typecheck::Env::new()
            .typecheck(&mut program)
            .map_err(|e| {
                for e in e {
                    // write!(stdout(), "{}", Report::from(e).with_source(code));
                }
                panic!();
            })
            .unwrap();
        let chunk = crate::codegen::Compiler::new().compile(&program);
        chunk
    }

    #[test]
    fn test_push_constant() {
        let mut chunk = Chunk::new();
        let num_index = unsafe { chunk.write_constant(Value { int: 1 }, &Type::Int) };
        chunk.write(I::PrimitiveConstant(num_index), 0);

        let mut vm = VM::new();
        let val = vm.eval(&chunk);

        unsafe { assert_eq!(val.int, 1) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn simple_arithmetic() {
        let mut chunk = Chunk::new();
        unsafe {
            *chunk.name_mut() = Some("simple_arithmetic".to_string());
            let num_1_index = chunk.write_constant(Value { int: 14 }, &Type::Int);
            let num_2_index = chunk.write_constant(Value { int: 5 }, &Type::Int);
            let num_3_index = chunk.write_constant(Value { int: 3 }, &Type::Int);
            let num_4_index = chunk.write_constant(Value { int: 9 }, &Type::Int);
            chunk.write(I::PrimitiveConstant(num_1_index), 110);
            chunk.write(I::PrimitiveConstant(num_2_index), 110);
            chunk.write(I::Add, 111);
            chunk.write(I::PrimitiveConstant(num_3_index), 112);
            chunk.write(I::Div, 113);
            chunk.write(I::PrimitiveConstant(num_4_index), 114);
            chunk.write(I::Sub, 115);
        }

        disassemble(&chunk);

        let mut vm = VM::new();
        let val = vm.eval(&chunk);

        unsafe { assert_eq!(val.int, -3) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn let_program() {
        let program = prog(indoc! {"
            let a = 1 in
            let b = 2 in
            let c = a + b in
            let d = c * 2 in
            d - c
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 3) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn add_with_let() {
        let program = prog(indoc! {"
            2 + (let a = 1 in a * 3)
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 5) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn if_true() {
        let program = prog(indoc! {"
            if 1 == 1 then
                2
            else
                3
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 2) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn if_false() {
        let program = prog(indoc! {"
            if 1 == 2 then
                2
            else
                3
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 3) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn function() {
        let program = prog(indoc! {"
            let f = x: int => x + 1 in
            f(1)
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 2) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn function_with_let() {
        let program = prog(indoc! {"
            let f = (x: int =>
                let a = x + 1 in
                let b = a * 2 in
                b + a
            ) in
            f(10)
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 33) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn nested_function_call() {
        let program = prog(indoc! {"
            let f = x: int => x / 2 in
            f (f (f (f (f 40))))
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 1) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn nested_closure_call() {
        let program = prog(indoc! {"
            let f = x: int => y: int => x + y in
            f (f (f 1 2) 3) (f (f 4 5) 6)
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 21) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn nested_curried_closure_call_depth_2() {
        let program = prog(indoc! {"
            let church_zero = f: int -> int => n: int => n in
            let church_succ =
                church: (int -> int) -> int -> int =>
                f: int -> int => n: int => f (church f n)
            in
            let church_two =
                church_succ (church_succ (church_zero))
            in
            church_two (x: int => x + 1) 0
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 2) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn nested_curried_closure_call_depth_3() {
        let program = prog(indoc! {"
            let church_zero = f: int -> int => n: int => n in
            let church_succ =
                church: (int -> int) -> int -> int =>
                f: int -> int => n: int => f (church f n)
            in
            let church_three =
                church_succ (church_succ (church_succ (church_zero)))
            in
            church_three (x: int => x + 1) 0
        "});

        disassemble(&program);

        let mut vm = VM::new();
        let val = vm.eval(&program);

        unsafe { assert_eq!(val.int, 3) };
        assert_eq!(vm.stack.len(), 0);
    }
}
