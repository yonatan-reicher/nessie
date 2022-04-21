use crate::chunk::{Chunk, Instruction};
use crate::disassemble::disassamble_instruction;
use crate::value::{Value, NessieFn, NativeFn, Function};

type I = Instruction;

/// A virtual machine is a stack-based interpreter for a chunk of bytecode.
pub struct VM {
    pub stack: Vec<Value>,
    pub frame_start: usize,
    debug_stream: Option<Box<dyn std::io::Write>>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frame_start: 0,
            debug_stream: None,
        }
    }

    /// Sets the debug stream for the virtual machine.
    pub fn set_debug_stream(&mut self, stream: Box<dyn std::io::Write>) {
        self.debug_stream = Some(stream);
    }

    /// Clear the debug stream for the virtual machine.
    pub fn clear_debug_stream(&mut self) {
        self.debug_stream = None;
    }

    unsafe fn run_nessie_function(&mut self, function: &NessieFn) {
        let old_frame_start = self.frame_start;
        self.frame_start = self.stack.len() - 1;
        self.run(&function.chunk);
        self.frame_start = old_frame_start;
    }

    unsafe fn run_native_function(&mut self, function: &NativeFn) {
        (function.function)(self.stack.last().unwrap().clone());
    }

    unsafe fn run_function(&mut self, function: &Function) {
        match function {
            Function::Nessie(nessie_fn) => self.run_nessie_function(nessie_fn),
            Function::Native(native_fn) => self.run_native_function(native_fn),
        }
    }

    /// Executes the virtual machine.
    pub fn run(&mut self, chunk: &Chunk) {
        let mut ip = 0;
        while ip < chunk.instructions().len() {
            let instruction = chunk.instructions()[ip];
            self.run_single(instruction, chunk, &mut ip);
        }
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

        // print debug information
        if let Some(ref mut debug_stream) = self.debug_stream {
            let stack_string = {
                let mut s = String::new();
                // print the stack
                s.push_str("[");
                for (i, value) in self.stack.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{:?}", value));
                }
                s.push_str("]");
                s
            };
            disassamble_instruction(debug_stream, chunk, *ip, &stack_string).unwrap();
        }

        // execute the instruction
        match instruction {
            I::Constant(constant) => {
                let value = chunk.constants()[constant as usize];
                self.stack.push(value);
            }
            I::True => {
                self.stack.push(Value { boolean: true });
            }
            I::False => {
                self.stack.push(Value { boolean: false });
            }
            I::Return => {
                let value = self.stack.pop().unwrap();
                println!("{:?}", value);
            }
            // int operations
            I::Add => binary!(+, int, int),
            I::Sub => binary!(-, int, int),
            I::Mul => binary!(*, int, int),
            I::Div => binary!(/, int, int),
            I::Mod => binary!(%, int, int),
            I::Neg => unary!(-, int, int),
            // bool operations
            I::And => binary!(&&, boolean, boolean),
            I::Or => binary!(||, boolean, boolean),
            I::Xor => binary!(^, boolean, boolean),
            I::Not => unary!(!, boolean, boolean),
            // comparison operations
            I::IntEq => binary!(==, int, boolean),
            I::IntNe => binary!(!=, int, boolean),
            I::BoolEq => binary!(==, boolean, boolean),
            I::BoolNe => binary!(!=, boolean, boolean),
            I::StringEq => ptr_binary!(new_boolean, ==, string.get()),
            I::StringNe => ptr_binary!(new_boolean, !=, string.get()),
            I::PtrEq => ptr_binary!(new_boolean, ==, ptr.ptr()),
            I::PtrNe => ptr_binary!(new_boolean, !=, ptr.ptr()),
            I::Lt => binary!(<, int, boolean),
            I::Le => binary!(<=, int, boolean),
            I::Gt => binary!(>, int, boolean),
            I::Ge => binary!(>=, int, boolean),
            I::Concat => ptr_binary!(new_string, |x, y| &(x.to_string() + y), string.get()),
            // Variables
            I::PrimitiveDropAbove => {
                let top = self.stack.pop().unwrap();
                // take the primitive value out
                self.stack.pop();
                // push back the top of the stack
                self.stack.push(top);
            }
            I::PtrDropAbove => {
                let top = self.stack.pop().unwrap();
                // take the string out
                let mut string_value = self.stack.pop().unwrap();
                // decrease reference count
                unsafe { string_value.string.dec_ref() };
                // place the top back
                self.stack.push(top);
            }
            I::PrimitiveGetLocal(offset) => {
                let value = self.stack[self.frame_start + offset as usize].clone();
                self.stack.push(value);
            }
            I::PtrGetLocal(offset) => {
                let mut string_value = self.stack[self.frame_start + offset as usize].clone();
                unsafe { string_value.string.inc_ref() };
                self.stack.push(string_value);
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
            I::Call => {
                let mut function = unsafe { self.stack.pop().unwrap().function };
                unsafe { self.run_function(function.get()) };
                unsafe { function.dec_ref() };
            }
        }
        *ip += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use crate::source_error::SourceError;

    /// This should be used instead of writing to stdout directly,
    /// because while println! are captured by the test runner,
    /// stdout is not.
    fn disassamble(chunk: &Chunk, name: &str) {
        use crate::disassemble::disassamble;

        let mut stream = Vec::new();
        disassamble(&mut stream, chunk, name).unwrap();
        println!("{}", String::from_utf8(stream).unwrap());
    }


    fn prog(code: &str) -> Chunk {
        let tokens = crate::lexer::lex(code).map_err(|e| {
            println!("{}", e.with_source(code))
        }).unwrap();
        let mut program = crate::parser::parse(&tokens).map_err(|e| {
            for e in e { 
                println!("{}", e.with_source(code));
            }
            panic!();
        }).unwrap();
        crate::typecheck::typecheck(&mut program).map_err(|e| {
            for e in e {
                println!("{}", e.with_source(code));
            }
            panic!();
        }).unwrap();
        let chunk = crate::codegen::compile(&program);
        println!("{:?}", chunk);
        chunk
    }

    #[test]
    fn test_push_constant() {
        let mut chunk = Chunk::new();
        let num_index = chunk.write_constant(Value { int: 1 });
        chunk.write(I::Constant(num_index), 0);

        let mut vm = VM::new();
        vm.run(&chunk);

        unsafe { assert_eq!(vm.stack.pop().unwrap().int, 1) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn simple_arithmetic() {
        let mut chunk = Chunk::new();
        let num_1_index = chunk.write_constant(Value { int: 14 });
        let num_2_index = chunk.write_constant(Value { int: 5 });
        let num_3_index = chunk.write_constant(Value { int: 3 });
        let num_4_index = chunk.write_constant(Value { int: 9 });
        chunk.write(I::Constant(num_1_index), 110);
        chunk.write(I::Constant(num_2_index), 110);
        chunk.write(I::Add, 111);
        chunk.write(I::Constant(num_3_index), 112);
        chunk.write(I::Div, 113);
        chunk.write(I::Constant(num_4_index), 114);
        chunk.write(I::Sub, 115);

        disassamble(&chunk, "simple_arithmetic");

        let mut vm = VM::new();
        vm.run(&chunk);

        unsafe { assert_eq!(vm.stack.pop().unwrap().int, -3) };
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

        disassamble(&program, "let_program");

        let mut vm = VM::new();
        vm.run(&program);

        unsafe { assert_eq!(vm.stack.pop().unwrap().int, 3) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn add_with_let() {
        let program = prog(indoc! {"
            2 + (let a = 1 in a * 3)
        "});

        disassamble(&program, "add_with_let");

        let mut vm = VM::new();
        vm.run(&program);

        unsafe { assert_eq!(vm.stack.pop().unwrap().int, 5) };
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

        disassamble(&program, "if_true");

        let mut vm = VM::new();
        vm.run(&program);

        unsafe { assert_eq!(vm.stack.pop().unwrap().int, 2) };
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

        disassamble(&program, "if_false");

        let mut vm = VM::new();
        vm.run(&program);

        unsafe { assert_eq!(vm.stack.pop().unwrap().int, 3) };
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn function() {
        let program = prog(indoc! {"
            let f = x: int => x + 1 in
            f == f
        "});

        disassamble(&program, "function");

        let mut vm = VM::new();
        vm.run(&program);

        unsafe { assert_eq!(vm.stack.pop().unwrap().boolean, true) };
        assert_eq!(vm.stack.len(), 0);
    }
}
