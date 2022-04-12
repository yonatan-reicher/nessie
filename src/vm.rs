use crate::chunk::{Chunk, Instruction};
use crate::disassemble::disassamble_instruction;
use crate::value::Value;

/// A virtual machine is a stack-based interpreter for a chunk of bytecode.
pub struct VM {
    pub stack: Vec<Value>,
    debug_stream: Option<Box<dyn std::io::Write>>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
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
        macro_rules! binary_operation {
            ($x: tt, $field:ident, $return_field:ident) => {{
                unsafe {
                    let b = self.stack.pop().unwrap().$field;
                    let a = self.stack.pop().unwrap().$field;
                    self.stack.push(Value { $return_field: a $x b });
                }
            }}
        }

        macro_rules! unary_operation {
            ($x: tt, $field:ident, $return_field:ident) => {{
                unsafe {
                    let a = self.stack.pop().unwrap().$field;
                    self.stack.push(Value { $return_field: $x a });
                }
            }}
        }

        // execute the instruction
        match instruction {
            Instruction::Constant(constant) => {
                let value = chunk.constants()[constant as usize];
                self.stack.push(value);
            }
            Instruction::True => {
                self.stack.push(Value { boolean: true });
            }
            Instruction::False => {
                self.stack.push(Value { boolean: false });
            }
            Instruction::Return => {
                let value = self.stack.pop().unwrap();
                println!("{:?}", value);
            }
            // int operations
            Instruction::Add => binary_operation!(+, int, int),
            Instruction::Sub => binary_operation!(-, int, int),
            Instruction::Mul => binary_operation!(*, int, int),
            Instruction::Div => binary_operation!(/, int, int),
            Instruction::Mod => binary_operation!(%, int, int),
            Instruction::Neg => unary_operation!(-, int, int),
            // bool operations
            Instruction::And => binary_operation!(&&, boolean, boolean),
            Instruction::Or => binary_operation!(||, boolean, boolean),
            Instruction::Xor => binary_operation!(^, boolean, boolean),
            Instruction::Not => unary_operation!(!, boolean, boolean),
            // comparison operations
            Instruction::IntEq => binary_operation!(==, int, boolean),
            Instruction::BoolEq => binary_operation!(==, boolean, boolean),
            Instruction::IntNe => binary_operation!(!=, int, boolean),
            Instruction::BoolNe => binary_operation!(!=, boolean, boolean),
            Instruction::Lt => binary_operation!(<, int, boolean),
            Instruction::Le => binary_operation!(<=, int, boolean),
            Instruction::Gt => binary_operation!(>, int, boolean),
            Instruction::Ge => binary_operation!(>=, int, boolean),
            // string equality is implemented differently
            Instruction::StringEq => unsafe {
                let mut b = self.stack.pop().unwrap().string;
                let mut a = self.stack.pop().unwrap().string;
                self.stack.push(Value {
                    boolean: a.get() == b.get(),
                });
                a.dec_ref();
                b.dec_ref();
            },
            Instruction::StringNe => unsafe {
                let mut b = self.stack.pop().unwrap().string;
                let mut a = self.stack.pop().unwrap().string;
                self.stack.push(Value {
                    boolean: a.get() != b.get(),
                });
                a.dec_ref();
                b.dec_ref();
            },
            Instruction::Concat => unsafe {
                let mut b = self.stack.pop().unwrap().string;
                let mut a = self.stack.pop().unwrap().string;
                let concatinated = a.get().to_string() + b.get();
                self.stack.push(Value::new_string(&concatinated));
                a.dec_ref();
                b.dec_ref();
            },
            // Variables
            Instruction::PrimitiveDropAbove => {
                let top = self.stack.pop().unwrap();
                // take the primitive value out
                self.stack.pop();
                // push back the top of the stack
                self.stack.push(top);
            }
            Instruction::StringDropAbove => {
                let top = self.stack.pop().unwrap();
                // take the string out
                let mut string_value = self.stack.pop().unwrap();
                // decrease reference count
                unsafe { string_value.string.dec_ref() };
                // place the top back
                self.stack.push(top);
            }
            Instruction::PrimitiveGetLocal(offset) => {
                let value = self.stack[offset as usize].clone();
                self.stack.push(value);
            }
            Instruction::StringGetLocal(offset) => {
                let mut string_value = self.stack[offset as usize].clone();
                unsafe { string_value.string.inc_ref() };
                self.stack.push(string_value);
            }
            Instruction::Jump(offset) => {
                *ip += offset as usize;
            }
            Instruction::JumpIfFalse(offset) => {
                let value = self.stack.pop().unwrap();
                unsafe { 
                    if !value.boolean {
                        *ip += offset as usize;
                    }
                }
            }
        }
        *ip += 1;

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
            disassamble_instruction(debug_stream, chunk, *ip - 1, &stack_string).unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

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
        let tokens = crate::lexer::lex(code).unwrap();
        let mut program = crate::parser::parse(&tokens).unwrap();
        crate::typecheck::typecheck(&mut program).unwrap();
        let chunk = crate::codegen::compile(&program);
        chunk
    }

    #[test]
    fn test_push_constant() {
        let mut chunk = Chunk::new();
        let num_index = chunk.write_constant(Value { int: 1 });
        chunk.write(Instruction::Constant(num_index), 0);

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
        chunk.write(Instruction::Constant(num_1_index), 110);
        chunk.write(Instruction::Constant(num_2_index), 110);
        chunk.write(Instruction::Add, 111);
        chunk.write(Instruction::Constant(num_3_index), 112);
        chunk.write(Instruction::Div, 113);
        chunk.write(Instruction::Constant(num_4_index), 114);
        chunk.write(Instruction::Sub, 115);

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
}
