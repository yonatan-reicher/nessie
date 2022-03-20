use crate::chunk::{Chunk, Instruction};
use crate::value::Value;
use crate::disassemble::disassamble_instruction;


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
            Instruction::And => binary_operation!(&, bool, bool),
            Instruction::Or => binary_operation!(|, bool, bool),
            Instruction::Xor => binary_operation!(^, bool, bool),
            Instruction::Not => unary_operation!(!, bool, bool),
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

    #[test]
    fn test_push_constant() {
        let mut chunk = Chunk::new();
        let num_index = chunk.write_constant(1);
        chunk.write(Instruction::Constant(num_index), 0);

        let mut vm = VM::new();
        vm.run(&chunk);
     
        assert_eq!(vm.stack.pop(), Some(1));
        assert_eq!(vm.stack.pop(), None);
    }

    #[test]
    fn simple_arithmetic() {
        let mut chunk = Chunk::new();
        let num_1_index = chunk.write_constant(14);
        let num_2_index = chunk.write_constant(5);
        let num_3_index = chunk.write_constant(3);
        let num_4_index = chunk.write_constant(9);
        chunk.write(Instruction::Constant(num_1_index), 110);
        chunk.write(Instruction::Constant(num_2_index), 110);
        chunk.write(Instruction::Add, 111);
        chunk.write(Instruction::Constant(num_3_index), 112);
        chunk.write(Instruction::Div, 113);
        chunk.write(Instruction::Constant(num_4_index), 114);
        chunk.write(Instruction::Sub, 115);

        let mut vm = VM::new();
        vm.run(&chunk);

        assert_eq!(vm.stack.pop(), Some(-3));
        assert_eq!(vm.stack.pop(), None);
    }
}

