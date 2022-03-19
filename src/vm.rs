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
            ($x: tt) => ( {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a $x b);
            } )
        }

        // execute the instruction
        match instruction {
            Instruction::Constant(constant) => {
                let value = chunk.constants()[constant as usize];
                self.stack.push(value);
            }
            Instruction::Return => {
                let value = self.stack.pop().unwrap();
                println!("{}", value);
            }
            Instruction::Add => binary_operation!(+),
            Instruction::Sub => binary_operation!(-),
            Instruction::Mul => binary_operation!(*),
            Instruction::Div => binary_operation!(/),
            Instruction::Mod => binary_operation!(%),
            Instruction::Neg => {
                let value = self.stack.pop().unwrap();
                self.stack.push(-value);
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
                    s.push_str(&value.to_string());
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

