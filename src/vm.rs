use crate::chunk::{Chunk, Instruction};
use crate::value::Value;
use crate::disassemble::disassamble_instruction;

/// A virtual machine is a stack-based interpreter for a chunk of bytecode.
pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: Vec<Value>,
    debug_stream: Option<&'a mut dyn std::io::Write>,
}

impl<'a> VM<'a> {
    /// Creates a new virtual machine from a chunk of bytecode.
    pub fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::new(),
            debug_stream: None,
        }
    }

    /// Sets the debug stream for the virtual machine.
    pub fn set_debug_stream(&mut self, stream: &'a mut dyn std::io::Write) {
        self.debug_stream = Some(stream);
    }

    /// Clear the debug stream for the virtual machine.
    pub fn clear_debug_stream(&mut self) {
        self.debug_stream = None;
    }

    /// Executes the virtual machine.
    pub fn run(&mut self) {
        while self.ip < self.chunk.instructions().len() {
            let instruction = self.chunk.instructions()[self.ip];
            self.run_single(instruction);
        }
    }

    /// Executes a single instruction.
    pub fn run_single(&mut self, instruction: Instruction) {

        // print debug information
        if let Some(ref mut debug_stream) = self.debug_stream {
            disassamble_instruction(debug_stream, self.chunk, self.ip).unwrap();
            // print the stack
            write!(debug_stream, "[").unwrap();
            for (i, value) in self.stack.iter().enumerate() {
                if i > 0 {
                    write!(debug_stream, ", ").unwrap();
                }
                write!(debug_stream, "{}", value).unwrap();
            }
            writeln!(debug_stream, "]").unwrap();
        }

        // execute the instruction
        match instruction {
            Instruction::Constant(constant) => {
                let value = self.chunk.constants()[constant as usize];
                self.stack.push(value);
            }
            Instruction::Return => {
                let value = self.stack.pop().unwrap();
                println!("{}", value);
            }
        }
        self.ip += 1;
    }
}
