//! Contains byte code instructions and chunk types.

use std::fmt::Display;
use crate::value::Value;


type LineOffset = usize;

type ConstantIndex = u16;


/// A C-like enum that represents the different types of instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Instruction {
    Return,
    Constant(ConstantIndex),
}


impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // fallback to Debug's displaying
        write!(f, "{:?}", self)
    }
}


/// A sequence of byte code instructions with their parameters and line numbers.
pub struct Chunk {
    instructions: Vec<Instruction>,
    instruction_lines: Vec<LineOffset>,
    constants: Vec<Value>,
}


impl Chunk {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            instruction_lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn write(&mut self, op: Instruction, line: LineOffset) {
        self.instructions.push(op);
        self.instruction_lines.push(line);
    }

    pub fn write_constant(&mut self, value: Value) -> ConstantIndex {
        let index = self.constants.len() as ConstantIndex;
        self.constants.push(value);
        index
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn instruction_lines(&self) -> &[LineOffset] {
        &self.instruction_lines
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn instruction_size() {
        assert_eq!(std::mem::size_of::<Instruction>(), 4);
    }
}
