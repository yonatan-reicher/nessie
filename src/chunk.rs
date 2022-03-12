//! Contains byte code instructions and chunk types.

use std::fmt::Display;
use crate::value::Value;


/// A sequence of byte code instructions.
pub struct Chunk {
    pub instructions: Vec<OpCode>,
    pub data: Vec<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    Return = 1,
    Constant,
}


impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // fallback to Debug's displaying
        write!(f, "{:?}", self)
    }
}


impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            instructions: Vec::new(),
            data: Vec::new(),
        }
    }

    pub fn write(&mut self, op: OpCode, operands: &[Value]) {
        self.instructions.push(op);
        for operand in operands {
            self.data.push(*operand);
        }
    }
}

