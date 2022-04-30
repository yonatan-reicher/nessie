//! Contains byte code instructions and chunk types.

use crate::token::Line;
use crate::value::Value;
use crate::vm::VM;
use crate::r#type::Type;
use std::fmt::{self, Display, Formatter};

type ConstantIndex = u16;

/// A C-like enum that represents the different types of instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Instruction {
    /// Pushes a primitive constant value onto the stack from the constant pool.
    PrimitiveConstant(ConstantIndex),
    /// Pushes a pointer constant value onto the stack from the constant pool.
    PtrConstant(ConstantIndex),
    /// Drops a primitive value placed one before the last value on the stack
    PrimitiveDropAbove,
    /// Pushes a primitive value onto the stack.
    PrimitiveGetLocal(u16),
    /// Pushes a pointer value onto the stack.
    PtrGetLocal(u16),
    // Integer arithmetic instructions.
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Le,
    Ge,
    IntEq,
    IntNe,
    // Boolean arithmetic instructions
    /// Pushes a true value onto the stack.
    True,
    /// Pushes a false value onto the stack.
    False,
    Not,
    And,
    Or,
    Xor,
    BoolEq,
    BoolNe,
    // String instructions
    Concat,
    StringEq,
    StringNe,
    /// Drops a string value placed one before the uppermost value on the stack
    StringDropAbove,
    /// Jumps the specified number of instructions forward unconditionally.
    Jump(u16),
    /// Jumps the specified number of instructions forward, if the value on the
    /// top of the stack it consumes is a false.
    JumpIfFalse(u16),
    // Function and closure instructions.
    /// Pops a value and function off the stack and calls it with the value.
    Call,
    /// Takes a closure with an empty capture list and a variable number of
    /// arguments off the stack and puts them into the closure's capture list.
    /// 
    /// The closure is the value at the top of the stack.
    /// The value farthest from the top of the stack is the first on the
    /// capture list.
    Closure(u16),
    ClosureSourceDropAbove,
    /// Takes a closure placed one before the uppermost value on the stack and
    /// frees and decreases all reference counts of its captured values and of
    /// itself.
    FunctionDropAbove,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // fallback to Debug's displaying
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Default)]
pub struct Chunk {
    /// Optional name for the chunk - for debuging nessie code.
    name: Option<String>,
    /// The set of constant values contained in the chunk.
    constants: Vec<Value>,
    /// The code for dropping the constants - called with an arbitrary value
    /// at the top of the stack and all the constants below it.
    /// For now, this is not `Instructions` because we don't need debuging
    /// info.
    constants_drop: Vec<Instruction>,
    /// The instructions of the chunk.
    instructions: Vec<Instruction>,
    /// Line information for the instructions.
    instruction_lines: Vec<Line>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn name_mut(&mut self) -> &mut Option<String> {
        &mut self.name
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn instructions_mut(&mut self) -> &mut [Instruction] {
        &mut self.instructions
    }

    pub fn instruction_lines(&self) -> &[Line] {
        &self.instruction_lines
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn constants_drop(&self) -> &[Instruction] {
        &self.constants_drop
    }

    // 1 + 1 = 

    /// Adds a new instruction to the chunk with the given line information.
    pub fn write(&mut self, op: Instruction, line: Line) {
        self.instructions.push(op);
        self.instruction_lines.push(line);
    }

    /// Adds a constant value to the chunk.
    ///
    /// # Safety
    /// This function is unsafe because the value must be dropped correctly.
    /// 
    /// Ensure that `ty` is the correct type for the value.
    pub unsafe fn write_constant(&mut self, value: Value, ty: &Type) -> ConstantIndex {
        let index = self.constants.len() as ConstantIndex;
        self.constants.push(value);
        self.constants_drop.extend_from_slice(ty.drop_above());
        index
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        VM::drop_chunk(self);
    }
}

pub mod prelude {
    pub use super::*;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instruction_size() {
        assert_eq!(std::mem::size_of::<Instruction>(), 4);
    }
}
