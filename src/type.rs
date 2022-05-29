use crate::chunk::Instruction;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Int,
    Bool,
    String,
    ClosureSource,
    Function { arg: Rc<Type>, ret: Rc<Type> },
}

/// A type can be either a primitive type or a pointer type
/// (stored on the heap).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Category {
    Primitive,
    Pointer,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            ClosureSource => write!(f, "closuresource"),
            Function { arg, ret } => write!(f, "({} -> {})", arg, ret),
        }
    }
}

impl Type {
    pub fn category(&self) -> Category {
        use Type::*;
        match self {
            Int | Bool => Category::Primitive,
            String | ClosureSource | Function { .. } => Category::Pointer,
        }
    }

    pub fn drop_above(&self) -> &[Instruction] {
        use Type::*;
        match self {
            Int | Bool => &[Instruction::PrimitiveDropAbove],
            String => &[Instruction::StringDropAbove],
            ClosureSource => &[Instruction::ClosureSourceDropAbove],
            Function { .. } => &[Instruction::FunctionDropAbove],
        }
    }
}

pub mod prelude {
    pub use super::{Type, Category};
}
