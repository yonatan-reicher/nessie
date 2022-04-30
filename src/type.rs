use crate::chunk::Instruction;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
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

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use TypeKind::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            ClosureSource => write!(f, "closuresource"),
            Function { arg, ret } => write!(f, "({} -> {})", arg, ret),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Type {
    pub const INT: Type = Type {
        kind: TypeKind::Int,
    };
    pub const BOOL: Type = Type {
        kind: TypeKind::Bool,
    };
    pub const STRING: Type = Type {
        kind: TypeKind::String,
    };
    pub const CLOSURE_SOURCE: Type = Type {
        kind: TypeKind::ClosureSource,
    };

    pub fn function(arg: Rc<Type>, ret: Rc<Type>) -> Type {
        Type {
            kind: TypeKind::Function { arg, ret },
        }
    }

    pub fn category(&self) -> Category {
        use TypeKind::*;
        match &self.kind {
            Int | Bool => Category::Primitive,
            String | ClosureSource | Function { .. } => Category::Pointer,
        }
    }

    pub fn drop_above(&self) -> &[Instruction] {
        use TypeKind::*;
        match &self.kind {
            Int | Bool => &[Instruction::PrimitiveDropAbove],
            String => &[Instruction::StringDropAbove],
            ClosureSource => &[Instruction::ClosureSourceDropAbove],
            Function { .. } => &[Instruction::FunctionDropAbove],
        }
    }
}

pub mod prelude {
    pub use super::{Type, TypeKind, Category};
}
