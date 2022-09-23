mod constructed;

pub use constructed::{Constructed, ConstructedPtr};

use crate::chunk::Instruction;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    String,
    ClosureSource,
    Function { arg: Rc<Type>, ret: Rc<Type> },
    Constructed(ConstructedPtr),
}

/// A type can be either a primitive type or a pointer type
/// (stored on the heap).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Category {
    Primitive,
    Pointer,
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::Equal;

        if self == other {
            Some(Equal)
        } else {
            None
        }
    }
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
            Constructed(c) => write!(f, "{}", &c.upgrade().ok_or(fmt::Error)?.name),
        }
    }
}

impl Type {
    pub fn category(&self) -> Category {
        use Type::*;
        match self {
            Int | Bool => Category::Primitive,
            String | ClosureSource | Function { .. } | Constructed(_) => Category::Pointer,
        }
    }

    pub fn drop_above(&self) -> &[Instruction] {
        use Type::*;
        match self {
            Int | Bool => &[Instruction::PrimitiveDropAbove],
            String => &[Instruction::StringDropAbove],
            ClosureSource => &[Instruction::ClosureSourceDropAbove],
            Function { .. } => &[Instruction::FunctionDropAbove],
            Constructed(_) => todo!(),
        }
    }

    pub fn curried_function(arguments: &[Type], ret: Type) -> Type {
        match arguments {
            [] => ret,
            [arg0, rest @ ..] => Type::Function {
                arg: arg0.clone().into(),
                ret: Self::curried_function(rest, ret).into(),
            },
        }
    }
}

pub mod prelude {
    pub use super::{Category, Type};
}
