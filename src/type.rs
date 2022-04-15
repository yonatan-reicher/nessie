use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Int,
    Bool,
    String,
    Function { arg: Rc<Type>, ret: Rc<Type> },
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use TypeKind::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
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

    pub fn function(arg: Rc<Type>, ret: Rc<Type>) -> Type {
        Type {
            kind: TypeKind::Function { arg, ret },
        }
    }
}
