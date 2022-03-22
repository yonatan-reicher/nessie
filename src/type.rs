use std::rc::Rc;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Int,
    Bool,
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TypeKind::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
}

