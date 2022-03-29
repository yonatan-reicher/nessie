#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Int,
    Bool,
    String,
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TypeKind::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
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
    pub const STRING: Type = Type {
        kind: TypeKind::String,
    };
}

