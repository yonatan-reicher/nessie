use crate::r#type::Type;
use std::rc::{Rc, Weak};

/// A type that can be constructed by it's constructors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constructed {
    pub name: String,
    pub constructors: Vec<Constructor>,
}

/// A constructor for a constructed type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constructor {
    pub name: String,
    pub arguments: Vec<Type>,
}

impl Constructor {
    pub fn r#type(&self, constructed: Weak<Constructed>) -> Type {
        Type::curried_function(
            &self.arguments,
            Type::Constructed(ConstructedPtr::Weak(constructed)),
        )
    }
}

/// Either a strong shared reference or a weak shared reference to a Constructed.
#[derive(Debug, Clone)]
pub enum ConstructedPtr {
    Rc(Rc<Constructed>),
    Weak(Weak<Constructed>),
}

impl PartialEq for ConstructedPtr {
    fn eq(&self, other: &Self) -> bool {
        match (self.upgrade(), other.upgrade()) {
            (Some(a), Some(b)) => a == b,
            (_, _) => false,
        }
    }
}

impl Eq for ConstructedPtr {}

impl ConstructedPtr {
    pub fn upgrade(&self) -> Option<Rc<Constructed>> {
        match self {
            Self::Rc(rc) => Some(rc.clone()),
            Self::Weak(weak) => weak.upgrade(),
        }
    }
}
