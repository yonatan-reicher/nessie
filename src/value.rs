pub mod manual_rc;

pub use self::manual_rc::ManualRc;

use crate::r#type::*;


#[derive(Clone, Copy)]
pub union Value {
    pub int: i32,
    pub boolean: bool,
    pub string: ManualRc<str>,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            // print the value's binary representation
            write!(f, "0b{:b}", self.int)
        }
    }
}

impl Value {
    pub fn new_int(value: i32) -> Self {
        Value { int: value }
    }

    pub fn new_boolean(value: bool) -> Self {
        Value { boolean: value }
    }

    /// Initializes a new string value.
    /// # Safety
    /// This string is copied to the heap, and must be manually memory managed.
    pub unsafe fn new_string(value: &str) -> Self {
        Value { string: ManualRc::new(value) }
    }

    /// Are two values equal?
    /// # Safety
    /// Behavior may be undefined if the values are not of the same type.
    pub unsafe fn is_equal(&self, other: &Self) -> bool {
        self.int == other.int
    }

    /// Free all resources owned by this value.
    /// # Safety
    /// The type provided must fit the value.
    pub unsafe fn free(&mut self, ty: Type) {
        match ty.kind {
            TypeKind::Int => {},
            TypeKind::Bool => {},
            TypeKind::String => {
                // uncount the string
                self.string.dec_ref();
            },
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_size() {
        use std::mem::size_of;
        assert_eq!(size_of::<Value>(), size_of::<usize>());
    }

    #[test]
    fn value_new_int() {
        let value = Value::new_int(42);
        unsafe {
            assert_eq!(value.int, 42);
        }
    }

    #[test]
    fn value_new_boolean() {
        let value = Value::new_boolean(true);
        unsafe {
            assert_eq!(value.boolean, true);
        }
    }

    #[test]
    fn value_new_string() {
        unsafe {
            let mut value = Value::new_string("hello world");
            assert_eq!(value.string.get(), "hello world");
            value.string.dec_ref();
        }
    }
}

