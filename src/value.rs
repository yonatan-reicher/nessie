#[derive(Clone, Copy)]
pub union Value {
    pub int: i32,
    pub boolean: bool,
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

    pub unsafe fn is_equal(&self, other: &Self) -> bool {
        self.int == other.int
    }
}
