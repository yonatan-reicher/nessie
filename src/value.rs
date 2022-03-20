#[derive(Clone, Copy)]
pub union Value {
    pub int: i32,
    pub bool: bool,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            // print the value's binary representation
            write!(f, "0b{:b}", self.int)
        }
    }
}
