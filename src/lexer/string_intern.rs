use std::collections::HashSet;
use std::rc::Rc;


pub struct StringInterner(HashSet<Rc<str>>);

impl StringInterner {
    pub fn new() -> StringInterner {
        StringInterner(HashSet::new())
    }

    pub fn intern(&mut self, s: &str) -> Rc<str> {
        if let Some(rc) = self.0.get(s) {
            return rc.clone();
        }
        let rc: Rc<str> = Rc::from(s);
        self.0.insert(rc.clone());
        rc
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_same() {
        let mut interner = StringInterner::new();
        let s1 = interner.intern("foo");
        let s2 = interner.intern("foo");
        assert_eq!(s1, s2);
    }

    #[test]
    fn intern_different() {
        let mut interner = StringInterner::new();
        let s1 = interner.intern("foo");
        let s2 = interner.intern("bar");
        assert_ne!(s1, s2);
    }
}

