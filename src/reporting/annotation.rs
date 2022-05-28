use std::fmt::{Debug, Write, Result};

/// A value `T` which is located at a specific region in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Located<T> {
    pub region: Region,
    pub value: T,
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Located<T> {
    /// Applies a function to the contained value.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        Located {
            region: self.region,
            value: f(self.value),
        }
    }

    pub fn as_ref(&self) -> Located<&T> {
        Located { region: self.region, value: &self.value }
    }

    pub fn as_mut(&mut self) -> Located<&mut T> {
        Located { region: self.region, value: &mut self.value }
    }
}

/// A part of the source code. The first position is the start (inclusive),
/// and the second is the end (exclusive).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Region(pub Position, pub Position);

impl Region {
    pub fn point(position: Position) -> Self {
        Region(position, position)
    }

    /// Creates a region as big as the space of two given regions.
    pub fn merge(&self, other: &Region) -> Region {
        Region(
            Position::min(self.0, other.0),
            Position::max(self.1, other.1),
        )
    }

    pub fn show_region<W: Write>(&self, source: &str, mut f: W) -> Result {
        assert!(self.1.line >= self.0.line);

        // Get the lines where the error occoured.
        let n_lines = self.1.line - self.0.line + 1;
        let lines: Vec<&str> = source
            .lines()
            .skip(self.0.line as _)
            .take(n_lines as _)
            .collect();

        // Printing is handled entirely differently for single line errors
        // and multiline errors.
        match &lines[..] {
            [] => todo!(),
            [line] => {
                let prefix = format!("{} |    ", self.0.line + 1);
                let n_columns = self.1.column - self.0.column + 1;
                let start = self.0.column as usize + prefix.len();
                writeln!(f, "{}{}", prefix, line)?;
                write!(f, "{}", " ".repeat(start))?;
                writeln!(f, "{}", "^".repeat(n_columns as _))?;
            }
            lines => {
                for (i, line) in lines.iter().enumerate() {
                    writeln!(f, "{} >|    {}", self.0.line as usize + 1 + i, line)?;
                }
            }
        }

        Ok(())
    }
}

/// A line number in the source code. First line is represented by 0.
pub type Line = u16;

/// A position in the source code.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: Line,
    pub column: u16,
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.line
            .cmp(&other.line)
            .then(self.column.cmp(&other.column))
    }
}


#[cfg(test)]
mod position_tests {
    use super::*;

    #[test]
    fn compare_equal_positions() {
        use std::cmp::Ordering::Equal;
        let a = Position { line: 4, column: 4 };
        assert_eq!(a.cmp(&a), Equal);
    }

    #[test]
    fn compare_bigger_by_line() {
        let a = Position { line: 5, column: 7 };
        let b = Position { line: 3, column: 10 };
        assert!(a > b);
    }

    #[test]
    fn compare_bigger_by_column() {
        let a = Position { line: 5, column: 4 };
        let b = Position { line: 5, column: 2 };
        assert!(a > b);
    }
}

#[cfg(test)]
mod region_tests {
    use super::*;
    use indoc::indoc;

    macro_rules! make_show_region_test {
        ($($name: ident, $expr:expr, $code:expr, $expected:expr),*) => {
            $(
                #[test]
                fn $name() {
                    let code = $code;
                    let region = $expr;
                    let mut buf = String::new();
                    region.show_region(code, &mut buf).expect("writing error");
                    assert_eq!(
                        &buf,
                        $expected,
                    );
                }
            )*
        }
    }

    make_show_region_test! {
        show_region_single_line,
        Region(
            Position { line: 1, column: 2 },
            Position { line: 1, column: 5 },
        ),
        indoc! {"
            hello
            world!
        "},
        indoc! {"
            2 |    world!
                     ^^^^
        "},

        show_region_multiple_lines,
        Region(
            Position { line: 0, column: 2 },
            Position { line: 1, column: 3 },
        ),
        indoc! {"
            hello
            world!
        "},
        indoc! {"
            1 >|    hello
            2 >|    world!
        "},

        show_region_single_char,
        Region::point(Position { line: 0, column: 1 }),
        indoc! {"
            hey man
        "},
        indoc! {"
            1 |    hey man
                    ^
        "}
    }
}

