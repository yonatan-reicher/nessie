use std::io::{Write, Result};
use std::fmt::Debug;

/// A value `T` which is located at a specific region in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Located<T> {
    pub region: Region,
    pub value: T,
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
        self.map(|x| &x)
    }

    pub fn as_mut(&mut self) -> Located<&mut T> {
        self.map(|x| &mut x)
    }
}


/// A part of the source code.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Region(pub Position, pub Position);

impl Region {
    /// Creates a region as big as the space of two given regions.
    pub fn merge(&self, other: &Region) -> Region {
        Region(
            Position::min(self.0, other.0),
            Position::max(self.1, other.1),
        )
    }

    pub fn show_region<W: Write>(&self, source: &str, mut f: W) -> Result<()> {
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
            [] => unreachable!(), // because of the + 1
            [line] => {
                let prefix = format!("{} |    ", self.0.line + 1);
                let n_columns = self.1.column - self.0.column + 1;
                let start = self.0.column as usize + prefix.len() + 1;
                writeln!(f, "{} {}", prefix, line)?;
                write!(f, "{}", " ".repeat(start))?;
                writeln!(f, "{}", "^".repeat(n_columns as _))?;
            }
            lines => {
                for (i, line) in lines.iter().enumerate() {
                    writeln!(f, "{} |>    {}", self.0.line as usize + 1 + i, line)?;
                }
            }
        }

        Ok(())
    }
}

/// A position in the source code.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: u16,
    pub column: u16,
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.line.cmp(&other.line).then(self.column.cmp(&other.column))
    }
}

