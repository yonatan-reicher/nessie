use std::io::{Write, Result};
use crate::token::Span;
use std::error::Error;
use std::fmt::Debug;
use thiserror::Error;

impl<E: Error> Spanned<E> {
    pub fn show_spanned_error<W: Write>(&self, source: &str, mut f: W) -> Result<()> {
         let span @ Span { start, end, line } = self.span;
         let length = span.len();
         let line_start = source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
         let line_end = source[end..].find('\n').map(|i| i + end).unwrap_or(source.len());
         let line_str = &source[line_start..line_end];
         let column = start - line_start;
 
         let prefix = format!("line {}:", line + 1);
         writeln!(f, "{} {}", prefix, line_str).unwrap();
         write!(f, "{}", " ".repeat(column + prefix.len() + 1)).unwrap();
         writeln!(f, "{}", "^".repeat(length)).unwrap();
         writeln!(f, "{}", self.value).unwrap();
         Ok(())
     }
}

// pub trait SourceError: Error {
//     fn get_span(&self) -> Span;
// 
//     fn with_source<'a, 'b>(&'a self, source: &'b str) -> SourceErrorWithSource<'a, 'b, Self>
//     where
//         Self: Sized,
//     {
//         SourceErrorWithSource {
//             source_error: self,
//             source,
//         }
//     }
// }
// 
// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct SourceErrorWithSource<'a, 'b, T>
// where
//     T: SourceError,
// {
//     source_error: &'a T,
//     source: &'b str,
// }
// 
// impl<'a, 'b, T> Display for SourceErrorWithSource<'a, 'b, T>
// where
//     T: SourceError,
// {
//     fn fmt(&self, f: &mut Formatter) -> Result {
//         let span @ Span { start, end, line } = self.source_error.get_span();
//         let length = span.len();
//         let line_start = self.source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
//         let line_end = self.source[end..].find('\n').map(|i| i + end).unwrap_or(self.source.len());
//         let line_str = &self.source[line_start..line_end];
//         let column = start - line_start;
// 
//         let prefix = format!("line {}:", line + 1);
//         writeln!(f, "{} {}", prefix, line_str)?;
//         write!(f, "{}", " ".repeat(column + prefix.len() + 1))?;
//         writeln!(f, "{}", "^".repeat(length))?;
//         writeln!(f, "{}", self.source_error)?;
//         Ok(())
//     }
// }
// 
// impl<'a, 'b, T> Debug for SourceErrorWithSource<'a, 'b, T>
// where
//     T: SourceError,
// {
//     fn fmt(&self, f: &mut Formatter) -> Result {
//         Display::fmt(self, f)
//     }
// }

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Region(pub Position, pub Position);

impl Region {
    /// Creates a region as big as the space of two given regions.
    pub fn merge(&self, other: &Region) -> Region {
        Region(
            Position::min(self.0, other.0),
            Position::max(self.1, other.1),
        )
    }
}

/// A position in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
