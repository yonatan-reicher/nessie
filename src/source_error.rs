use crate::token::Span;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result};

pub trait SourceError: Error {
    fn get_span(&self) -> Span;

    fn with_source<'a, 'b>(&'a self, source: &'b str) -> SourceErrorWithSource<'a, 'b, Self>
    where
        Self: Sized,
    {
        SourceErrorWithSource {
            source_error: self,
            source,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceErrorWithSource<'a, 'b, T>
where
    T: SourceError,
{
    source_error: &'a T,
    source: &'b str,
}

impl<'a, 'b, T> Display for SourceErrorWithSource<'a, 'b, T>
where
    T: SourceError,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        let span @ Span { start, end, line } = self.source_error.get_span();
        let length = span.len();
        let line_start = self.source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let line_end = self.source[end..].find('\n').map(|i| i + end).unwrap_or(self.source.len());
        let line_str = &self.source[line_start..line_end];
        let column = start - line_start;

        let prefix = format!("line {}:", line + 1);
        writeln!(f, "{} {}", prefix, line_str)?;
        write!(f, "{}", " ".repeat(column + prefix.len() + 1))?;
        writeln!(f, "{}", "^".repeat(length))?;
        writeln!(f, "{}", self.source_error)?;
        Ok(())
    }
}

impl<'a, 'b, T> Debug for SourceErrorWithSource<'a, 'b, T>
where
    T: SourceError,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        Display::fmt(self, f)
    }
}
