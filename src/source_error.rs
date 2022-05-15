use std::io::{Write, Result};
use crate::token::Span;
use std::error::Error;
use std::fmt::Debug;
use thiserror::Error;

#[derive(Error, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[error("at {span:?}: {value}")]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
}

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
