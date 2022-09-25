use crate::reporting::annotation::Located;
use crate::reporting::report::Report;
use thiserror::Error;

#[derive(Error, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
    #[error("invalid character: {0}")]
    InvalidCharacter(char),
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("invalid escape sequence")]
    InvalidEscapeSequence,
    #[error("invalid unicode escape sequence")]
    InvalidUnicodeEscapeSequence,
}

impl From<&Located<Error>> for Report {
    fn from(error: &Located<Error>) -> Report {
        match **error {
            Error::InvalidCharacter(c) => Report {
                message: "I found an invalid character while reading the code:".into(),
                region: error.region,
                notes: vec![format!("Found an invalid character {c}")],
                suggestion: vec![],
            },
            Error::UnterminatedString => Report {
                message: "I found a string that was never ended:".into(),
                region: error.region,
                notes: vec![format!("Found an `'` or an `\"` that did not have pair to end it")],
                suggestion: vec![
                    format!("Make sure that a string should start there"),
                    format!("Add a matching `'` or `\"` to end the string"),
                ],
            },
            Error::InvalidEscapeSequence => todo!(),
            Error::InvalidUnicodeEscapeSequence => todo!(),
        }
    }
}
