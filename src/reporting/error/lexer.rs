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

impl From<Error> for Report {
}
