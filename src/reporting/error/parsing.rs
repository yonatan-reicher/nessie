use crate::reporting::annotation::Located;
use crate::reporting::Report;
use crate::token::Token;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Error {
    #[error("expected an expression")]
    ExpectedExpression,
    #[error("expected an expression atom")]
    ExpectedExpressionAtom,
    #[error("left over tokens")]
    LeftoverSource,
    #[error("unclosed delimiter")]
    UnclosedDelimiter,
    #[error("expected an identifier")]
    ExpectedIdentifier,
    #[error("expected a {0}")]
    ExpectedToken(Token),
    #[error("empty program")]
    EmptyCode,
    #[error("expected a type expression")]
    ExpectedTypeExpr,
    #[error("unary operator is missing an operand")]
    UnaryOperatorMissingOperand,
}

impl From<&Located<Error>> for Report {
    fn from(error: &Located<Error>) -> Report {
        match **error {
            Error::ExpectedExpression => Report {
                message: "I expected to find an expression, but I did not.".into(),
                region: error.region,
                notes: vec![],
                suggestion: vec![],
            },
            Error::ExpectedExpressionAtom => Report {
                message: "I expected to find an expression atom, but I did not.".into(),
                region: error.region,
                notes: vec![
                    format!("An expression atom is a small expression, usually a single word, number, or something inside parentheses.")
                ],
                suggestion: vec![],
            },
            Error::LeftoverSource => Report {
                message: "I thought the code was done, but there was still more text to read.".into(),
                region: error.region,
                notes: vec![],
                suggestion: vec![
                    format!("Try removing the code that was left over."),
                ],
            },
            Error::UnclosedDelimiter => Report {
                message: "I found an open `(`, `[` or `{` that had no matching pair.".into(),
                region: error.region,
                notes: vec![],
                suggestion: vec![],
            },
            Error::ExpectedIdentifier => Report {
                message: "I expected to find an identifier, but I did not.".into(),
                region: error.region,
                notes: vec![],
                suggestion: vec![],
            },
            Error::ExpectedToken(ref token) => Report {
                message: format!("I expected to find a `{}`, but I did not.", token),
                region: error.region,
                notes: vec![],
                suggestion: vec![],
            },
            Error::EmptyCode => todo!(),
            Error::ExpectedTypeExpr => todo!(),
            Error::UnaryOperatorMissingOperand => todo!(),
        }
    }
}
