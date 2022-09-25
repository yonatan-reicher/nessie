use crate::r#type::Type;
use crate::reporting::Report;
use crate::reporting::annotation::Located;
use std::rc::Rc;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Error {
    #[error("type {0} is not a function")]
    NotAFunction(Type),
    #[error("the type of an expression cannot be inferred")]
    UnknownType,
    #[error("The type of this expression was expected to be {expected} but was {actual}")]
    OperatorTypeMissmatch { expected: Type, actual: Type },
    #[error("Variable {0} is not defined")]
    UndefinedVariable(Rc<str>),
}

impl From<&Located<Error>> for Report {
    fn from(error: &Located<Error>) -> Self {
        match **error {
            Error::UnknownType => Report {
                message: "I need to know the type of this expression to compile it.".into(),
                region: error.region,
                notes: vec![],
                suggestion: vec![],
            },
            Error::NotAFunction(ref ty) => Report {
                message: format!("The type `{}` is not a function.", ty),
                region: error.region,
                notes: vec!["Only functions can be called.".into()],
                suggestion: vec![],
            },
            Error::OperatorTypeMissmatch { ref expected, ref actual } => Report {
                message: format!(
                    "The type of this expression was expected to be `{}` but was `{}`.",
                    expected, actual
                ),
                region: error.region,
                notes: vec![],
                suggestion: vec![],
            },
            Error::UndefinedVariable(ref name) => Report {
                message: format!("Variable `{}` is not defined.", name),
                region: error.region,
                notes: vec![],
                suggestion: vec![],
            },
        }
    }
}
