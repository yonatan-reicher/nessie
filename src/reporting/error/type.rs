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
    #[error("The program's type could not be inferred")]
    ProgramTypeUnknown,
    #[error("Variable {0} is not defined")]
    UndefinedVariable(Rc<str>),
}

impl From<&Located<Error>> for Report {
    fn from(error: &Located<Error>) -> Self {
        todo!()
    }
}
