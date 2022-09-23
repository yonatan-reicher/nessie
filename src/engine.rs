//! The `Engine` is the main way to embed and interact with Nessie programs.
//!
//! # Examples
//! ```
//! use nessie::Engine;

// data types
use crate::ast::prelude::*;
use crate::chunk::prelude::*;
use crate::r#type::prelude::*;
use crate::reporting::Report;
use crate::value::prelude::*;

// pipeline stages
use crate::codegen;
use crate::lexer;
use crate::parser;
use crate::typecheck;
use crate::vm;

// Errors and reporting.
use crate::reporting::annotation::Located;
use crate::reporting::error::{LexError, ParseError, TypeError};

// other
use std::io::{self, Write};
use std::rc::Rc;
use std::fmt::{self, Display, Formatter};
use std::result;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub struct Error {
    #[source]
    pub kind: ErrorKind,
    /// The original source code.
    /// TODO: Save only the relevant part of the source code.
    source: String,
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    #[error("lexing error")]
    Lex(#[source] Located<LexError>),
    #[error("parsing errors")]
    Parse(Vec<Located<ParseError>>),
    #[error("type checking errors")]
    Typecheck(Vec<Located<TypeError>>),
}

impl ErrorKind {
    pub fn reports<'a>(&'a self) -> Box<dyn Iterator<Item=Report> + 'a> {
        match self {
            Self::Lex(err) => Box::new(std::iter::once(err.into())),
            Self::Parse(errors) => Box::new(errors.iter().map(Report::from)),
            Self::Typecheck(errors) => Box::new(errors.iter().map(Report::from)),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let reports = self.kind.reports();
        writeln!(f, "{}", self.kind)?;
        for report in reports {
            writeln!(f, "{}", report.with_source(&self.source))?;
        }
        Ok(())
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Default)]
pub struct Engine {
    /// The type checking context.
    env: typecheck::Env,
    /// The code generation context.
    compiler: codegen::Compiler,
    /// The code execution context.
    vm: vm::VM,
}

/// A program that is well typed
pub struct TypedProgram {
    ast: Program,
    chunk: Chunk,
}

impl TypedProgram {
    pub fn ast(&self) -> &Program {
        &self.ast
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    /// Returns the typed of the compiled program.
    pub fn ty(&self) -> Type {
        self.ast.body.ty.clone().unwrap()
    }
}

fn as_error<T, E>(
    source: &str,
    kind_function: fn(E) -> ErrorKind,
    r: result::Result<T, E>,
) -> Result<T> {
    r.map_err(|e| Error {
        kind: kind_function(e),
        source: source.into(),
    })
}

impl Engine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn declare(&mut self, name: Rc<str>, ty_: Type, value: Value) {
        let unique_name = self.env.declare(name, ty_.clone());
        self.compiler.declare(unique_name, ty_.clone());
        self.vm.stack.push(value);
    }

    pub fn typecheck(&mut self, source_code: &str) -> Result<TypedProgram> {
        let tokens = as_error(source_code, ErrorKind::Lex, lexer::lex(source_code))?;
        let mut ast = as_error(source_code, ErrorKind::Parse, parser::parse(&tokens))?;
        as_error(
            source_code,
            ErrorKind::Typecheck,
            self.env.typecheck(&mut ast),
        )?;
        let chunk = self.compiler.compile(&ast);
        Ok(TypedProgram { ast, chunk })
    }

    pub fn disassamble<W: Write>(&self, program: &TypedProgram, write: W) -> io::Result<()> {
        use crate::disassemble;

        disassemble::disassemble(write, &program.chunk)?;
        Ok(())
    }

    pub fn eval(&mut self, program: &TypedProgram) -> Value {
        self.vm.eval(&program.chunk)
    }
}

pub mod prelude {
    pub use super::Engine;
}
