//! The `Engine` is the main way to embed and interact with nessie programs.
//!
//! # Examples
//! ```
//! use nessie::Engine;

// datatypes
use crate::ast::prelude::*;
use crate::chunk::prelude::*;
use crate::r#type::prelude::*;
use crate::value::prelude::*;

// pipeline stages
use crate::codegen;
use crate::lexer;
use crate::parser;
use crate::source_error;
use crate::typecheck;
use crate::vm;

// other
use source_error::SourceError;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Write};
use std::rc::Rc;
use std::result;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    /// The original source code.
    /// TODO: Save only the relevant part of the source code.
    source: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    Lex(lexer::Error),
    Parse(Vec<parser::Error>),
    Typecheck(Vec<typecheck::Error>),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use ErrorKind::*;
        match &self.kind {
            Lex(err) => {
                writeln!(f, "Lexing error:")?;
                writeln!(f, "{}", err.with_source(&self.source))?;
            }
            Parse(err) => {
                writeln!(f, "Parsing errors:")?;
                for e in err {
                    writeln!(f, "{}", e.with_source(&self.source))?;
                }
            }
            Typecheck(errors) => {
                writeln!(f, "Type errors:")?;
                for e in errors {
                    writeln!(f, "{}", e.with_source(&self.source))?;
                }
            }
        }
        Ok(())
    }
}

impl std::error::Error for Error {}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Default)]
pub struct Engine {
    /// The typechecking context.
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
