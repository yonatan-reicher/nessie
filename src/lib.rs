//! Nessie - A typed, functional scripting language
//!
//! Nessie is an interpreted language that is typed and functional, which you
//! can embed in your applications or use independently.

// Internal modules:

// data types
mod token;
mod value;
mod r#type;
mod ast;
mod chunk;

// pipeline stages
mod lexer;
mod parser;
mod typecheck;
mod codegen;
mod vm;
mod disassemble;
mod source_error;

use source_error::SourceError;
use std::result;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use std::io::{self, Write};


// Bring in the data types:
pub use token::prelude::*;
pub use value::prelude::*;
pub use r#type::prelude::*;
pub use ast::prelude::*;
pub use chunk::prelude::*;


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

#[derive(Default)]
pub struct Engine {
    /// The typechecking context.
    env: typecheck::Env,
    /// The code generation context.
    compiler: codegen::Compiler,
    /// The code execution context.
    vm: vm::VM,
    debug_stream: Option<Box<dyn Write>>,
}

/// A program that is well typed
pub struct TypedProgram {
    ast: Program,
    chunk: Chunk,
}

fn as_error<T, E>(source: &str, kind_function: fn(E) -> ErrorKind, r: result::Result<T, E>) -> Result<T> {
    r.map_err(|e| Error {
        kind: kind_function(e),
        source: source.into(),
    })
}

impl Engine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn debug_stream(&mut self) -> &mut Option<Box<dyn Write>> {
        &mut self.debug_stream
    }

    pub fn declare(&mut self, name: Rc<str>, ty_: Type, value: Value) {
        let unique_name = self.env.declare(name, ty_.clone());
        self.compiler.declare(unique_name, ty_.clone());
        self.vm.stack.push(value);
    }

    pub fn typecheck(&mut self, source_code: &str) -> Result<TypedProgram> {
        let tokens = as_error(source_code, ErrorKind::Lex, lexer::lex(source_code))?;
        let mut ast = as_error(source_code, ErrorKind::Parse, parser::parse(&tokens))?;
        as_error(source_code, ErrorKind::Typecheck, self.env.typecheck(&mut ast))?;
        let chunk = self.compiler.compile(&ast);
        Ok(TypedProgram {
            ast,
            chunk,
        })
    }

    pub fn disassamble<W: Write>(&self, program: &TypedProgram, write: W) -> io::Result<()> {
        disassemble::disassamble(write, &program.chunk, "chunk")?;
        Ok(())
    }

    pub fn eval(&mut self, program: &TypedProgram) -> Value {
        self.vm.run(&program.chunk);
        self.vm.stack.pop().unwrap()
    }
}

