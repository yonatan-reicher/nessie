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

// api
mod engine;

// Bring in the data types:
pub use token::prelude::*;
pub use value::prelude::*;
pub use r#type::prelude::*;
pub use ast::prelude::*;
pub use chunk::prelude::*;
pub use engine::prelude::*;

