mod source_error;

mod token;
mod value;

mod r#type;
mod ast;
mod chunk;

mod lexer;
mod parser;
mod typecheck;
mod codegen;
mod vm;
mod disassemble;

mod cli;

use std::path::Path;
use std::io::{self, Write, stdin, stdout};
use std::fs::read_to_string;
use vm::VM;
use lexer::{lex, LexError};
use parser::{parse, ParseError};
use typecheck::{typecheck, TypeError};
use codegen::compile;
use value::Value;
use r#type::Type;
use source_error::SourceError;


#[derive(Debug)]
enum Error {
    Lex(LexError),
    Parse(Vec<ParseError>),
    Type(Vec<TypeError>),
}

impl Error {
    fn show<W>(&self, source: &str, mut out: W) -> io::Result<()>
        where W: Write
    {
        match self {
            Error::Lex(err) => {
                writeln!(out, "Lexing error:")?;
                writeln!(out, "{}", err.with_source(source))?;
            }
            Error::Parse(err) => {
                writeln!(out, "Parsing errors:")?;
                for e in err {
                    writeln!(out, "{}", e.with_source(source))?;
                }
            }
            Error::Type(errors) => {
                writeln!(out, "Type errors:")?;
                for e in errors {
                    writeln!(out, "{}", e.with_source(source))?;
                }
            }
        }
        Ok(())
    }
}


fn main() -> io::Result<()> {
    let cli = cli::Cli::parse();

    match cli.command {
        cli::Command::Run { file_path } => {
            run_file(&file_path)?;
        }
        cli::Command::Repl => {
            repl()?;
        }
        cli::Command::Disassemble { file_path } => {
            disassemble_file(&file_path)?;
        }
    }

    Ok(())
}

unsafe fn interpret(input: &str, vm: &mut VM)
-> Result<Option<(Value, Type)>, Error> {
    let tokens = lex(input).map_err(Error::Lex)?;
    let mut ast = parse(&tokens).map_err(Error::Parse)?;
    typecheck(&mut ast).map_err(Error::Type)?;
    let chunk = compile(&ast);

    vm.stack.clear();
    vm.run(&chunk);
    Ok(vm.stack.pop().map(|v| (v, ast.body.ty.unwrap())))
}

fn run_file(file_path: &Path) -> io::Result<()> {
    println!("Running {}", file_path.display());

    let mut vm = VM::new();
    vm.set_debug_stream(Box::new(stdout()));

    let source = read_to_string(file_path)?;
    unsafe {
        let result = interpret(&source, &mut vm);
        match result {
            Ok(Some((mut value, ty))) => {
                println!("{:?}", value);
                value.free(ty);
            }
            Ok(None) => {
                println!("No value returned");
            }
            Err(err) => {
                err.show(&source, &mut io::stderr())?;
            }
        }
    }
    Ok(())
}

fn repl() -> io::Result<()> {
    let mut input_buf = String::new();
    let mut vm = VM::new();
    vm.set_debug_stream(Box::new(stdout()));

    loop {
        print!("> ");
        stdout().flush()?;
        input_buf.clear();
        stdin().read_line(&mut input_buf)?;
        let input = input_buf.trim();

        // stop if "exit" was typed
        if input == "exit" {
            break Ok(());
        }

        unsafe {
            match interpret(input, &mut vm) {
                Ok(Some((mut value, ty))) => {
                    println!("{:?}", value);
                    value.free(ty);
                }
                Ok(None) => { }
                Err(err) => {
                    err.show(input, &mut io::stderr())?;
                }
            }
        }
    }
}

fn disassemble_file(file_path: &Path) -> io::Result<()> {
    use disassemble::disassamble;

    let name = file_path.file_name().unwrap().to_str().unwrap();
    let source = read_to_string(file_path)?;
    let chunk = {
        lex(&source).map_err(Error::Lex)
        .and_then(|tokens| parse(&tokens).map_err(Error::Parse))
        .and_then(|ast| Ok(compile(&ast)))
    };

    match chunk {
        Ok(chunk) => {
            disassamble(&mut stdout(), &chunk, name)?;
        }
        Err(err) => {
            err.show(&source, io::stderr())?;
        }
    }

    Ok(())
}

