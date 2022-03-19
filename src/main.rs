mod lexer;
mod parser;
mod ast;
mod codegen;
mod value;
mod chunk;
mod vm;
mod disassemble;
mod cli;

use std::path::Path;
use std::io::{self, Write, stdin, stdout};
use std::fs::read_to_string;
use std::error;
use vm::VM;
use lexer::{lex, LexError};
use parser::{parse, ParseError};
use codegen::compile;
use value::Value;


#[derive(Debug)]
enum Error {
    Io(io::Error),
    Lex(LexError),
    Parse(Vec<ParseError>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Error::Io(ref err) => {
                write!(f, "IO error: {}", err)
            }
            Error::Lex(ref err) => {
                write!(f, "Lex error: {}", err)
            }
            Error::Parse(ref err) => {
                write!(f, "Parse errors:")?;
                for e in err {
                    write!(f, "\n  {}", e)?;
                }
                Ok(())
            }
        }
    }
}

impl error::Error for Error {}


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

fn interpret(input: &str, vm: &mut VM) -> Result<Option<Value>, Error> {
    let tokens = lex(input).map_err(Error::Lex)?;
    let ast = parse(&tokens).map_err(Error::Parse)?;
    let chunk = compile(&ast);

    vm.stack.clear();
    vm.run(&chunk);
    Ok(vm.stack.pop())
}

fn run_file(file_path: &Path) -> io::Result<()> {
    println!("Running {}", file_path.display());

    let mut vm = VM::new();
    vm.set_debug_stream(Box::new(stdout()));

    let source = read_to_string(file_path)?;
    let result = interpret(&source, &mut vm);
    match result {
        Ok(Some(value)) => {
            println!("{}", value);
        }
        Ok(None) => {
            println!("No value returned");
        }
        Err(err) => {
            eprintln!("{}", err);
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

        match interpret(input, &mut vm) {
            Ok(Some(value)) => {
                println!("{}", value);
            }
            Ok(None) => { }
            Err(err) => {
                eprintln!("{}", err);
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
            eprintln!("{}", err);
        }
    }

    Ok(())
}

