mod cli;

use std::path::Path;
use std::io::{self, Write, stdin, stdout};
use std::fs::read_to_string;
use nessie::Engine;
//use nessie::vm::VM;
//use nessie::lexer::{lex, Error as LexError};
//use nessie::parser::{parse, Error as ParseError};
//use nessie::typecheck::{typecheck, Error as TypeError};
//use nessie::codegen::compile;
//use nessie::value::Value;
//use nessie::r#type::Type;
//use nessie::source_error::SourceError;

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

//unsafe fn interpret(input: &str, vm: &mut VM)
//-> Result<Option<(Value, Type)>, Error> {
//    let tokens = lex(input).map_err(Error::Lex)?;
//    let mut ast = parse(&tokens).map_err(Error::Parse)?;
//    typecheck(&mut ast).map_err(Error::Type)?;
//    let chunk = compile(&ast);
//
//    vm.stack.clear();
//    vm.run(&chunk);
//    Ok(vm.stack.pop().map(|v| (v, ast.body.ty.unwrap())))
//}

fn run_file(file_path: &Path) -> io::Result<()> {
    println!("Running {}", file_path.display());

    let source = read_to_string(file_path)?;

    let mut engine = Engine::new();
    match engine.typecheck(&source) {
        Ok(program) => {
            let value = engine.eval(&program);
            println!("{:?}", value);
        }
        Err(err) => {
            println!("{}", err);
        }
    }
    Ok(())
}

fn repl() -> io::Result<()> {
    let mut input_buf = String::new();
    let mut engine = Engine::new();
    *engine.debug_stream() = Some(Box::new(stdout()));

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

        match engine.typecheck(input) {
            Ok(program) => {
                let value = engine.eval(&program);
                println!("{:?}", value);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}

fn disassemble_file(file_path: &Path) -> io::Result<()> {
    let _name = file_path.file_name().unwrap().to_str().unwrap();
    let source = read_to_string(file_path)?;
    let mut engine = Engine::new();
    match engine.typecheck(&source) {
        Ok(program) => {
            engine.disassamble(&program, stdout())?;
        }
        Err(err) => {
            println!("{}", err);
        }
    }

    Ok(())
}

