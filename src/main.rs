mod cli;
mod chunk;
mod disassemble;
mod value;
mod vm;

use std::path::Path;


fn main() {
    let cli = cli::Cli::parse();

    match cli.command {
        cli::Command::Run { file_path } => {
            run_file(&file_path);
        }
    }
}

fn run_file(file: &Path) {
    println!("Running {}", file.display());
}

