use std::path::PathBuf;
pub use clap::{Parser, Subcommand};


#[derive(Parser, Debug)]
#[clap(
    author = "Yonatan Reicher",
    version = "0.1.0",
    about,
    long_about = None,
)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    #[clap(about = "Run an input program")]
    Run {
        file_path: PathBuf,
    },
}


impl Cli {
    /// Creates a new `Cli` instance that parses the command line arguments.
    pub fn parse() -> Self {
        // Delegade to the Parser trait
        <Cli as Parser>::parse()
    }
}

