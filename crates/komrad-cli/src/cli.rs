use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Top-level argument parser. The subcommand is optional.
#[derive(Parser)]
#[command(author, version, about)]
pub struct Args {
    #[command(subcommand)]
    pub command: Option<Command>,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,
}

/// Your supported subcommands.
#[derive(Subcommand, Debug, Clone)]
pub enum Command {
    /// Start the REPL.
    ///
    /// The file argument is optional so that the REPL can be the default behavior.
    Repl {
        /// Optional file to load on startup.
        file: Option<PathBuf>,
    },

    /// Run in non-interactive mode.
    Run {
        /// The file to run.
        file: PathBuf,

        #[clap(short, long)]
        wait: bool,
    },
}

/// Provide a default value for the Command enum.
/// When no subcommand is provided, we want to default to `Command::Repl { file: None }`.
impl Default for Command {
    fn default() -> Self {
        Command::Repl { file: None }
    }
}

pub async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse the command line arguments.
    let args = Args::parse();

    // If no subcommand is provided, default to the REPL command.
    // Thanks to the Default implementation we can do this neatly.
    let command = args.command.unwrap_or_default();
    let io_uses_tracing = matches!(command, Command::Repl { .. });

    let features = komrad_interpreter::InterpreterFeatures {
        io_uses_tracing,
        verbosity: args.verbosity.tracing_level().unwrap(),
        ..Default::default()
    };

    // Initialize your interpreter.
    let interpreter = komrad_interpreter::Interpreter::new(features).await;

    // Match on the command.
    match command {
        Command::Repl { file } => {
            crate::repl::main(interpreter, &file).await?;
        }
        Command::Run { file, wait } => {
            crate::run::main(interpreter, &file, wait).await;
        }
    }
    Ok(())
}
