use clap::Parser;
use komrad_cli::cli::{Args, Command};

#[test]
fn test_default_command() {
    // When no subcommand is provided, it should default to the REPL command
    let args = Args::parse_from(["komrad"]);
    let command = args.command.unwrap_or_default();
    
    match command {
        Command::Repl { file } => {
            assert!(file.is_none(), "Default REPL command should have no file");
        }
        _ => panic!("Default command should be REPL"),
    }
}

#[test]
fn test_repl_command_with_file() {
    // Test the REPL command with a file argument
    let args = Args::parse_from(["komrad", "repl", "test.kom"]);
    let command = args.command.unwrap_or_default();
    
    match command {
        Command::Repl { file } => {
            assert!(file.is_some(), "REPL command should have a file");
            assert_eq!(file.unwrap().to_str().unwrap(), "test.kom");
        }
        _ => panic!("Command should be REPL with file"),
    }
}

#[test]
fn test_run_command() {
    // Test the Run command
    let args = Args::parse_from(["komrad", "run", "test.kom"]);
    let command = args.command.unwrap_or_default();
    
    match command {
        Command::Run { file, wait } => {
            assert_eq!(file.to_str().unwrap(), "test.kom");
            assert!(!wait, "Wait flag should be false by default");
        }
        _ => panic!("Command should be Run"),
    }
}

#[test]
fn test_run_command_with_wait() {
    // Test the Run command with the wait flag
    let args = Args::parse_from(["komrad", "run", "--wait", "test.kom"]);
    let command = args.command.unwrap_or_default();
    
    match command {
        Command::Run { file, wait } => {
            assert_eq!(file.to_str().unwrap(), "test.kom");
            assert!(wait, "Wait flag should be true");
        }
        _ => panic!("Command should be Run with wait flag"),
    }
}