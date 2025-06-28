// pub async fn main

use komrad_interpreter::Interpreter;
use std::path::PathBuf;
use tracing::{debug, info};

pub(crate) async fn main(mut interpreter: Interpreter, file_path: &PathBuf, wait: bool) {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_max_level(interpreter.features().verbosity).init();

    match interpreter.load_and_run_file_path(file_path).await {
        Ok(_) => {
            debug!("File executed successfully.");
        }
        Err(e) => {
            tracing::error!("Error executing file: {}", e);
        }
    }

    tokio::time::sleep(std::time::Duration::from_secs(0)).await;

    if wait {
        info!("Press Ctrl+C to exit.");

        tokio::signal::ctrl_c().await.expect("Failed to listen for event");
    }
}