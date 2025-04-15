#[tokio::main]
async fn main() {
    match komrad_cli::repl::main().await {
        Ok(_) => println!("REPL exited successfully."),
        Err(e) => eprintln!("Error: {}", e),
    }
}
