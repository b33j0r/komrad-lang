#[tokio::main]
async fn main() {
    match komrad_cli::cli::main().await {
        Ok(_) => {}
        Err(e) => eprintln!("Error: {}", e),
    }
}
