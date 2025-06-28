#[cfg(test)]
mod tests {
    #[test]
    fn run_workspace_tests() {
        // This test is a placeholder to ensure that running `cargo test` in the root crate
        // includes at least one test.

        // To run all tests in the workspace, use:
        // cargo test --workspace

        // This will run tests for all crates in the workspace, including:
        // - The root crate
        // - komrad-cli
        // - komrad-core
        // - komrad-parser (including golden tests)
        // - komrad-interpreter
        // - komrad-macros
        // - komrad-web

        // By adding all workspace crates as dev-dependencies in the root Cargo.toml,
        // we ensure that changes to any crate will trigger a rebuild of the root crate,
        // making it easier to run all tests with a single command.
    }
}
