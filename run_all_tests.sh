#!/bin/bash
set -e

# Run all tests in the workspace
echo "Running all tests in the workspace..."
cargo test --workspace

# Run golden tests specifically
echo "Running golden tests..."
cargo test golden --package komrad-parser -- --show-output

echo "All tests completed successfully!"