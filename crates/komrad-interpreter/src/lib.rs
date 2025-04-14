use komrad_core::{Block, CodeAtlas, Env, RuntimeError, Value};
use std::path::PathBuf;

pub type InterpreterResult<T> = Result<T, RuntimeError>;

pub struct Interpreter {
    codemaps: CodeAtlas,
    env: Env,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            codemaps: CodeAtlas::new(),
        }
    }

    // The difference between most of the run modes is how we acquire the input
    // span and add it to the codemaps.

    pub fn run_block(&mut self, block: Block) -> InterpreterResult<Value> {}

    pub fn run_statement(&mut self, statement: &str) -> InterpreterResult<Value> {}

    pub fn run_string(&mut self, input: &str) -> InterpreterResult<Value> {}

    pub fn run_file(&mut self, path: &PathBuf) -> InterpreterResult<Value> {}
}
