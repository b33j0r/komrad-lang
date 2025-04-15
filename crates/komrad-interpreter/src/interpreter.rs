use komrad_core::RuntimeError;
use komrad_core::{CodeAtlas, Env, Evaluate, EvaluationContext, Spanned, Statement, TopLevel, Value};
use komrad_parser::parse_toplevel::parse_file_complete;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use thiserror::Error;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("Runtime error: {0}")]
    RuntimeError(#[from] RuntimeError),

    #[error("File not found: {0}")]
    FileNotFound(String),

    #[error("Failed to read file: {0}")]
    FileReadError(#[from] std::io::Error),
}

pub struct Interpreter {
    codemaps: CodeAtlas,
    evaluation_context: EvaluationContext,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            codemaps: CodeAtlas::new(),
            evaluation_context: EvaluationContext {
                env: Env::new(HashMap::new(), Vec::new()),
            },
        }
    }

    pub async fn run_statement(
        &mut self,
        statement: Spanned<Statement>,
    ) -> InterpreterResult<Value> {
        let result = statement.evaluate(&mut self.evaluation_context).await;
        Ok(result)
    }

    pub async fn run_top_level(&mut self, top_level: TopLevel) -> InterpreterResult<Value> {
        match top_level {
            TopLevel::Block(block) => {
                let mut result = Value::Null;
                for statement in block.0 {
                    match self.run_statement(statement).await {
                        Ok(value) => result = value,
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(result)
            }
            TopLevel::Statement(statement) => self.run_statement(statement).await,
        }
    }

    pub async fn load_and_run_file_path(
        &mut self,
        file_path: &Path,
    ) -> InterpreterResult<Value> {
        let code = tokio::fs::read_to_string(file_path).await?;
        let toplevel = parse_file_complete(&mut self.codemaps, &code, Some(PathBuf::from(file_path)));
        match toplevel {
            Ok(toplevel) => self.run_top_level(toplevel).await,
            Err(e) => Err(InterpreterError::RuntimeError(RuntimeError::ParseError(e))),
        }
    }
}