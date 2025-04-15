#![feature(box_patterns)]

use komrad_core::{CodeAtlas, Env, Evaluate, EvaluationContext, RuntimeError, Spanned, Statement, TopLevel, Value};
use std::collections::HashMap;

pub type InterpreterResult<T> = Result<T, RuntimeError>;

pub struct Interpreter {
    codemaps: CodeAtlas,
    evaluation_context: EvaluationContext,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            codemaps: CodeAtlas::new(),
            evaluation_context: EvaluationContext {
                env: Env::new(
                    HashMap::new(),
                    Vec::new(),
                )
            },
        }
    }

    pub async fn run_statement(&mut self, statement: Spanned<Statement>) -> InterpreterResult<Value> {
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
            TopLevel::Statement(statement) => {
                self.run_statement(statement).await
            }
        }
    }
}
