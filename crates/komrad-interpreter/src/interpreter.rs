use crate::agents::fs_agent::FsAgent;
use crate::agents::io_agent::IoAgent;
use crate::agents::log_agent::LogAgent;
use crate::spawn_agent::SpawnAgent;
#[allow(unused_imports)]
use komrad_core::Agent;
use komrad_core::{AgentFactory, ParseError, RuntimeError};
use komrad_core::{CodeAtlas, Env, Evaluate, Spanned, Statement, TopLevel, Value};
use komrad_parser::parse_toplevel::parse_file_complete;
use komrad_web::HttpListenerFactory;
use std::collections::HashMap;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use thiserror::Error;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("Runtime error: {0}")]
    RuntimeError(#[from] Spanned<RuntimeError>),

    #[error("Parse error: {0}")]
    ParseError(#[from] ParseError),

    #[error("File not found: {0}")]
    FileNotFound(String),

    #[error("Failed to read file: {0}")]
    FileReadError(#[from] std::io::Error),
}

pub struct Interpreter {
    codemaps: CodeAtlas,
    env: Env,
}

impl Interpreter {
    pub async fn new() -> Self {
        let mut initial_bindings = HashMap::new();
        let initial_handlers = Vec::new();

        let io_agent = IoAgent::default();
        let io_agent_channel = io_agent.spawn();
        initial_bindings.insert("Io".to_string(), Value::Channel(io_agent_channel));

        let fs_agent = FsAgent::default();
        let fs_agent_channel = fs_agent.spawn();
        initial_bindings.insert("Fs".to_string(), Value::Channel(fs_agent_channel));

        let log_agent = LogAgent::default();
        let log_agent_channel = log_agent.spawn();
        initial_bindings.insert("Log".to_string(), Value::Channel(log_agent_channel));

        let mut env = Env::new(initial_bindings.clone(), initial_handlers);

        let mut factory_registry: HashMap<String, Box<dyn AgentFactory + Send + Sync>> = HashMap::new();

        factory_registry.insert("HttpListener".to_string(), Box::new(HttpListenerFactory));

        let spawn_agent = SpawnAgent::new(env.clone(), factory_registry);
        let spawn_agent_channel = spawn_agent.spawn();

        env.set("spawn", Value::Channel(spawn_agent_channel)).await;

        Self {
            env,
            codemaps: CodeAtlas::new(),
        }
    }

    pub async fn run_statement(
        &mut self,
        statement: Spanned<Statement>,
    ) -> InterpreterResult<Value> {
        let result = statement.evaluate(&mut self.env).await;
        Ok(result)
    }

    pub async fn run_top_level(&mut self, top_level: TopLevel) -> InterpreterResult<Value> {
        match top_level {
            TopLevel::Block(block) => {
                let result = block.evaluate(&mut self.env).await;
                if let Value::Error(error) = result {
                    return Err(InterpreterError::RuntimeError(error));
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
            Err(e) => Err(InterpreterError::ParseError(e)),
        }
    }
}
