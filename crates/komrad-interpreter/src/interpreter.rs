use crate::agents::fs_agent::FsAgent;
use crate::agents::io_agent::{ConsoleIo, IoAgent, IoInterface, TracingIo};
use crate::agents::log_agent::LogAgent;
use crate::import_agent::ImportAgent;
use crate::spawn_agent::SpawnAgent;
use dashmap::DashMap;
use komrad_core::Agent;
use komrad_core::{AgentFactory, ParseError, RuntimeError};
use komrad_core::{CodeAtlas, Env, Evaluate, Spanned, Statement, TopLevel, Value};
use komrad_parser::parse_toplevel::parse_file_complete;
use komrad_web::HttpListenerFactory;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
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
    features: InterpreterFeatures,
}

impl Interpreter {
    pub fn features(&self) -> &InterpreterFeatures {
        &self.features
    }
}

pub struct InterpreterFeatures {
    pub io_uses_tracing: bool,
    pub verbosity: tracing::Level,
}

impl Default for InterpreterFeatures {
    fn default() -> Self {
        Self {
            io_uses_tracing: false,
            verbosity: tracing::Level::INFO,
        }
    }
}

impl Interpreter {
    pub async fn new(features: InterpreterFeatures) -> Self {
        let mut initial_bindings = HashMap::new();
        let initial_handlers = Vec::new();

        let io_interface: Arc<dyn IoInterface + Send + Sync> = if features.io_uses_tracing {
            Arc::new(TracingIo)
        } else {
            Arc::new(ConsoleIo)
        };
        let io_agent = IoAgent::new(io_interface);
        let io_agent_channel = io_agent.spawn();
        initial_bindings.insert("Io".to_string(), Value::Channel(io_agent_channel));

        let fs_agent = FsAgent::default();
        let fs_agent_channel = fs_agent.spawn();
        initial_bindings.insert("Fs".to_string(), Value::Channel(fs_agent_channel));

        let log_agent = LogAgent::default();
        let log_agent_channel = log_agent.spawn();
        initial_bindings.insert("Log".to_string(), Value::Channel(log_agent_channel));

        let mut env = Env::new(initial_bindings.clone(), initial_handlers);

        let factory_registry: Arc<DashMap<String, Box<dyn AgentFactory + Send + Sync>>> = Arc::new(DashMap::new());

        factory_registry.insert("HttpListener".to_string(), Box::new(HttpListenerFactory));

        let import_agent = ImportAgent::new(
            env.clone(),
            factory_registry.clone(),
            std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")), // Start with current working directory
        );
        let import_agent_channel = import_agent.spawn();
        env.set("import", Value::Channel(import_agent_channel)).await;

        let spawn_agent = SpawnAgent::new(env.clone(), factory_registry.clone());
        let spawn_agent_channel = spawn_agent.spawn();

        env.set("spawn", Value::Channel(spawn_agent_channel)).await;

        Self {
            env,
            codemaps: CodeAtlas::new(),
            features,
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