use dashmap::DashMap;
use komrad_core::{Agent, AgentFactory, AgentLifecycle, Env, MessageHandler, Value};
use komrad_core::{CodeAtlas, Evaluate, TopLevel};
use komrad_macros::Agent;
use komrad_parser::parse_toplevel::parse_file_complete;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs;

#[derive(Agent)]
pub struct ImportAgent {
    pub env: Env,
    pub factory_registry: Arc<DashMap<String, Box<dyn AgentFactory + Send + Sync>>>,
    pub current_dir: PathBuf,  // Key addition: knows its directory context
    module_cache: HashMap<String, Value>,
    codemaps: CodeAtlas,
}

impl ImportAgent {
    /// Create a new ImportAgent with the given environment, factory registry, and current directory
    pub fn new(
        env: Env,
        factory_registry: Arc<DashMap<String, Box<dyn AgentFactory + Send + Sync>>>,
        current_dir: PathBuf,
    ) -> Self {
        ImportAgent {
            env,
            factory_registry,
            current_dir,
            module_cache: HashMap::new(),
            codemaps: CodeAtlas::new(),
        }
    }

    async fn load_module(&mut self, module_path: &str) -> Value {
        // Check cache first (cache key includes current dir context)
        let cache_key = format!("{}:{}", self.current_dir.display(), module_path);
        if let Some(cached_value) = self.module_cache.get(&cache_key) {
            return cached_value.clone();
        }

        // Resolve path relative to current directory
        let file_path = self.current_dir.join(format!("{}.kom", module_path));

        match fs::read_to_string(&file_path).await {
            Ok(code) => {
                match parse_file_complete(&mut self.codemaps, &code, Some(file_path.clone())) {
                    Ok(toplevel) => {
                        // Create a child environment for module evaluation
                        let mut child_env = self.env.clone_child().await;

                        // CRITICAL: Create a new ImportAgent for the imported file's directory
                        // This shadows the parent's import agent with one that knows the new context
                        if let Some(parent_dir) = file_path.parent() {
                            let child_import_agent = ImportAgent::new(
                                child_env.clone(),
                                self.factory_registry.clone(),
                                parent_dir.to_path_buf(),
                            );
                            let child_import_channel = child_import_agent.spawn();
                            child_env.set("import", Value::Channel(child_import_channel)).await;
                        }

                        // Evaluate the module in the child environment (with shadowed import)
                        let result = match toplevel {
                            TopLevel::Block(block) => block.evaluate(&mut child_env).await,
                            TopLevel::Statement(statement) => statement.evaluate(&mut child_env).await,
                        };

                        // Cache the result
                        self.module_cache.insert(cache_key, result.clone());
                        result
                    }
                    Err(parse_error) => {
                        Value::RemoteError(format!("Parse error in module '{}': {}", module_path, parse_error))
                    }
                }
            }
            Err(io_error) => {
                Value::RemoteError(format!("Could not read module '{}': {}", module_path, io_error))
            }
        }
    }
}

impl AgentLifecycle for ImportAgent {}

#[async_trait::async_trait]
impl MessageHandler for ImportAgent {
    async fn on_message(&mut self, message: &komrad_core::Message) -> Option<komrad_core::Value> {
        match message.value() {
            Value::String(module_path) => {
                Some(self.load_module(module_path).await)
            }
            Value::List(list) if !list.is_empty() => {
                match &list[0] {
                    Value::String(module_path) => {
                        Some(self.load_module(module_path).await)
                    }
                    _ => Some(Value::RemoteError(
                        "ImportAgent: expected module path as String".to_string(),
                    ))
                }
            }
            _ => Some(Value::RemoteError(
                "ImportAgent: expected String (module path) or List".to_string(),
            )),
        }
    }
}