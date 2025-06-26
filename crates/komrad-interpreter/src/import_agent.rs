use dashmap::DashMap;
use komrad_core::{Agent, AgentFactory, AgentLifecycle, Env, MessageHandler, Value};
use komrad_core::{CodeAtlas, Evaluate, TopLevel};
use komrad_macros::Agent;
use komrad_parser::parse_toplevel::parse_file_complete;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs;

#[derive(Agent)]
pub struct ImportAgent {
    pub env: Env,
    pub factory_registry: Arc<DashMap<String, Box<dyn AgentFactory + Send + Sync>>>,
    pub current_dir: PathBuf,

    /// Cache for loaded modules to avoid reloading them
    module_cache: HashMap<String, Value>,

    /// CodeAtlas for tracking source code locations and spans
    codemaps: CodeAtlas,

    /// To detect circular imports, we keep track of the import stack
    import_stack: HashSet<PathBuf>,
}

impl ImportAgent {
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
            import_stack: HashSet::new(),
        }
    }

    async fn load_module(&mut self, module_path: &str) -> Value {
        let file_path = self.current_dir.join(format!("{}.kom", module_path));
        let canonical_path = match file_path.canonicalize() {
            Ok(path) => path,
            Err(_) => file_path.clone(),
        };

        // Check for cycles first
        if self.import_stack.contains(&canonical_path) {
            return Value::RemoteError(format!(
                "Circular import detected: {} -> ... -> {}",
                canonical_path.display(),
                canonical_path.display()
            ));
        }

        // THEN check cache
        let cache_key = format!("{}:{}", self.current_dir.display(), module_path);
        if let Some(cached_value) = self.module_cache.get(&cache_key) {
            return cached_value.clone();
        }

        // Add to import stack before loading
        self.import_stack.insert(canonical_path.clone());

        let result = match fs::read_to_string(&file_path).await {
            Ok(code) => {
                match parse_file_complete(&mut self.codemaps, &code, Some(file_path.clone())) {
                    Ok(toplevel) => {
                        let mut child_env = self.env.clone_child().await;

                        if let Some(parent_dir) = file_path.parent() {
                            let mut child_import_agent = ImportAgent::new(
                                child_env.clone(),
                                self.factory_registry.clone(),
                                parent_dir.to_path_buf(),
                            );
                            // Propagate the import stack to detect cycles across agents
                            child_import_agent.import_stack = self.import_stack.clone();

                            let child_import_channel = child_import_agent.spawn();
                            child_env.set("import", Value::Channel(child_import_channel)).await;
                        }

                        let result = match toplevel {
                            TopLevel::Block(block) => block.evaluate(&mut child_env).await,
                            TopLevel::Statement(statement) => statement.evaluate(&mut child_env).await,
                        };

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
        };

        // Remove from import stack after loading
        self.import_stack.remove(&canonical_path);
        result
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