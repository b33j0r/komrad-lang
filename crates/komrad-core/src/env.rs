use crate::ast::{Handler, Value};
use indexmap::IndexMap;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct Env {
    scope_stack: VecDeque<Arc<Scope>>,
    handlers: Vec<Arc<Handler>>,
    is_handler_scope: bool,
}

impl Env {
    pub fn new(
        initial_bindings: HashMap<String, Value>,
        initial_handlers: Vec<Arc<Handler>>,
    ) -> Self {
        let scope = Scope::new(initial_bindings);
        let mut scope_stack = VecDeque::new();
        scope_stack.push_back(Arc::new(scope));

        Env {
            is_handler_scope: false,
            scope_stack,
            handlers: initial_handlers,
        }
    }

    /// Create a fresh child environment:
    /// 1. Shares all parent scopes via arcs
    /// 2. Adds a new empty (fresh) scope on top
    /// 3. Does not inherit handlers
    pub async fn clone_child(&self) -> Self {
        let mut new_stack = self.scope_stack.clone();
        // Push an empty scope on top for the child
        new_stack.push_back(Arc::new(Scope::default()));

        Env {
            scope_stack: new_stack,
            // No handlers inherited from the parent
            handlers: Vec::new(),
            is_handler_scope: false,
        }
    }

    /// Create a handler scope environment:
    /// 1. Shares all parent scopes via arcs
    /// 2. Adds a new empty (fresh) scope on top
    /// 3. Does not inherit handlers
    /// 4. Sets the is_handler_scope flag to true
    /// 4.b. that makes `set` check the immediate parent
    pub async fn clone_handler_scope(&self) -> Self {
        let mut new_stack = self.scope_stack.clone();
        // Push an empty scope on top for the child
        new_stack.push_back(Arc::new(Scope::default()));

        Env {
            scope_stack: new_stack,
            // No handlers inherited from the parent
            handlers: Vec::new(),
            is_handler_scope: true,
        }
    }

    pub fn clone_for_agent(&self) -> Self {
        Env {
            scope_stack: self.scope_stack.clone(), // Arc-ed stack
            handlers: self.handlers.clone(),       // Arc-ed handlers
            is_handler_scope: self.is_handler_scope,
        }
    }

    /// Look through scopes from innermost to outermost for a variable
    pub async fn get(&self, name: &str) -> Option<Value> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.get(name).await {
                return Some(value);
            }
        }
        None
    }

    pub async fn set(&mut self, name: &str, value: Value) {
        if self.is_handler_scope {
            // Handler scope: Check immediate parent before setting new value
            if self.scope_stack.len() >= 2 {
                let immediate_parent_scope = &self.scope_stack[self.scope_stack.len() - 2];
                if immediate_parent_scope.get(name).await.is_some() {
                    immediate_parent_scope.set(name, value).await;
                    return;
                }
            }
        }

        // Default behavior: set in current scope if not overwriting parent binding
        if let Some(current_scope) = self.scope_stack.back() {
            current_scope.set(name, value).await;
            return;
        }
    }

    /// Update the current environment with a new set of bindings
    pub async fn update(&mut self, bindings: IndexMap<String, Value>) {
        for (key, value) in bindings {
            self.set(&key, value).await;
        }
    }

    /// Push a new handler onto this environment's handler list
    pub async fn push_handler(&mut self, handler: Arc<Handler>) {
        self.handlers.push(handler);
    }

    /// Get the current list of handlers
    pub async fn handlers(&self) -> Vec<Arc<Handler>> {
        self.handlers.clone()
    }

    /// Get the current list of bindings
    pub async fn bindings(&self) -> IndexMap<String, Value> {
        let mut bindings = IndexMap::new();
        for scope in self.scope_stack.iter() {
            let scope_bindings = scope.bindings.read().await;
            for (key, value) in scope_bindings.iter() {
                bindings.insert(key.clone(), value.clone());
            }
        }
        bindings
    }
}

impl Default for Env {
    fn default() -> Self {
        Env::new(HashMap::new(), Vec::new())
    }
}

#[derive(Clone)]
pub struct Scope {
    bindings: Arc<RwLock<HashMap<String, Value>>>,
}

impl Scope {
    pub fn new(initial_bindings: HashMap<String, Value>) -> Self {
        Scope {
            bindings: Arc::new(RwLock::new(initial_bindings)),
        }
    }

    /// Read a variable binding
    pub async fn get(&self, name: &str) -> Option<Value> {
        let bindings = self.bindings.read().await;
        bindings.get(name).cloned()
    }

    pub async fn set(&self, name: &str, value: Value) {
        let mut bindings = self.bindings.write().await;
        bindings.insert(name.to_string(), value);
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new(HashMap::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, Expr, Pattern, Spanned, Value};
    use std::collections::HashMap;

    #[tokio::test]
    async fn test_env() {
        let mut env = Env::new(HashMap::new(), Vec::new());
        env.set("x", Value::Int(42)).await;
        assert_eq!(env.get("x").await, Some(Value::Int(42)));
    }

    #[tokio::test]
    async fn test_scope() {
        let scope = Scope::new(HashMap::new());
        let scope_clone = scope.clone();
        scope_clone
            .set("y", Value::String("Hello".to_string()))
            .await;

        assert_eq!(
            scope_clone.get("y").await,
            Some(Value::String("Hello".to_string()))
        );
        // The original scope (not mutated afterward) won't see y if we didn't reassign it
    }

    #[tokio::test]
    async fn test_env_clone_child() {
        let mut env = Env::new(HashMap::new(), Vec::new());
        env.set("x", Value::Int(42)).await;
        let block = Block(Vec::new());
        let block_value = Value::Block(Arc::new(block));
        let spanned_value = Spanned::new(
            crate::ast::Span::default(),
            Expr::Value(Spanned::new(crate::ast::Span::default(), block_value)),
        );
        let handler = Handler {
            pattern: Pattern::new_word("foo".to_string()),
            expr: spanned_value,
        };
        env.push_handler(handler).await;

        // Clone a child environment
        let mut child_env = env.clone_child().await;
        // Child inherits all scope data
        assert_eq!(child_env.get("x").await, Some(Value::Int(42)));
        // Child does not inherit handlers
        assert_eq!(child_env.handlers().await.len(), 0);
        assert_eq!(env.handlers().await.len(), 1);

        // Mutate the child
        child_env.set("x", Value::Int(100)).await;
        assert_eq!(child_env.get("x").await, Some(Value::Int(100)));

        // Parent not affected
        assert_eq!(env.get("x").await, Some(Value::Int(42)));
    }

    #[tokio::test]
    async fn test_env_clone_handler_scope() {
        let mut env = Env::new(HashMap::new(), Vec::new());
        env.set("x", Value::Int(42)).await;
        env.push_handler(Arc::new(Handler {
            pattern: Pattern::new_word("foo".to_string()),
            expr: Spanned::new(
                crate::ast::Span::default(),
                Expr::Value(Spanned::new(crate::ast::Span::default(), Value::Int(100))),
            ),
        }))
            .await;

        // Clone a child environment
        let mut child_env = env.clone_handler_scope().await;
        // Child inherits all scope data
        assert_eq!(child_env.get("x").await, Some(Value::Int(42)));
        // Child does not inherit handlers
        assert_eq!(child_env.handlers().await.len(), 0);
        assert_eq!(env.handlers().await.len(), 1);

        // Mutate the child, which will affect the parent
        child_env.set("x", Value::Int(100)).await;
        assert_eq!(child_env.get("x").await, Some(Value::Int(100)));
        // Parent IS affected
        assert_eq!(env.get("x").await, Some(Value::Int(100)));
    }
}
