use async_trait::async_trait;
use indexmap::IndexMap;
use std::sync::Arc;
use tracing::{debug, error, info, trace};

use komrad_core::{Agent, AgentLifecycle, Block, Channel, Destructure, DestructureResult, Env, Evaluate, Message, MessageHandler, PatternDestructure, ToSExpr, Value};
use komrad_macros::Agent;

/// A DynamicAgent that, when initialized, evaluates its associated block
/// to set up handlers and other state. Incoming messages match the
/// environment’s handlers by pattern destructure.
#[derive(Agent)]
pub struct DynamicAgent {
    pub env: Env,
    pub block: Arc<Block>,
    me: Option<Channel>,
}

impl DynamicAgent {
    /// Create a new DynamicAgent given an environment and the agent’s top-level block.
    pub fn new(env: Env, block: Arc<Block>) -> Self {
        DynamicAgent {
            env,
            block,
            // Will be set in on_init
            me: None,
        }
    }
}

#[async_trait]
impl AgentLifecycle for DynamicAgent {
    async fn on_init(
        &mut self,
        channel: Channel,
        initializer_map: IndexMap<String, Value>,
    ) {
        info!("DynamicAgent on_init");
        // Provide "me" so code in the block can reference itself.
        self.me = Some(channel.clone());
        self.env.set("me", Value::Channel(channel.clone())).await;

        // Evaluate the block in the environment to load handlers, etc.
        // Typically, the block is a series of statements that define
        // patterns, variables, or do side effects.

        let mut initializer_result = Value::Null;
        for stmt in &self.block.0 {
            initializer_result = stmt.evaluate(&mut self.env).await;
            debug!("DynamicAgent block stmt: {:?} => {:?}", stmt.to_sexpr(), initializer_result.to_sexpr());
        }
        debug!("DynamicAgent block result: {:?}", initializer_result.to_sexpr());

        // Apply additional initializer bindings (like the old system).
        for (key, value) in initializer_map {
            self.env.set(&key, value).await;
        }

        // Debug output
        for handler in self.env.handlers().await {
            debug!("Handler: {:?}", handler.pattern.value.to_sexpr());
        }
        let bindings = self.env.bindings().await;
        for (k, v) in bindings.iter() {
            debug!("Binding: {} => {:?}", k, v.to_sexpr());
        }
    }

    async fn on_start(&mut self) {
        debug!("DynamicAgent on_start");

        // Like the old system, we forcibly send a "start" message to ourselves.
        // That triggers any handler matching ["start"], etc.
        let start_msg = Message::new(Value::List(vec![Value::Word("start".to_string())]), None);
        let reply = self.on_message(&start_msg).await;
        info!("DynamicAgent on_start; forced 'start' message => {:?}", reply.to_sexpr());
    }

    async fn on_stop(&mut self) {
        info!("DynamicAgent on_stop");

        // If there's any channel stored in the environment’s bindings, we can stop it
        // or do any cleanup as needed. The snippet below is optional.
        let bindings = self.env.bindings().await;
        for value in bindings.values() {
            if let Value::Channel(ch) = value {
                // For example, we might signal them to stop:
                let _ = ch.stop().await;
            }
        }
    }
}

#[async_trait]
impl MessageHandler for DynamicAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        trace!("DynamicAgent received message: {:?}", message);

        // Attempt to match the incoming message’s value against each handler’s pattern.
        let handlers = self.env.handlers().await;
        for handler in &handlers {
            let pattern = &handler.pattern.value;
            let destruct = PatternDestructure::destructure(pattern, message.value());
            match destruct {
                DestructureResult::Match(bindings) => {
                    trace!("DynamicAgent handler matched: {:?}", pattern);

                    // Create a handler scope so changes propagate up if the variable already existed.
                    let mut child_env = self.env.clone_handler_scope().await;
                    // Insert the matched bindings
                    for (k, v) in bindings {
                        child_env.set(&k, v).await;
                    }

                    // Evaluate the handler's expression. In your new AST, that’s `handler.expr`.
                    let result = handler.expr.evaluate(&mut self.env).await;
                    return Some(result);
                }
                DestructureResult::NoMatch => {
                    // keep looking
                }
                DestructureResult::Err(e) => {
                    error!("Handler pattern error: {:?}", e);
                    // keep looking or break, as desired
                }
            }
        }

        // If no handler matched, we return an error (similar to old logic).
        Some(Value::RemoteError(format!(
            "No handler found for message: {:?}",
            message.value()
        )))
    }
}
