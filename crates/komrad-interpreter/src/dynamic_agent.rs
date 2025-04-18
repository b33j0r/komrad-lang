use async_trait::async_trait;
use indexmap::IndexMap;
use std::sync::Arc;
use tracing::{error, info, trace};

use komrad_core::{
    AgentLifecycle, Block, Channel, Destructure, DestructureResult, Env, Evaluate, Expr,
    Message, MessageHandler, PatternDestructure, ToSExpr, Value,
};
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
    async fn on_init(&mut self, channel: Channel, initializer_map: IndexMap<String, Value>) {
        trace!("DynamicAgent on_init");
        // Provide "me" so code in the block can reference itself.
        self.me = Some(channel.clone());
        self.env.set("me", Value::Channel(channel.clone())).await;

        // Evaluate the block in the environment to load handlers, etc.
        // Typically, the block is a series of statements that define
        // patterns, variables, or do side effects.

        let initializer_result = self.block.evaluate(&mut self.env).await;
        trace!(
            "DynamicAgent block result: {:?}",
            initializer_result.to_sexpr()
        );

        // Apply additional initializer bindings (like the old system).
        for (key, value) in initializer_map {
            self.env.set(&key, value).await;
        }

        // Debug output
        for handler in self.env.handlers().await {
            trace!("Handler: {:?}", handler.pattern.value.to_sexpr());
        }
        let bindings = self.env.bindings().await;
        for (k, v) in bindings.iter() {
            trace!("Binding: {} => {:?}", k, v.to_sexpr());
        }
    }

    async fn on_start(&mut self) {
        trace!("DynamicAgent on_start");
        let start_msg = Message::new(Value::List(vec![Value::Word("start".to_string())]), None);
        let reply = self.on_message(&start_msg).await;
        trace!(
            "DynamicAgent on_start; `start` message => {:?}",
            reply.to_sexpr()
        );
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
        trace!("DynamicAgent received message: {:?}", message.to_sexpr());

        // Attempt to match the incoming message’s value against each handler’s pattern.
        let handlers = self.env.handlers().await;
        for handler in &handlers {
            let pattern = &handler.pattern.value;
            trace!("DynamicAgent handler pattern: {:?}", pattern.to_sexpr());
            let destruct = PatternDestructure::destructure(pattern, message.value());
            match destruct {
                DestructureResult::Match(bindings) => {
                    trace!("DynamicAgent handler matched: {:?}", pattern.to_sexpr());

                    // Create a handler scope so changes propagate up if the variable already existed.
                    let mut child_env = self.env.clone_handler_scope().await;
                    // Insert the matched bindings
                    for (k, v) in bindings {
                        child_env.set(&k, v).await;
                    }

                    // Evaluate the handler's expression.
                    match &*handler.expr {
                        Expr::Value(spanned_value) => {
                            match &*spanned_value.value {
                                // For block we actually have to evaluate the statements
                                Value::Block(block) => {
                                    // Evaluate the block in the handler's environment
                                    let result = block.evaluate(&mut child_env).await;
                                    return Some(result);
                                }
                                _ => {
                                    // If the handler's expression is not a block, we evaluate it directly.
                                    let result = handler.expr.evaluate(&mut child_env).await;
                                    return Some(result);
                                }
                            }
                        }
                        _ => {
                            // If the handler's expression is not a Value variant, we evaluate it directly.
                            let result = handler.expr.evaluate(&mut child_env).await;
                            return Some(result);
                        }
                    }
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