use async_trait::async_trait;
use dashmap::DashMap;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::info;

use komrad_core::{Agent, AgentFactory, AgentLifecycle, Env, Message, MessageHandler, Value};
use komrad_macros::Agent;

use crate::dynamic_agent::DynamicAgent;

/// A built-in agent that spawns a DynamicAgent from a Block or from a named factory.
#[derive(Agent)]
pub struct SpawnAgent {
    pub env: Env,
    // For named (non-block) agents, we have a registry of factories:
    pub factory_registry: Arc<DashMap<String, Box<dyn AgentFactory + Send + Sync>>>,
}

impl SpawnAgent {
    /// Create a new SpawnAgent with the given environment and factory registry.
    pub fn new(
        env: Env,
        factory_registry: Arc<DashMap<String, Box<dyn AgentFactory + Send + Sync>>>,
    ) -> Self {
        SpawnAgent { env, factory_registry }
    }
}

#[async_trait]
impl AgentLifecycle for SpawnAgent {
    async fn on_stop(&mut self) {
        info!("SpawnAgent on_stop");
    }
}

#[async_trait]
impl MessageHandler for SpawnAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        // Old system logic: message.value() is expected to be a list:
        //   [ blockOrName, (optional) dictOfInitializers ]
        // We'll adapt it to the new Value/AST approach as best we can.
        match message.value() {
            Value::List(list) if !list.is_empty() => {
                // 1) Extract optional initializer from second item in the list
                let initializer_map: IndexMap<String, Value> =
                    if let Some(val) = list.get(1) {
                        if let Value::Dict(dict) = val {
                            // Convert the Value::Dict(IndexMap) into the needed initializer
                            dict.into()
                        } else {
                            return Some(Value::RemoteError(
                                "SpawnAgent: second list element must be a dict for initializer"
                                    .to_string(),
                            ));
                        }
                    } else {
                        // No initializer => empty
                        IndexMap::new()
                    };

                // 2) Distinguish: block or word?
                let first_item = &list[0];
                let maybe_agent: Option<Box<dyn Agent>> = match first_item {
                    // If it's a block, we spawn a DynamicAgent using that block
                    Value::Block(b) => {
                        // Clone a child env so that the spawned agent sees
                        // the parent's definitions but has its own top scope.
                        let child_env = futures::executor::block_on(self.env.clone_child());
                        Some(Box::new(DynamicAgent::new(child_env, b.clone())))
                    }
                    // If it's a Word, see if there's a named factory
                    Value::Word(name) => {
                        if let Some(factory) = self.factory_registry.get(name) {
                            let child_env = futures::executor::block_on(self.env.clone_child());
                            Some(factory.create_agent(child_env).await)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                // 3) If we recognized the item, spawn it
                if let Some(agent) = maybe_agent {
                    let agent_channel = agent.spawn_with_initializer(initializer_map);
                    Some(Value::Channel(agent_channel))
                } else {
                    Some(Value::RemoteError(
                        "SpawnAgent: expected Block or Word as first item".to_string(),
                    ))
                }
            }
            _ => Some(Value::RemoteError(
                "SpawnAgent: input must be a list".to_string(),
            )),
        }
    }
}
