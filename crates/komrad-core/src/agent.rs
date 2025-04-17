use crate::channel::{Channel, ChannelListener, ControlMessage, Message};
use crate::env::Env;
use crate::value::Value;
use async_trait::async_trait;
use indexmap::IndexMap;
use std::sync::Arc;
use tokio::select;
use tracing::{error, info};

/// An object that spawns a specific type of agent
#[async_trait]
pub trait AgentFactory {
    async fn create_agent(&self, env: Env) -> Box<dyn Agent>;
}

#[async_trait]
pub trait MessageHandler {
    async fn on_message(&mut self, message: &Message) -> Option<Value>;
}

#[async_trait]
pub trait Agent: MessageHandler + AgentLifecycle {
    fn spawn(self) -> Channel;

    fn spawn_with_initializer(self: Box<Self>, initializer_map: IndexMap<String, Value>)
                              -> Channel;

    async fn run(
        &mut self,
        channel: Channel,
        listener: Arc<ChannelListener>,
        initializer_map: IndexMap<String, Value>,
    ) where
        Self: Sized,
    {
        self.on_init(channel.clone(), initializer_map).await;
        self.on_start().await;
        loop {
            select! {
                message = listener.recv() => {
                    match message {
                        Ok(msg) => {
                            let response = self.on_message(&msg).await.unwrap_or(Value::Null);
                            if let Some(reply_to) = msg.reply_to {
                                match reply_to.send(response.into()).await {
                                    Ok(_) => {}
                                    Err(err) => {
                                        error!("Error sending response: {:?}", err);
                                    }
                                }
                            }
                            
                        }
                        Err(err) => {
                            error!("Error receiving message: {:?}", err);
                        }
                    }
                }
                ctrl = listener.recv_ctl() => {
                    match ctrl {
                        Ok(ControlMessage::Stop) => break,
                        Err(err) => {
                            error!("Error receiving control message: {:?}", err);
                        }
                    }
                }
            }
        }
    }
}

#[async_trait]
pub trait AgentLifecycle {
    async fn on_init(&mut self, channel: Channel, initializer_map: IndexMap<String, Value>) {
        info!("Agent initialized: {:?}", channel);
        for (key, value) in initializer_map {
            info!("Initializer: {} = {:?}", key, value);
        }
    }

    async fn on_start(&mut self) {}

    async fn on_stop(&mut self) {}
}
