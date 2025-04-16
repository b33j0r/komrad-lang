use async_trait::async_trait;
use komrad_core::{AgentLifecycle, Message, MessageHandler, Value};
use komrad_macros::Agent;

#[derive(Agent)]
pub struct LogAgent;

impl Default for LogAgent {
    fn default() -> Self {
        LogAgent
    }
}

pub enum LogMessage {
    Trace(Value),
    Debug(Value),
    Info(Value),
    Warning(Value),
    Error(Value),
}

impl LogAgent {
    pub fn new() -> Self {
        LogAgent
    }
}

#[async_trait]
impl AgentLifecycle for LogAgent {}

#[async_trait]
impl MessageHandler for LogAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        if let Value::List(list) = message.value() {
            match list.as_slice() {
                [Value::Word(level), Value::String(msg)] => {
                    match level.as_str() {
                        "trace" => tracing::trace!("{}", msg),
                        "debug" => tracing::debug!("{}", msg),
                        "info" => tracing::info!("{}", msg),
                        "warning" => tracing::warn!("{}", msg),
                        "error" => tracing::error!("{}", msg),
                        _ => {
                            return Some(Value::RemoteError(
                                "LogAgent: unknown log level".to_string(),
                            ));
                        }
                    }
                    None
                }
                _ => {
                    Some(Value::RemoteError(
                        "LogAgent: message must be a list with a log level and message".to_string(),
                    ))
                }
            }
        } else {
            Some(Value::RemoteError(
                "LogAgent: message must be a list".to_string(),
            ))
        }
    }
}