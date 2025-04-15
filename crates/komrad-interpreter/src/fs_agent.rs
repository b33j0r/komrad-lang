use async_trait::async_trait;
use komrad_core::{AgentLifecycle, Message, MessageHandler, Value};
use komrad_macros::Agent;
use std::env;
use tokio::fs;

#[derive(Agent)]
pub struct FsAgent;

impl Default for FsAgent {
    fn default() -> Self {
        FsAgent
    }
}

pub enum FsAgentMessage {
    WorkingDir,
    ReadFileAll(String),
    WriteFileAll(String, String),
    ReadDir(String),
}

#[async_trait]
impl AgentLifecycle for FsAgent {}

#[async_trait]
impl MessageHandler for FsAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        let fs_message = FsAgentMessage::try_from(message.clone()).ok()?;
        match fs_message {
            FsAgentMessage::WorkingDir => {
                let path = env::current_dir().unwrap_or_default();
                Some(Value::String(path.to_string_lossy().to_string()))
            }
            FsAgentMessage::ReadFileAll(path) => {
                match fs::read_to_string(path).await {
                    Ok(content) => Some(Value::String(content)),
                    Err(_) => None,
                }
            }
            FsAgentMessage::WriteFileAll(path, content) => {
                match fs::write(path, content).await {
                    Ok(_) => Some(Value::String("File written successfully".to_string())),
                    Err(_) => None,
                }
            }
            FsAgentMessage::ReadDir(path) => {
                match fs::read_dir(path).await {
                    Ok(mut entries) => {
                        let mut files = Vec::new();
                        // Use while let to fetch entries
                        while let Ok(Some(entry)) = entries.next_entry().await {
                            files.push(Value::String(
                                entry.file_name().to_string_lossy().to_string(),
                            ));
                        }
                        Some(Value::List(files))
                    }
                    Err(_) => None,
                }
            }
        }
    }
}

impl TryFrom<Message> for FsAgentMessage {
    type Error = String;

    fn try_from(message: Message) -> Result<Self, Self::Error> {
        if let Value::List(list) = message.value() {
            if let Some(Value::Word(command)) = list.get(0) {
                match command.as_ref() {
                    "working-dir" => Ok(FsAgentMessage::WorkingDir),
                    "read-file-all" => {
                        if let Some(Value::String(path)) = list.get(1) {
                            Ok(FsAgentMessage::ReadFileAll(path.clone()))
                        } else {
                            Err("Invalid path".to_string())
                        }
                    }
                    "write-file-all" => {
                        if let Some(Value::String(path)) = list.get(1) {
                            if let Some(Value::String(content)) = list.get(2) {
                                Ok(FsAgentMessage::WriteFileAll(path.clone(), content.clone()))
                            } else {
                                Err("Invalid content".to_string())
                            }
                        } else {
                            Err("Invalid path".to_string())
                        }
                    }
                    "read-dir" => {
                        if let Some(Value::String(path)) = list.get(1) {
                            Ok(FsAgentMessage::ReadDir(path.clone()))
                        } else {
                            Err("Invalid path".to_string())
                        }
                    }
                    _ => Err("Unknown command".to_string()),
                }
            } else {
                Err("Invalid command".to_string())
            }
        } else {
            Err("Invalid message format".to_string())
        }
    }
}
