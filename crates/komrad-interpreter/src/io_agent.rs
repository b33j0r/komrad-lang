use async_trait::async_trait;
use komrad_core::{Agent, AgentLifecycle, Message, MessageHandler, Value};
use komrad_macros::Agent;
use std::convert::TryFrom;
use std::sync::Arc;

//
// IoInterface: allows swapping out the I/O implementation.
// (This is unchanged from your current io_agent.rs file.)
//
pub trait IoInterface {
    fn print(&self, message: &str);
    fn println(&self, message: &str);
}

pub struct ConsoleIo;
impl IoInterface for ConsoleIo {
    fn print(&self, message: &str) {
        print!("{}", message);
    }
    fn println(&self, message: &str) {
        println!("{}", message);
    }
}

/// IoAgent uses an IoInterface to respond to print commands.
#[derive(Agent)]
pub struct IoAgent {
    // The underlying I/O implementation. It can be swapped out.
    io_interface: Arc<dyn IoInterface + Send + Sync>,
}

impl IoAgent {
    pub fn new(io_interface: Arc<dyn IoInterface + Send + Sync>) -> Self {
        IoAgent { io_interface }
    }
}

impl Default for IoAgent {
    fn default() -> Self {
        IoAgent {
            io_interface: Arc::new(ConsoleIo),
        }
    }
}

/// These are the possible messages that IoAgent understands.
pub enum IoAgentMessage {
    Print(String),
    Println(String),
}

/// Convert an incoming Message to an IoAgentMessage.
/// This follows the same approach as fs_agent by expecting a Value::List.
/// The first element should be a Word (or String) command, and subsequent elements
/// its arguments.
impl TryFrom<Message> for IoAgentMessage {
    type Error = String;

    fn try_from(message: Message) -> Result<Self, Self::Error> {
        // Expect the message value to be a list.
        if let Value::List(list) = message.value() {
            if list.is_empty() {
                return Err("Empty message list".to_string());
            }
            // The first element is the command.
            // Here we accept either a Word or a String for flexibility.
            let command = match &list[0] {
                Value::Word(s) => s.clone(),
                Value::String(s) => s.clone(),
                _ => return Err("First element must be a command string".to_string()),
            };
            // Match the command and extract argument(s).
            match command.as_str() {
                "print" => {
                    if let Some(Value::String(arg)) = list.get(1) {
                        Ok(IoAgentMessage::Print(arg.clone()))
                    } else {
                        Err("print command requires a string argument".to_string())
                    }
                }
                "println" => {
                    if let Some(Value::String(arg)) = list.get(1) {
                        Ok(IoAgentMessage::Println(arg.clone()))
                    } else {
                        Err("println command requires a string argument".to_string())
                    }
                }
                _ => Err(format!("Unknown IoAgent command: {}", command)),
            }
        } else {
            Err("Message must be a list".to_string())
        }
    }
}

#[async_trait]
impl MessageHandler for IoAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        // Convert the incoming message into an IoAgentMessage.
        let io_msg = match IoAgentMessage::try_from(message.clone()) {
            Ok(m) => m,
            Err(e) => return Some(Value::RemoteError(e)),
        };
        // Process the command.
        match io_msg {
            IoAgentMessage::Print(text) => {
                self.io_interface.print(&text);
                // Return a confirmation; you could also return Null.
                Some(Value::String("Printed".to_string()))
            }
            IoAgentMessage::Println(text) => {
                self.io_interface.println(&text);
                Some(Value::String("Printed with newline".to_string()))
            }
        }
    }
}

#[async_trait]
impl AgentLifecycle for IoAgent {}
