use crate::ast::RuntimeError;
use crate::value::Value;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

const CHANNEL_CAPACITY_MSG: usize = 64;
const CHANNEL_CAPACITY_CTL: usize = 8;

#[derive(Debug, Clone)]
pub struct Message {
    pub uuid: uuid::Uuid,
    pub value: Value,
    pub reply_to: Option<Channel>,
}

impl Default for Message {
    fn default() -> Self {
        Message {
            uuid: uuid::Uuid::now_v7(),
            value: Value::Null,
            reply_to: None,
        }
    }
}

impl Message {
    pub fn new(value: Value, reply_to: Option<Channel>) -> Self {
        Message {
            uuid: uuid::Uuid::now_v7(),
            value,
            reply_to,
        }
    }

    pub fn with_terms(mut self, terms: Vec<Value>) -> Self {
        if !terms.is_empty() {
            self.value = Value::List(terms);
        } else {
            self.value = Value::Null;
        }
        self
    }

    pub fn with_reply_to(mut self, reply_to: Channel) -> Self {
        self.reply_to = Some(reply_to);
        self
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn reply_to(&self) -> Option<&Channel> {
        self.reply_to.as_ref()
    }
}

pub enum ControlMessage {
    Stop,
}

#[derive(Debug, Clone)]
pub struct Channel {
    uuid: uuid::Uuid,
    sender_msg: mpsc::Sender<Message>,
    sender_ctl: mpsc::Sender<ControlMessage>,
}

impl PartialEq for Channel {
    fn eq(&self, other: &Self) -> bool {
        self.uuid == other.uuid
    }
}

pub struct ChannelListener {
    uuid: uuid::Uuid,
    receiver_msg: Arc<Mutex<mpsc::Receiver<Message>>>,
    receiver_ctl: Arc<Mutex<mpsc::Receiver<ControlMessage>>>,
}

impl Channel {
    pub fn uuid(&self) -> uuid::Uuid {
        self.uuid
    }

    pub fn new() -> (Self, Arc<ChannelListener>) {
        let (sender_msg, receiver_msg) = mpsc::channel(CHANNEL_CAPACITY_MSG);
        let (sender_ctl, receiver_ctl) = mpsc::channel(CHANNEL_CAPACITY_CTL);
        let channel = Channel {
            uuid: uuid::Uuid::now_v7(),
            sender_msg,
            sender_ctl,
        };
        let listener = ChannelListener {
            uuid: channel.uuid,
            receiver_msg: Arc::new(Mutex::new(receiver_msg)),
            receiver_ctl: Arc::new(Mutex::new(receiver_ctl)),
        };
        (channel, Arc::new(listener))
    }

    pub async fn stop(&self) {
        let _ = self.sender_ctl.send(ControlMessage::Stop).await;
    }

    pub async fn send(&self, value: Value) -> Result<(), RuntimeError> {
        let msg = Message {
            uuid: uuid::Uuid::now_v7(),
            value,
            reply_to: None,
        };
        match self.sender_msg.send(msg).await {
            Ok(_) => Ok(()),
            Err(_) => Err(RuntimeError::ChannelError("Channel closed".to_string())),
        }
    }

    pub async fn send_and_recv(&self, value: Value) -> Result<Value, RuntimeError> {
        let (reply_channel, listener) = Channel::new();
        let msg = Message {
            uuid: uuid::Uuid::now_v7(),
            value,
            reply_to: Some(reply_channel.clone()),
        };
        let _ = self.sender_msg.send(msg).await;

        match listener.recv().await {
            Ok(msg) => Ok(msg.value),
            Err(e) => Err(e),
        }
    }
}

impl ChannelListener {
    pub fn uuid(&self) -> uuid::Uuid {
        self.uuid
    }

    /// Because this uses a mutex, it should only be used from a single async task.
    pub async fn recv(&self) -> Result<Message, RuntimeError> {
        let mut receiver = self.receiver_msg.lock().await;
        match receiver.recv().await {
            Some(msg) => Ok(msg),
            None => Err(RuntimeError::ChannelError("Channel closed".to_string())),
        }
    }

    /// Because this uses a mutex, it should only be used from a single async task.
    pub async fn recv_ctl(&self) -> Result<ControlMessage, RuntimeError> {
        let mut receiver = self.receiver_ctl.lock().await;
        match receiver.recv().await {
            Some(msg) => Ok(msg),
            None => Err(RuntimeError::ChannelError(
                "Control channel closed".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[tokio::test]
    async fn test_channel_send_recv() {
        let (channel, listener) = Channel::new();
        let value = Value::String("Hello, World!".to_string());
        match channel.send(value.clone()).await {
            Ok(_) => assert!(true),
            Err(_) => panic!("Expected to send the message"),
        }
        let listener = listener.clone();
        match listener.recv().await {
            Ok(msg) => assert_eq!(msg.value, value),
            Err(_) => panic!("Expected to receive the message"),
        }
    }

    #[tokio::test]
    async fn test_channel_send_and_recv() {
        // set up two pairs of channels
        let (channel1, listener1) = Channel::new();

        let value = Value::String("Hello from channel 1!".to_string());

        tokio::spawn(async move {
            let listener = listener1.clone();
            match listener.recv().await {
                Ok(msg) => {
                    if let Some(reply_to) = msg.reply_to {
                        reply_to
                            .send(Value::String("Reply from channel 2!".to_string()))
                            .await
                            .unwrap();
                    }
                }
                Err(_) => panic!("Expected to receive the message"),
            }
        });

        match channel1.send_and_recv(value.clone()).await {
            Ok(response) => {
                assert_eq!(response, Value::String("Reply from channel 2!".to_string()))
            }
            Err(_) => panic!("Expected to receive the response"),
        }
    }

    #[tokio::test]
    async fn test_channel_stop() {
        let (channel, listener) = Channel::new();
        channel.stop().await;
        match listener.recv_ctl().await {
            Ok(ControlMessage::Stop) => assert!(true),
            _ => panic!("Expected to receive stop control message"),
        }
    }
}
