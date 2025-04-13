use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};
use crate::expr::{RuntimeError, Value};

const CHANNEL_CAPACITY_MSG: usize = 64;
const CHANNEL_CAPACITY_CTL: usize = 8;

#[derive(Debug, Clone)]
pub struct Message {
    pub uuid: uuid::Uuid,
    pub value: Value,
    pub reply_to: Option<Channel>,
}

enum ControlMessage {
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

    pub async fn send(&self, value: Value) {
        let msg = Message {
            uuid: uuid::Uuid::now_v7(),
            value,
            reply_to: None,
        };
        let _ = self.sender_msg.send(msg).await;
    }

    pub async fn send_and_recv(&self, value: Value) -> Value {
        let (reply_channel, mut listener) = Channel::new();
        let msg = Message {
            uuid: uuid::Uuid::now_v7(),
            value,
            reply_to: Some(reply_channel.clone()),
        };
        let _ = self.sender_msg.send(msg).await;

        match listener.recv().await {
            Message { value, .. } => value,
        }
    }
}

impl ChannelListener {
    pub fn uuid(&self) -> uuid::Uuid {
        self.uuid
    }

    /// Because this uses a mutex, it should only be used from a single async task.
    pub async fn recv(&self) -> Message {
        let mut receiver = self.receiver_msg.lock().await;
        match receiver.recv().await {
            Some(msg) => msg,
            None => {
                panic!("ChannelListener: No message received");
            }
        }
    }

    /// Because this uses a mutex, it should only be used from a single async task.
    pub async fn recv_ctl(&self) -> Option<ControlMessage> {
        self.receiver_ctl.lock().await.recv().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::Value;

    #[tokio::test]
    async fn test_channel_send_recv() {
        let (channel, listener) = Channel::new();
        let value = Value::String("Hello, World!".to_string());
        channel.send(value.clone()).await;
        let mut listener = listener.clone();
        match listener.recv().await.value {
            Value::String(msg) => assert_eq!(msg, "Hello, World!".to_string()),
            _ => panic!("Expected a string message"),
        }
    }

    #[tokio::test]
    async fn test_channel_send_and_recv() {
        // set up two pairs of channels
        let (channel1, listener1) = Channel::new();
        let (channel2, listener2) = Channel::new();

        let value = Value::String("Hello from channel 1!".to_string());

        tokio::spawn(async move {
            let mut listener = listener1.clone();
            match listener.recv().await {
                Message { value, reply_to: Some(reply_channel), .. } => {
                    reply_channel.send(Value::String("Reply from channel 2!".to_string())).await;
                }
                Message { uuid: _, value: _, reply_to: None } => {
                    panic!("Expected a reply channel");
                }
            }
        });

        match channel1.send_and_recv(value.clone()).await {
            Value::String(msg) => assert_eq!(msg, "Reply from channel 2!"),
            _ => panic!("Expected a string reply"),
        }
    }
}