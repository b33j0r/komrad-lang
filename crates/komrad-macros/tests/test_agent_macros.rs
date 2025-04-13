use async_trait::async_trait;
use komrad_core::{Agent, AgentLifecycle, Channel, Message, MessageHandler, Value};
use komrad_macros::{Agent, AgentMessage};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Agent)]
pub struct TestAgent {
    pub requests: Arc<RwLock<Vec<Value>>>,
}

#[async_trait]
impl AgentLifecycle for TestAgent {}

impl TestAgent {
    pub fn new() -> (Self, Arc<RwLock<Vec<Value>>>) {
        let requests = Arc::new(RwLock::new(Vec::new()));
        let agent = TestAgent {
            requests: requests.clone(),
        };
        (agent, requests)
    }
}

#[async_trait]
impl MessageHandler for TestAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        let mut requests = self.requests.write().await;
        requests.push(message.value.clone());
        println!("Received message: {:?}", message.value);
        Some(Value::String("Response".to_string()))
    }
}

#[tokio::test]
async fn test_agent_spawn() {
    let (agent, requests) = TestAgent::new();
    let agent_channel = agent.spawn();

    agent_channel
        .send_and_recv(Value::String("Hello".to_string()))
        .await
        .unwrap();
    let requests = requests.write().await;
    assert_eq!(requests.len(), 1);
}

#[derive(AgentMessage)]
pub struct TestAgentMessage {
    #[exact_string = "send"]
    command: String,
    target: Channel,
}

#[tokio::test]
async fn test_agent_message() {
    let (agent, requests) = TestAgent::new();
    let agent_channel = agent.spawn();

    let msg = Message::default()
        .with_terms(vec![
            Value::Word("send".into()),
            Value::Channel(agent_channel.clone()),
        ])
        .with_reply_to(agent_channel.clone());

    if let msg = TestAgentMessage::try_from(&msg) {
        agent_channel
            .send_and_recv(Value::String("Hello".to_string()))
            .await
            .unwrap();
    }
}

#[tokio::test]
async fn test_agent_enum_message() {
    let (agent, _requests) = TestAgent::new();
    let agent_channel = agent.spawn();

    // Test Send variant
    let send_msg = Message::default()
        .with_terms(vec![
            Value::Word("send".into()),
            Value::Channel(agent_channel.clone()),
        ])
        .with_reply_to(agent_channel.clone());

    // Test Echo variant
    let echo_msg = Message::default()
        .with_terms(vec![
            Value::Word("echo".into()),
            Value::String("Hello World".into()),
        ])
        .with_reply_to(agent_channel.clone());

    // Test parsing into different enum variants
    if let Ok(TestEnumMessage::Send(chan)) = TestEnumMessage::try_from(&send_msg) {
        assert_eq!(chan, agent_channel);
    } else {
        panic!("Failed to parse Send message");
    }

    if let Ok(TestEnumMessage::Echo(text)) = TestEnumMessage::try_from(&echo_msg) {
        assert_eq!(text, "Hello World");
    } else {
        panic!("Failed to parse Echo message");
    }
}

#[derive(Debug, Clone, PartialEq, AgentMessage)]
pub enum TestEnumMessage {
    #[exact_string(send)]
    Send(Channel),

    #[exact_string(echo)]
    Echo(String),
}
