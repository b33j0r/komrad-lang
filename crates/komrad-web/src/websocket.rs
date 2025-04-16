use crate::web_util::full;
use async_trait::async_trait;
use bytes::Bytes;
use futures::stream::{SplitSink, SplitStream};
use futures::{SinkExt, StreamExt};
use http::{Request, StatusCode};
use http_body_util::combinators::BoxBody;
use http_body_util::BodyExt;
use hyper::body;
use hyper::body::Incoming;
use hyper::upgrade::Upgraded;
use hyper_util::rt::TokioIo;
use indexmap::IndexMap;
use komrad_core::{Agent, AgentLifecycle, Channel, Message, MessageHandler, Value};
use komrad_macros::Agent;
use std::sync::Arc;
use tokio::select;
use tokio::sync::Mutex;
use tokio_tungstenite::tungstenite::handshake::server::Response;
use tokio_tungstenite::tungstenite::protocol::{Message as WsMessage, Role};
use tokio_tungstenite::WebSocketStream;
use tokio_util::sync::CancellationToken;
use tracing::{error, info};

#[derive(Agent)]
pub struct WebSocketAgent {
    name: String,
    /// The WS agent’s own channel (its identity), captured on initialization.
    channel: Option<Channel>,
    // Writer half for sending messages concurrently.
    ws_sink: Arc<Mutex<SplitSink<WebSocketStream<TokioIo<Upgraded>>, WsMessage>>>,
    // Reader half for processing incoming messages.
    ws_stream: Arc<Mutex<SplitStream<WebSocketStream<TokioIo<Upgraded>>>>>,
    // Delegate channel for forwarding WS events.
    delegate: Arc<Mutex<Option<Channel>>>,
    // Cancellation token for graceful shutdown.
    cancellation_token: CancellationToken,
}

impl WebSocketAgent {
    pub fn new(name: &str, ws_stream_full: WebSocketStream<TokioIo<Upgraded>>) -> Self {
        let (sink, stream) = ws_stream_full.split();
        Self {
            name: name.to_string(),
            channel: None,
            ws_sink: Arc::new(Mutex::new(sink)),
            ws_stream: Arc::new(Mutex::new(stream)),
            delegate: Arc::new(Mutex::new(None)),
            cancellation_token: CancellationToken::new(),
        }
    }
}

#[async_trait]
impl AgentLifecycle for WebSocketAgent {
    async fn on_init(&mut self, channel: Channel, _init_map: IndexMap<String, Value>) {
        self.channel = Some(channel);
        info!("WebSocketAgent {} initialized with channel", self.name);
    }

    async fn on_start(&mut self) {
        let cancellation_token = self.cancellation_token.clone();
        let delegate = self.delegate.clone();
        let ws_stream = self.ws_stream.clone();
        // Capture our own channel (must be set in on_init)
        let my_channel = self
            .channel
            .clone()
            .expect("Channel must be set in on_init before on_start");
        tokio::spawn(read_loop(
            cancellation_token,
            delegate,
            ws_stream,
            my_channel,
        ));
    }
}

#[async_trait]
impl MessageHandler for WebSocketAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        // Expect the message value to be a list.
        if let Value::List(list) = &message.value() {
            if let Some(Value::Word(cmd)) = list.get(0) {
                match cmd.as_str() {
                    "set-delegate" => {
                        if let Some(Value::Channel(delegate_channel)) = list.get(1) {
                            let mut delegate = self.delegate.lock().await;
                            *delegate = Some(delegate_channel.clone());
                            info!("WebSocketAgent::on_message: set delegate channel");
                            // Send "ws _socket connected" to delegate using our own channel identity.
                            let msg = Value::List(vec![
                                Value::Word("ws".into()),
                                Value::Channel(
                                    self.channel
                                        .clone()
                                        .expect("Channel must be set in on_init"),
                                ),
                                Value::Word("connected".into()),
                            ]);
                            if let Some(delegate) = delegate.as_ref() {
                                let _ = delegate.send(msg).await;
                            }
                        } else {
                            error!("WebSocketAgent::on_message: set-delegate expects a channel");
                        }
                        None
                    }
                    "send" => {
                        match list.get(1) {
                            // For plain text messages.
                            Some(Value::String(text)) => {
                                let mut sink = self.ws_sink.lock().await;
                                if let Err(e) = sink.send(WsMessage::Text(text.into())).await {
                                    error!("WebSocketAgent::on_message: send error: {:?}", e);
                                }
                                None
                            }
                            // For JSON messages – we expect a dict.
                            Some(Value::Dict(dict)) => {
                                error!("WebSocketAgent::on_message: send dict: {:?}", dict);
                                error!("not implemented yet");
                                // info!("Sending dict: {:?}", dict);
                                // match serde_json::to_string(dict) {
                                //     Ok(json_string) => {
                                //         let mut sink = self.ws_sink.lock().await;
                                //         if let Err(e) =
                                //             sink.send(WsMessage::Text(json_string.into())).await
                                //         {
                                //             error!(
                                //                 "WebSocketAgent::on_message: send error: {:?}",
                                //                 e
                                //             );
                                //         }
                                //     }
                                //     Err(e) => {
                                //         error!(
                                //             "WebSocketAgent::on_message: failed to serialize dict to JSON: {:?}",
                                //             e
                                //         );
                                //     }
                                // }
                                None
                            }
                            _ => {
                                error!("WebSocketAgent::on_message: Invalid send message format");
                                None
                            }
                        }
                    }
                    _ => {
                        error!("WebSocketAgent::on_message: Unknown command {}", cmd);
                        None
                    }
                }
            } else {
                error!("WebSocketAgent::on_message: Expected command as Word");
                None
            }
        } else {
            error!("WebSocketAgent::on_message: Invalid message format");
            None
        }
    }
}

/// The read loop now also receives a clone of the agent’s own channel so that it can include
/// it in all outgoing messages (e.g. for "message" and "disconnected").
async fn read_loop(
    cancellation_token: CancellationToken,
    delegate: Arc<Mutex<Option<Channel>>>,
    ws_stream: Arc<Mutex<SplitStream<WebSocketStream<TokioIo<Upgraded>>>>>,
    my_channel: Channel,
) {
    let mut stream = ws_stream.lock().await;
    loop {
        select! {
            Some(msg) = stream.next() => {
                match msg {
                    Ok(WsMessage::Text(text)) => {
                        if let Some(delegate) = delegate.lock().await.as_ref() {
                            // Try to parse the incoming text as JSON.
                            let outgoing = if let Ok(json_val) = serde_json::from_str::<serde_json::Value>(&text) {
                                // Build message with JSON (converted to native dict).
                                info!("A JSON message was received: {:?}", json_val);
                                Value::List(vec![
                                    Value::Word("ws".into()),
                                    Value::Channel(my_channel.clone()),
                                    Value::Word("message".into()),
                                    json_val_to_value_dict(json_val),
                                ])
                            } else {
                                info!("A string message was received: {:?}", text);
                                // Fallback: send plain text.
                                Value::List(vec![
                                    Value::Word("ws".into()),
                                    Value::Channel(my_channel.clone()),
                                    Value::Word("message".into()),
                                    Value::String(text.to_string()),
                                ])
                            };
                            let _ = delegate.send(outgoing).await;
                        }
                    },
                    Ok(WsMessage::Binary(_)) => {
                        // Optionally handle binary messages here.
                    },
                    Ok(WsMessage::Close(_)) => {
                        let msg = Value::List(vec![
                                Value::Word("ws".into()),
                                Value::Channel(my_channel.clone()),
                                Value::Word("disconnected".into()),
                            ]);
                        if let Some(delegate) = delegate.lock().await.as_ref() {
                            let _ = delegate.send(msg).await;
                        }
                        break;
                    },
                    Err(e) => {
                        error!("WebSocketAgent::read_loop: error: {:?}", e);
                        break;
                    },
                    _ => {}
                }
            }
            _ = cancellation_token.cancelled() => {
                break;
            }
        }
    }
}

/// Converts a serde_json::Value (assumed to be an object) into a native Value::Dict.
/// For non-object values, returns Value::Null.
fn json_val_to_value_dict(json_val: serde_json::Value) -> Value {
    match json_val {
        serde_json::Value::Object(map) => {
            let dict = map
                .into_iter()
                .map(|(k, v)| (k, json_val_to_value(v)))
                .collect();
            Value::Dict(dict)
        }
        _ => Value::Null,
    }
}

/// Recursively converts a serde_json::Value into a native Value.
fn json_val_to_value(json_val: serde_json::Value) -> Value {
    match json_val {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Boolean(b),
        serde_json::Value::Number(num) => {
            if let Some(i) = num.as_i64() {
                Value::Int(i)
            } else if let Some(f) = num.as_f64() {
                Value::Float(f)
            } else {
                Value::Null
            }
        }
        serde_json::Value::String(s) => Value::String(s),
        serde_json::Value::Array(arr) => {
            Value::List(arr.into_iter().map(json_val_to_value).collect())
        }
        serde_json::Value::Object(map) => {
            let dict = map
                .into_iter()
                .map(|(k, v)| (k, json_val_to_value(v)))
                .collect();
            Value::Dict(dict)
        }
    }
}

/// Computes the Sec-WebSocket-Accept header value as specified in RFC 6455.
fn compute_accept_key(key: &str) -> String {
    use base64::Engine as _;
    use base64::engine::general_purpose;
    use sha1::{Digest, Sha1};
    let mut hasher = Sha1::new();
    hasher.update(key.as_bytes());
    hasher.update(b"258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
    let result = hasher.finalize();
    general_purpose::STANDARD.encode(result)
}

/// Checks if the request contains the required headers for a WebSocket upgrade.
pub(crate) fn is_websocket_request(req: &Request<body::Incoming>) -> bool {
    if let Some(upgrade) = req.headers().get("upgrade") {
        if upgrade
            .to_str()
            .map(|s| !s.eq_ignore_ascii_case("websocket"))
            .unwrap_or(true)
        {
            return false;
        }
    } else {
        return false;
    }
    if let Some(conn) = req.headers().get("connection") {
        if let Ok(conn_str) = conn.to_str() {
            if !conn_str.to_ascii_lowercase().contains("upgrade") {
                return false;
            }
        } else {
            return false;
        }
    } else {
        return false;
    }
    req.headers().contains_key("sec-websocket-key")
        && req
        .headers()
        .get("sec-websocket-version")
        .map_or(false, |v| v == "13")
}

pub(crate) async fn handle_websocket_request(
    mut req: Request<Incoming>,
    delegate: Channel,
) -> Result<http::Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let key = req
        .headers()
        .get("sec-websocket-key")
        .and_then(|val| val.to_str().ok())
        .unwrap_or("");
    let accept_key = compute_accept_key(key);

    let upgrade_future = hyper::upgrade::on(&mut req);

    let response = Response::builder()
        .status(StatusCode::SWITCHING_PROTOCOLS)
        .header("Upgrade", "websocket")
        .header("Connection", "Upgrade")
        .header("Sec-WebSocket-Accept", accept_key)
        .body(full(""))
        .unwrap();

    tokio::spawn(async move {
        match upgrade_future.await {
            Ok(upgraded) => {
                let upgraded = TokioIo::new(upgraded);
                let ws_stream =
                    WebSocketStream::from_raw_socket(upgraded, Role::Server, None).await;
                let mut ws_agent = WebSocketAgent::new("WebSocket", ws_stream);
                let ws_channel = ws_agent.spawn();
                let ws_channel_clone = ws_channel.clone();

                let msg = Value::List(vec![
                    Value::Word("ws".into()),
                    Value::Channel(ws_channel),
                    Value::Word("connect".into()),
                ]);
                match delegate.send_and_recv(msg).await {
                    Ok(_) => {
                        info!("WebSocketAgent: Delegate notified of connection");
                    }
                    Err(e) => {
                        error!("WebSocketAgent: Failed to notify delegate: {:?}", e);
                    }
                }
            }
            Err(e) => {
                error!("WebSocket upgrade error: {:?}", e);
            }
        }
    });

    let converted_response =
        response.map(|body| BoxBody::new(body.map_err(|_| panic!("Infallible error occurred"))));
    Ok(converted_response)
}
