use async_trait::async_trait;
use indexmap::IndexMap;
use komrad_core::{Agent, AgentLifecycle, Channel, Message, MessageHandler, RuntimeError, Value};
use komrad_macros::Agent;
use tracing::{error, info, warn};

use bytes::Bytes;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Agent, Debug, Default)]
pub struct HttpResponse {
    reply_to: Option<Channel>,
    status: i64, // Using Int variant from Value
    headers: HashMap<String, String>,
    cookies: Vec<(String, String)>,
    body: Vec<u8>,
    finished: bool,
}

impl HttpResponse {
    pub fn new(reply_to: Option<Channel>) -> Self {
        Self {
            reply_to,
            ..Default::default()
        }
    }

    fn finalize(&mut self) -> Option<Value> {
        if self.finished {
            warn!("Already finalized");
            return None;
        }
        self.finished = true;

        // Instead of Value::Number(...), we do Value::Int(...)
        let status_val = Value::Int(self.status);
        let headers_val = Value::List(
            self.headers
                .iter()
                .map(|(k, v)| Value::List(vec![Value::String(k.clone()), Value::String(v.clone())]))
                .collect(),
        );
        let cookies_val = Value::List(
            self.cookies
                .iter()
                .map(|(k, v)| Value::List(vec![Value::String(k.clone()), Value::String(v.clone())]))
                .collect(),
        );

        // Must wrap body in Arc for Value::Bytes
        let body_val = Value::Bytes(Arc::new(Bytes::copy_from_slice(&self.body)));

        Some(Value::List(vec![status_val, headers_val, cookies_val, body_val]))
    }
}

#[async_trait]
impl MessageHandler for HttpResponse {
    async fn on_message(&mut self, msg: &Message) -> Option<Value> {
        if self.finished {
            warn!("Message after finalize: {:?}", msg);
            return None;
        }

        if let Value::List(terms) = &msg.value() {
            if let Value::Word(cmd) = &terms[0] {
                let args = terms.get(1..).unwrap_or(&[]);

                // finalizer?
                if let Some(ret) = self.handle_finalizer(cmd, args).await {
                    return Some(ret);
                }

                // else additive
                self.handle_additive(cmd, args);
                Some(Value::String("OK".to_string()))
            } else {
                Some(Value::RemoteError(
                    "First term must be a command".to_string(),
                ))
            }
        } else {
            Some(Value::RemoteError("Message must be a list".to_string()))
        }
    }
}

impl HttpResponse {
    fn handle_additive(&mut self, cmd: &str, args: &[Value]) {
        match cmd {
            "set-status" => {
                if let Some(Value::Int(i)) = args.get(0) {
                    self.status = *i;
                }
            }
            "set-header" => {
                if let (Some(Value::String(k)), Some(Value::String(v))) = (args.get(0), args.get(1))
                {
                    self.headers.insert(k.clone(), v.clone());
                }
            }
            "set-cookie" => {
                if let (Some(Value::String(k)), Some(Value::String(v))) = (args.get(0), args.get(1))
                {
                    self.cookies.push((k.clone(), v.clone()));
                }
            }
            "write" => {
                if let Some(Value::String(s)) = args.get(0) {
                    self.body.extend_from_slice(s.as_bytes());
                }
            }
            _ => {}
        }
    }

    async fn handle_finalizer(&mut self, cmd: &str, args: &[Value]) -> Option<Value> {
        match cmd {
            "html" => {
                if let Some(Value::String(body_str)) = args.get(0) {
                    self.body.clear();
                    self.body.extend_from_slice(body_str.as_bytes());
                    self.headers
                        .insert("Content-Type".into(), "text/html".into());
                    self.status = 200;
                }
                // Finalize and store it
                let final_msg = self.finalize();
                let chan = self.reply_to.clone();
                if let Some(msg) = final_msg {
                    if let Some(ch) = chan {
                        let _ = ch.send(msg).await;
                    }
                }
                Some(Value::Boolean(true))
            }
            "file" => {
                warn!("Handling file command: {:?}", args);
                if let Some(Value::String(path)) = args.get(0) {
                    // Read the file content asynchronously
                    match tokio::fs::read(path).await {
                        Ok(bytes) => {
                            self.body.clear();
                            self.body.extend_from_slice(&bytes);
                            let mime = mime_guess::from_path(path).first_or_octet_stream();
                            self.headers.insert("Content-Type".into(), mime.to_string());
                            self.status = 200;
                        }
                        Err(err) => {
                            warn!("Failed to read file: {}", err);
                            self.status = 500; // Internal Server Error
                        }
                    }
                }
                // Finalize and store it
                let final_msg = self.finalize();
                let chan = self.reply_to.clone();
                if let Some(msg) = final_msg {
                    if let Some(ch) = chan {
                        let _ = ch.send(msg).await;
                    }
                }
                Some(Value::Boolean(true))
            }
            "binary" => {
                if let Some(Value::Bytes(bytes)) = args.get(0) {
                    self.body.clear();
                    self.body.extend_from_slice(&bytes);
                    self.headers
                        .insert("Content-Type".into(), "application/octet-stream".into());
                    self.status = 200;
                }
                // Finalize and store it
                let final_msg = self.finalize();
                let chan = self.reply_to.clone();
                if let Some(msg) = final_msg {
                    if let Some(ch) = chan {
                        let _ = ch.send(msg).await;
                    }
                }
                Some(Value::Boolean(true))
            }
            "json" => {
                if let Some(Value::Dict(dict)) = args.get(0) {
                    match serde_json::to_vec(dict) {
                        Ok(json_bytes) => {
                            self.body.clear();
                            self.body.extend_from_slice(&json_bytes);
                            self.headers
                                .insert("Content-Type".into(), "application/json".into());
                            self.status = 200;
                        }
                        Err(err) => {
                            warn!("Failed to serialize JSON: {}", err);
                            self.status = 500;
                        }
                    }
                } else {
                    self.status = 400;
                }

                let final_msg = self.finalize();
                let chan = self.reply_to.clone();
                if let Some(msg) = final_msg {
                    if let Some(ch) = chan {
                        let _ = ch.send(msg).await;
                    }
                }
                Some(Value::Boolean(true))
            }
            "redirect" => {
                if let Some(Value::String(location)) = args.get(0) {
                    self.status = 303; // 303 See Other (safe for POST->GET)
                    self.headers.insert("Location".into(), location.clone());
                    self.headers
                        .insert("Content-Type".into(), "text/plain".into());
                    self.body.clear();
                    self.body
                        .extend_from_slice(format!("Redirecting to {}", location).as_bytes());
                } else {
                    self.status = 400;
                    self.body.clear();
                    self.body
                        .extend_from_slice(b"Missing location for redirect");
                }

                let final_msg = self.finalize();
                let chan = self.reply_to.clone();
                if let Some(msg) = final_msg {
                    if let Some(ch) = chan {
                        let _ = ch.send(msg).await;
                    }
                }
                Some(Value::Boolean(true))
            }

            "template" => {
                if let (Some(Value::String(template_path)), Some(Value::Dict(dict_ctx))) =
                    (args.get(0), args.get(1))
                {
                    // 1) Read the template file
                    match tokio::fs::read_to_string(template_path).await {
                        Ok(template_content) => {
                            // 2) Convert the `Value::Dict` into something Tera can handle:
                            let mut tera_ctx = tera::Context::new();

                            // If you have a real method to convert Value -> JSON, you can do it:
                            for (k, v) in dict_ctx.iter() {
                                let json_val = serde_json::to_value(v).unwrap();
                                tera_ctx.insert(k, &json_val);
                            }

                            // 3) Render the template
                            let result = tera::Tera::one_off(&template_content, &tera_ctx, true);
                            match result {
                                Ok(rendered) => {
                                    self.body.clear();
                                    self.body.extend_from_slice(rendered.as_bytes());
                                    self.headers
                                        .insert("Content-Type".into(), "text/html".into());
                                    self.status = 200;
                                }
                                Err(err) => {
                                    warn!("Failed to render Tera template: {}", err);
                                    self.status = 500;
                                }
                            }
                        }
                        Err(err) => {
                            warn!("Failed to read template file {}: {}", template_path, err);
                            self.status = 500;
                        }
                    }
                } else {
                    warn!("Expected template path as string and context as dict");
                    self.status = 400; // Bad Request
                }
                // 4) Finalize
                let final_msg = self.finalize();
                let chan = self.reply_to.clone();
                if let Some(msg) = final_msg {
                    if let Some(ch) = chan {
                        let _ = ch.send(msg).await;
                    }
                }
                Some(Value::Boolean(false))
            }
            _ => None,
        }
    }
}

#[async_trait]
impl AgentLifecycle for HttpResponse {}
