use async_trait::async_trait;
use bytes::Bytes;
use http::{HeaderMap, Request};
use http_body_util::BodyExt;
use indexmap::IndexMap;
use komrad_core::{Agent, AgentLifecycle, Channel, Message, MessageHandler, Value};
use komrad_macros::Agent;
use std::collections::HashMap;
use std::sync::Arc;

/// Holds the data from a single HTTP request.
#[derive(Debug)]
pub struct RequestData {
    pub url: String,
    pub method: String,
    pub body: Arc<Bytes>,
    pub headers: HeaderMap,
    pub params: HashMap<String, String>,
    pub cookies: HashMap<String, String>,
}

#[derive(Agent, Debug)]
pub struct HttpRequest {
    name: String,
    data: Arc<RequestData>,
}

impl HttpRequest {
    pub async fn new(agent_name: &str, req: Request<hyper::body::Incoming>) -> Self {
        let method = req.method().to_string();
        let url = req.uri().to_string();
        let headers = req.headers().clone();
        let params = Self::parse_query_params(req.uri().query());
        let body_bytes = req.collect().await.unwrap_or_default().to_bytes();
        let cookies = Self::parse_cookies(&headers);

        let data = RequestData {
            url,
            method,
            headers,
            params,
            cookies,
            // store body as Arc<Bytes>
            body: Arc::new(body_bytes),
        };

        Self {
            name: agent_name.to_string(),
            data: Arc::new(data),
        }
    }

    pub fn method(&self) -> &str {
        &self.data.method
    }

    pub fn path(&self) -> Vec<String> {
        self.data
            .url
            .split('/')
            .filter_map(|seg| {
                let seg = seg.trim();
                if seg.is_empty() {
                    None
                } else {
                    Some(seg.to_string())
                }
            })
            .collect()
    }

    fn parse_query_params(q: Option<&str>) -> HashMap<String, String> {
        let mut m = HashMap::new();
        if let Some(query) = q {
            for pair in query.split('&') {
                let mut it = pair.splitn(2, '=');
                if let (Some(k), Some(v)) = (it.next(), it.next()) {
                    m.insert(k.to_string(), v.to_string());
                }
            }
        }
        m
    }

    fn parse_cookies(hdrs: &HeaderMap) -> HashMap<String, String> {
        let mut m = HashMap::new();
        if let Some(c) = hdrs.get("cookie") {
            if let Ok(s) = c.to_str() {
                for chunk in s.split(';') {
                    let chunk = chunk.trim();
                    if let Some(eqpos) = chunk.find('=') {
                        let ck = &chunk[..eqpos];
                        let cv = &chunk[eqpos + 1..];
                        m.insert(ck.to_string(), cv.to_string());
                    }
                }
            }
        }
        m
    }

    /// Logic to retrieve properties from the stored request data.
    /// e.g. "method", "url", "body", "headers", "params", "cookie"
    fn get(&self, key: &str, sub: Option<&str>) -> Value {
        match key {
            "url" => Value::String(self.data.url.clone()),
            "method" => Value::String(self.data.method.clone()),
            "body" => Value::Bytes(self.data.body.clone()),
            "headers" => {
                if let Some(hdr) = sub {
                    if let Some(hv) = self.data.headers.get(hdr) {
                        Value::String(hv.to_str().unwrap_or("").to_string())
                    } else {
                        Value::String("".to_string())
                    }
                } else {
                    let lst = self
                        .data
                        .headers
                        .iter()
                        .map(|(k, v)| {
                            Value::List(vec![
                                Value::String(k.to_string()),
                                Value::String(v.to_str().unwrap_or("").to_string()),
                            ])
                        })
                        .collect::<Vec<_>>();
                    Value::List(lst)
                }
            }
            "params" => {
                if let Some(param_name) = sub {
                    if let Some(val) = self.data.params.get(param_name) {
                        Value::String(val.clone())
                    } else {
                        Value::String("".to_string())
                    }
                } else {
                    let lst = self
                        .data
                        .params
                        .iter()
                        .map(|(k, v)| {
                            Value::List(vec![Value::String(k.clone()), Value::String(v.clone())])
                        })
                        .collect();
                    Value::List(lst)
                }
            }
            "cookie" => {
                if let Some(cn) = sub {
                    if let Some(cv) = self.data.cookies.get(cn) {
                        Value::String(cv.clone())
                    } else {
                        Value::String("".to_string())
                    }
                } else {
                    let lst = self
                        .data
                        .cookies
                        .iter()
                        .map(|(k, v)| {
                            Value::List(vec![Value::String(k.clone()), Value::String(v.clone())])
                        })
                        .collect();
                    Value::List(lst)
                }
            }
            _ => Value::Null,
        }
    }
}

#[async_trait]
impl MessageHandler for HttpRequest {
    async fn on_message(&mut self, msg: &Message) -> Option<Value> {
        // We'll pattern-match on a top-level Value::List.
        let val = msg.value();
        let terms = match val {
            Value::List(terms) => terms,
            _ => {
                // Not the shape we expect, ignore
                return None;
            }
        };

        // We expect something like [ "get", <property>, <maybe subkey>?, <maybe reply_chan>? ]
        if terms.is_empty() {
            return None;
        }

        // The first item should be a command word
        let cmd = match &terms[0] {
            Value::Word(w) => w.as_str(),
            Value::String(s) => s.as_str(),
            _ => return None, // not recognized
        };

        match cmd {
            "get" => {
                // We can adapt to up to 4 terms: "get", property, (subkey?), (reply_chan?)
                let (prop, subkey) = match terms.len() {
                    2 => {
                        // [ "get", <prop> ]
                        let prop_s = extract_wordlike(&terms[1]);
                        (prop_s, None)
                    }
                    3 => {
                        // Could be [ "get", <prop>, <subkey-or-reply-chan> ]
                        let prop_s = extract_wordlike(&terms[1]);
                        match &terms[2] {
                            Value::Channel(_) => (prop_s, None),
                            _ => (prop_s, Some(extract_wordlike(&terms[2]))),
                        }
                    }
                    4 | _ => {
                        // [ "get", <prop>, <subkey>, <reply_chan> ]
                        // If there's more than 4, we only read first 4 for simplicity
                        let prop_s = extract_wordlike(&terms[1]);
                        let subkey_s = extract_wordlike(&terms[2]);
                        (prop_s, Some(subkey_s))
                    }
                };

                let result = self.get(prop, subkey);

                // If the last term is a channel, we send the result there
                // TODO: I don't remember why this was a thing
                if let Some(Value::Channel(reply_chan)) = terms.last() {
                    let _ = reply_chan.send(Value::List(vec![result.clone()])).await;
                }
                Some(result)
            }
            _ => None,
        }
    }
}

#[async_trait]
impl AgentLifecycle for HttpRequest {
    async fn on_init(&mut self, _chan: Channel, _init_map: IndexMap<String, Value>) {
        // no-op
    }
}

/// Helper to handle a single 'wordlike' argument that might be Value::Word(w)
/// or Value::String(s).  Returns &str so we can do the property or subkey.
fn extract_wordlike(v: &Value) -> &str {
    match v {
        Value::Word(w) => w.as_str(),
        Value::String(s) => s.as_str(),
        _ => "",
    }
}
