use async_trait::async_trait;
use bytes::Bytes;
use http::{Request, Response, StatusCode};
use http_body_util::combinators::BoxBody;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper_util::rt::TokioIo;
use indexmap::IndexMap;
use komrad_core::{
    Agent, AgentFactory, AgentLifecycle, Channel, Env, Message, MessageHandler, Value,
};
use komrad_macros::Agent;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::select;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use tokio_util::sync::CancellationToken;
use tracing::{debug, error, info, trace, warn};

use crate::http_request::HttpRequest;
use crate::http_response::HttpResponse;
use crate::web_util::{empty, full};
use crate::websocket;

#[derive(Clone)]
pub struct HttpListenerConfig {
    pub port: u16,
    pub host: String,
    pub delegate: Channel,
}

pub struct HttpListenerFactory;

#[async_trait]
impl AgentFactory for HttpListenerFactory {
    async fn create_agent(&self, _env: Env) -> Box<dyn Agent> {
        Box::new(HttpListener::new())
    }
}

#[derive(Agent)]
pub struct HttpListener {
    config: Option<HttpListenerConfig>,
    server_handle: Mutex<Option<JoinHandle<()>>>,
    shutdown_token: CancellationToken,
}

impl HttpListener {
    pub fn new() -> Self {
        Self {
            config: None,
            server_handle: Mutex::new(None),
            shutdown_token: CancellationToken::new(),
        }
    }

    async fn start_server(
        config: HttpListenerConfig,
        shutdown_token: CancellationToken,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let addr_str = format!("{}:{}", config.host, config.port);
        let addr: SocketAddr = addr_str.parse()?;
        let listener = TcpListener::bind(addr).await?;
        trace!("HttpListener: listening on http://{}", addr);

        let delegate = config.delegate.clone();
        loop {
            select! {
                accept_res = listener.accept() => {
                    match accept_res {
                        Ok((tcp_stream, _)) => {
                            let io = TokioIo::new(tcp_stream);
                            let delegate_clone = delegate.clone();
                            tokio::spawn(async move {
                                // handle each connection
                                let result = http1::Builder::new()
                                    .serve_connection(
                                        io,
                                        service_fn(move |req| {
                                            Self::handle_incoming_request(req, delegate_clone.clone())
                                        }),
                                    )
                                    .with_upgrades()
                                    .await;

                                if let Err(e) = result {
                                    error!("Connection error: {:?}", e);
                                }
                            });
                        },
                        Err(e) => {
                            error!("Failed to accept: {:?}", e);
                        }
                    }
                },
                _ = shutdown_token.cancelled() => {
                    warn!("HttpListener shutting down gracefully...");
                    break;
                }
            }
        }

        Ok(())
    }

    async fn handle_incoming_request(
        req: Request<hyper::body::Incoming>,
        delegate: Channel,
    ) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
        if websocket::is_websocket_request(&req) {
            return websocket::handle_websocket_request(req, delegate).await;
        }
        Self::process_http_request(req, delegate).await
    }

    async fn process_http_request(
        req: Request<hyper::body::Incoming>,
        delegate: Channel,
    ) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
        // 1) ephemeral request agent
        let req_agent = HttpRequest::new("Request", req).await;
        let method_str = req_agent.method().to_string();
        let path_vals: Vec<Value> = req_agent.path().into_iter().map(Value::String).collect();
        let req_chan = req_agent.spawn();

        // 2) ephemeral response agent
        let (final_tx, final_rx) = Channel::new();
        let resp_agent = HttpResponse::new(Some(final_tx));
        let resp_chan = resp_agent.spawn();

        // 3) Compose message, e.g. ["http", req_chan, resp_chan, "GET", "some", "path"]
        let mut terms = vec![
            Value::Word("http".into()),
            Value::Channel(req_chan),
            Value::Channel(resp_chan),
            Value::Word(method_str.into()),
        ];
        terms.extend(path_vals);

        // 4) Send to delegate
        if let Err(e) = delegate.send(Value::List(terms)).await {
            error!("Delegate send error: {:?}", e);
            let resp = Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body(full("Delegate error"))
                .unwrap();
            return Ok(resp);
        }

        // 5) Wait for final from response agent
        let final_msg = match final_rx.recv().await {
            Ok(m) => m,
            Err(e) => {
                error!("No final message: {:?}", e);
                let resp = Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(full("No final message"))
                    .unwrap();
                return Ok(resp);
            }
        };

        // 6) Convert final message -> hyper response
        //    We expect final_msg.value() to be a List of [ status, headers, cookies, body ]
        let val = final_msg.value();
        let mut resp = Self::build_hyper_response_from_value(val);
        Ok(resp)
    }

    /// *FIXED* Now we pass a single Value. We expect `Value::List(...)`.
    fn build_hyper_response_from_value(val: &Value) -> Response<BoxBody<Bytes, hyper::Error>> {
        let lst = match val {
            Value::List(lst) => lst,
            _ => {
                return Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(full("Final response was not a list"))
                    .unwrap();
            }
        };
        if lst.len() < 4 {
            return Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body(full("Invalid final format"))
                .unwrap();
        }

        // status
        let status = match &lst[0] {
            Value::Int(i) => *i as u16,
            _ => 200,
        };

        // parse headers
        let headers_vec = match &lst[1] {
            Value::List(items) => items
                .iter()
                .filter_map(|pair| {
                    if let Value::List(p) = pair {
                        if p.len() == 2 {
                            if let (Value::String(k), Value::String(v)) = (&p[0], &p[1]) {
                                return Some((k.clone(), v.clone()));
                            }
                        }
                    }
                    None
                })
                .collect::<Vec<_>>(),
            _ => vec![],
        };

        // parse cookies
        let cookies_vec = match &lst[2] {
            Value::List(items) => items
                .iter()
                .filter_map(|pair| {
                    if let Value::List(p) = pair {
                        if p.len() == 2 {
                            if let (Value::String(k), Value::String(v)) = (&p[0], &p[1]) {
                                return Some((k.clone(), v.clone()));
                            }
                        }
                    }
                    None
                })
                .collect::<Vec<_>>(),
            _ => vec![],
        };

        // parse body
        let body_bytes = match &lst[3] {
            Value::Bytes(arcb) => (*arcb).clone(),
            _ => Arc::new(Bytes::from_static(b"")),
        };

        // Build
        let mut builder = Response::builder().status(status);
        for (k, v) in headers_vec {
            builder = builder.header(k, v);
        }
        for (ck, cv) in cookies_vec {
            builder = builder.header("Set-Cookie", format!("{}={}", ck, cv));
        }

        let cloned_body_bytes = body_bytes.clone();
        builder
            .body(full(cloned_body_bytes.as_ref().clone()))
            .unwrap_or_else(|_| {
                Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(full("Build error"))
                    .unwrap()
            })
    }
}

#[async_trait]
impl MessageHandler for HttpListener {
    async fn on_message(&mut self, _msg: &Message) -> Option<Value> {
        None
    }
}

#[async_trait]
impl AgentLifecycle for HttpListener {
    async fn on_init(&mut self, _channel: Channel, init_map: IndexMap<String, Value>) {
        trace!("HttpListener on_init: {:?}", init_map);
        let mut cport = 8080u16;
        let mut chost = "0.0.0.0".to_string();
        let mut cdelegate = None;

        for (k, v) in init_map {
            match k.as_str() {
                "port" => {
                    if let Value::Int(i) = v {
                        cport = i as u16;
                    }
                }
                "host" => {
                    if let Value::String(h) = v {
                        chost = h;
                    }
                }
                "delegate" => {
                    if let Value::Channel(ch) = v {
                        cdelegate = Some(ch);
                    }
                }
                _ => {}
            }
        }
        if let Some(del) = cdelegate {
            self.config = Some(HttpListenerConfig {
                port: cport,
                host: chost,
                delegate: del,
            });
        } else {
            error!("No delegate in init_map");
        }
    }
    async fn on_start(&mut self) {
        if let Some(conf) = &self.config {
            let conf2 = conf.clone();
            let shut = self.shutdown_token.clone();
            let fut = async move {
                if let Err(e) = Self::start_server(conf2, shut).await {
                    error!("HttpListener error: {:?}", e);
                }
            };
            let handle = tokio::spawn(fut);
            *self.server_handle.lock().await = Some(handle);
        } else {
            error!("No config set for HttpListener");
        }
    }

    async fn on_stop(&mut self) {
        self.shutdown_token.cancel();
        if let Some(h) = self.server_handle.lock().await.take() {
            let _ = h.await;
        }
    }
}
