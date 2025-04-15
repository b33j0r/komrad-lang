use async_trait::async_trait;
use komrad_core::{AgentLifecycle, Channel, Destructure, DestructureResult, Env, Evaluate, EvaluationContext, Message, MessageHandler, PatternDestructure, Value};
use komrad_macros::Agent;
use tracing::error;

#[derive(Agent)]
pub struct DynamicAgent {
    me: Channel,
    env: Env,
}

#[async_trait]
impl AgentLifecycle for DynamicAgent {
    async fn on_init(&mut self, channel: Channel, initializer_map: indexmap::IndexMap<String, Value>) {
        self.me = channel;
        // Set initial bindings into the environment.
        for (key, value) in initializer_map {
            self.env.set(&key, value).await;
        }
    }
}

#[async_trait]
impl MessageHandler for DynamicAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        // Iterate over registered handlers in the current environment.
        for handler in self.env.handlers().await.iter() {
            // Note: handler.pattern is a Spanned<Pattern> so we extract its inner value.
            let result = PatternDestructure::destructure(&handler.pattern.value, message.value());
            match result {
                DestructureResult::Match(bindings) => {
                    // Instead of directly using self.env, create a new handler scope.
                    // This ensures that changes made within the handler block follow the
                    // "handler scope" semantics (e.g., updating parent's variables if needed).
                    let mut handler_env = self.env.clone_handler_scope().await;

                    // Inject all variable bindings from the pattern match into the new environment.
                    for (key, value) in bindings {
                        handler_env.set(&key, value).await;
                    }

                    // Create an evaluation context using the new environment.
                    let mut eval_context = EvaluationContext { env: handler_env };

                    // Evaluate the handler's block (its expression) and return the result.
                    let reply = handler.expr.evaluate(&mut eval_context).await;
                    return Some(reply);
                }
                DestructureResult::NoMatch => continue,
                DestructureResult::Err(e) => {
                    error!("Pattern matching error: {:?}", e);
                    continue;
                }
            }
        }
        Some(Value::RemoteError(format!(
            "No handler found for message: {:?}",
            message.value()
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use komrad_core::{Expr, Handler, Pattern, Span, Spanned};
    use std::sync::Arc;
    use tokio;

    /// Test that a handler registered with a literal ValueMatch pattern
    /// correctly matches a message and returns the expected reply.
    #[tokio::test]
    async fn test_handler_match() {
        // Create an environment and register one handler.
        let mut env = Env::default();
        // Create a handler whose pattern expects Value::Int(42).
        let pattern = Spanned::new(
            Span::default(),
            Pattern::ValueMatch(Spanned::new(Span::default(), Value::Int(42))),
        );
        // Its expression returns a literal Value::String("Matched").
        let expr = Spanned::new(
            Span::default(),
            Expr::Value(Spanned::new(Span::default(), Value::String("Matched".to_string()))),
        );
        let handler = Arc::new(Handler { pattern, expr });
        env.push_handler(handler).await;

        // Initialize the dynamic agent with the environment.
        let (chan, _) = Channel::new();
        let mut agent = DynamicAgent {
            me: chan.clone(),
            env,
        };
        agent.on_init(chan.clone(), IndexMap::new()).await;

        // Create a message with Value::Int(42) so the handler should match.
        let msg = Message::new(Value::Int(42), None);
        let reply = agent.on_message(&msg).await;
        match reply {
            Some(Value::String(result)) => assert_eq!(result, "Matched"),
            _ => panic!("Expected handler to match and return 'Matched'"),
        }
    }

    /// Test that when no handler matches the incoming message,
    /// the dynamic agent returns a RemoteError indicating no match.
    #[tokio::test]
    async fn test_no_handler_match() {
        // Create an environment with no handlers.
        let env = Env::default();
        let (chan, _) = Channel::new();
        let mut agent = DynamicAgent {
            me: chan.clone(),
            env,
        };
        agent.on_init(chan.clone(), IndexMap::new()).await;

        // Create a message with an arbitrary value that will not match.
        let msg = Message::new(Value::Int(100), None);
        let reply = agent.on_message(&msg).await;
        match reply {
            Some(Value::RemoteError(err_msg)) => {
                assert!(
                    err_msg.contains("No handler found for message"),
                    "Error message did not contain expected text"
                );
            }
            _ => panic!("Expected a RemoteError when no handler matches"),
        }
    }

    /// Test that a handler using a VariableCapture pattern correctly injects
    /// the captured value into the evaluation context. The handler’s expression
    /// performs a lookup of the captured variable "x" and returns its value.
    #[tokio::test]
    async fn test_handler_with_variable_capture() {
        let mut env = Env::default();
        // Create a handler with a variable capture pattern for "x".
        let pattern = Spanned::new(
            Span::default(),
            Pattern::VariableCapture(Spanned::new(Span::default(), "x".to_string())),
        );
        // The expression is a variable lookup: it returns the value of "x" by
        // using Expr::Value with Value::Word("x").
        let expr = Spanned::new(
            Span::default(),
            Expr::Value(Spanned::new(Span::default(), Value::Word("x".to_string()))),
        );
        let handler = Arc::new(Handler { pattern, expr });
        env.push_handler(handler).await;

        let (chan, _) = Channel::new();
        let mut agent = DynamicAgent {
            me: chan.clone(),
            env,
        };
        agent.on_init(chan.clone(), IndexMap::new()).await;

        // Send a message with an arbitrary value (e.g. Value::Int(99))
        // that should be captured in "x".
        let test_value = Value::Int(99);
        let msg = Message::new(test_value.clone(), None);
        let reply = agent.on_message(&msg).await;
        // The evaluated expression will look up "x" in the environment and return its value.
        match reply {
            Some(ref v) if v == &test_value => {}
            _ => panic!("Expected handler to return the captured value"),
        }
    }

    /// Test that when multiple handlers are registered, the first matching handler
    /// is chosen. We register two handlers with distinct patterns.
    #[tokio::test]
    async fn test_multiple_handlers() {
        let mut env = Env::default();
        // First handler: matches Value::Int(1) and returns "First".
        let pattern1 = Spanned::new(
            Span::default(),
            Pattern::ValueMatch(Spanned::new(Span::default(), Value::Int(1))),
        );
        let expr1 = Spanned::new(
            Span::default(),
            Expr::Value(Spanned::new(Span::default(), Value::String("First".to_string()))),
        );
        let handler1 = Arc::new(Handler {
            pattern: pattern1,
            expr: expr1,
        });
        env.push_handler(handler1).await;

        // Second handler: matches Value::Int(2) and returns "Second".
        let pattern2 = Spanned::new(
            Span::default(),
            Pattern::ValueMatch(Spanned::new(Span::default(), Value::Int(2))),
        );
        let expr2 = Spanned::new(
            Span::default(),
            Expr::Value(Spanned::new(Span::default(), Value::String("Second".to_string()))),
        );
        let handler2 = Arc::new(Handler {
            pattern: pattern2,
            expr: expr2,
        });
        env.push_handler(handler2).await;

        let (chan, _) = Channel::new();
        let mut agent = DynamicAgent {
            me: chan.clone(),
            env,
        };
        agent.on_init(chan.clone(), IndexMap::new()).await;

        // Send a message with Value::Int(2): this should skip the first handler
        // and match the second handler.
        let msg = Message::new(Value::Int(2), None);
        let reply = agent.on_message(&msg).await;
        match reply {
            Some(Value::String(result)) => assert_eq!(result, "Second"),
            _ => panic!("Expected the second handler to match and return 'Second'"),
        }
    }
}
