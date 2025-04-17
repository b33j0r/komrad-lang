use crate::{Env, Evaluate, InternalMessageHandler, Value};
use async_trait::async_trait;

#[async_trait]
impl InternalMessageHandler for Vec<Value> {
    async fn on_internal_message(&mut self, env: &mut Env, message: &crate::Message) -> Option<Value> {
        // We expect the message to be a Value::List of terms, e.g. [ Word("len") ]
        let terms_val = message.value();

        // Only handle Value::List for the terms
        let Some(Value::List(terms)) = terms_val.clone().into() else {
            return None; // not a List => no match
        };

        match &self[..] {
            //----------------------
            // 1) LIST
            //----------------------
            _ => {
                match terms.as_slice() {
                    // e.g. [ Word("len") ]
                    [Value::Word(w)] if w.as_str() == "len" => Some(Value::Int(self.len() as i64)),
                    // e.g. [ Word("foreach"), Word("x"), Block(b) ]
                    [Value::Word(w), Value::Word(var), Value::Block(b)] if w.as_str() == "foreach" => {
                        for item in self.iter() {
                            let mut env = env.clone_handler_scope().await;
                            env.set(var, item.clone()).await;
                            b.evaluate(&mut env).await;
                        }
                        Some(Value::Null)
                    }
                    // [retain var block]
                    [Value::Word(w), Value::Word(var), Value::Block(b)]
                    if w == "retain" => {
                        let mut new_list = Vec::with_capacity(self.len());
                        for item in self.iter() {
                            let mut env = env.clone_handler_scope().await;
                            env.set(var, item.clone()).await;
                            let result = b.evaluate(&mut env).await;
                            match result {
                                Value::Boolean(true) => new_list.push(item.clone()),
                                Value::Boolean(false) => {}
                                other => {
                                    return Some(Value::RemoteError(format!(
                                        "retain block must return boolean, got: {:?}",
                                        other
                                    )));
                                }
                            }
                        }
                        Some(Value::List(new_list))
                    }
                    // [map var block]
                    [Value::Word(w), Value::Word(var), Value::Block(b)]
                    if w == "map" => {
                        let mut new_list = Vec::with_capacity(self.len());
                        for item in self.iter() {
                            let mut env = env.clone_handler_scope().await;
                            env.set(var, item.clone()).await;
                            let result = b.evaluate(&mut env).await;
                            match result {
                                Value::List(list) => new_list.extend(list),
                                other => {
                                    return Some(Value::RemoteError(format!(
                                        "map block must return list, got: {:?}",
                                        other
                                    )));
                                }
                            }
                        }
                        Some(Value::List(new_list))
                    }
                    _ => None,
                }
            }
        }
    }
}