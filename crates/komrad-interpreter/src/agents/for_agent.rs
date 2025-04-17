// src/agents/for_agent.rs

use async_trait::async_trait;
use komrad_core::{Agent, AgentLifecycle, Destructure, Message, MessageHandler, Type, Value};
use komrad_core::{
    CommandDestructure,
    CommandSignature,
    DestructureResult,
};
use komrad_macros::Agent;
use std::sync::Arc;

#[derive(Agent)]
pub struct ForAgent;

impl Default for ForAgent {
    fn default() -> Self {
        ForAgent
    }
}

#[async_trait]
impl AgentLifecycle for ForAgent {}

#[async_trait]
impl MessageHandler for ForAgent {
    async fn on_message(&mut self, message: &Message) -> Option<Value> {
        // We support two forms:
        //
        //   for <var> in <list> { <block> }
        //   for <var> in <dict> { <block> }
        //
        // Both are dispatched via CommandDestructure.
        //
        //   ["for", var_name:String, "in":String, iterable, block]
        //

        // 1) Signature expecting a List
        let sig_list = CommandSignature {
            name: "for".into(),
            args: vec![
                Type::Word, // var name
                Type::Word, // the literal "in"
                Type::List,   // iterable
                Type::Block,  // block body
            ],
        };

        // 2) Signature expecting a Dict
        let sig_dict = CommandSignature {
            name: "for".into(),
            args: vec![
                Type::Word,
                Type::Word,
                Type::Dict,   // iterable
                Type::Block,
            ],
        };

        // 3) Try the list form first
        let (cmd, iterable_value) = match CommandDestructure::destructure(&sig_list, message.value()) {
            DestructureResult::Match(cmd) => {
                let args = cmd.args.clone();
                // we got a Value::List
                (cmd, Iterable::List(args[2].clone()))
            }
            DestructureResult::NoMatch => {
                // not a list, try the dict form
                match CommandDestructure::destructure(&sig_dict, message.value()) {
                    DestructureResult::Match(cmd) => {
                        let args = cmd.args.clone();
                        (cmd, Iterable::DictKeys(args[2].clone()))
                    }
                    DestructureResult::NoMatch => {
                        // not a for‐list or for‐dict → ignore
                        return None;
                    }
                    DestructureResult::Err(e) => {
                        return Some(Value::RemoteError(format!("{:?}", e)));
                    }
                }
            }
            DestructureResult::Err(e) => {
                return Some(Value::RemoteError(format!("{:?}", e)));
            }
        };

        // 4) Pull out the loop variable name & block
        let var_name = if let Value::String(s) = &cmd.args[0] {
            s.clone()
        } else {
            return Some(Value::RemoteError(
                "for: loop variable must be a string".into()
            ));
        };

        if cmd.args[1] != Value::String("in".into()) {
            return Some(Value::RemoteError("for: missing `in` keyword".into()));
        }

        let block = if let Value::Block(b) = &cmd.args[3] {
            Arc::clone(b)
        } else {
            return Some(Value::RemoteError("for: missing block body".into()));
        };

        // 5) Iterate!
        match iterable_value {
            Iterable::List(list_val) => {
                if let Value::List(elems) = list_val {
                    for item in elems {
                        // TODO: bind `var_name = item` and evaluate `block` here,
                    }
                }
            }
            Iterable::DictKeys(dict_val) => {
                if let Value::Dict(d) = dict_val {
                    for key in d.keys() {
                        let item = Value::String(key.clone());
                        // TODO: bind `var_name = item` and evaluate `block` here
                    }
                }
            }
        }

        // 6) Signal we handled it (returning Null)
        Some(Value::Null)
    }
}

/// Helper to distinguish our two iterable forms
enum Iterable {
    List(Value),
    DictKeys(Value),
}
