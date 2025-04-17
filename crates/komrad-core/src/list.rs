use crate::{MessageHandler, Value};
use async_trait::async_trait;

#[async_trait]
impl MessageHandler for Vec<Value> {
    async fn on_message(&mut self, message: &crate::Message) -> Option<Value> {
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
                    _ => None,
                }
            }
        }
    }
}