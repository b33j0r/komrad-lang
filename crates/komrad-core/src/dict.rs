use crate::{Env, Evaluate, InternalMessageHandler, Message, MessageHandler, Value};
use async_trait::async_trait;
use indexmap::IndexMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value as JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    pub index_map: IndexMap<String, Value>,
}

#[async_trait]
impl InternalMessageHandler for Dict {
    async fn on_internal_message(&mut self, env: &mut Env, message: &Message) -> Option<Value> {
        // We expect the message to be a Value::List of terms, e.g. [ Word("len") ]
        let terms_val = message.value();

        // Only handle Value::List for the terms
        let Some(Value::List(terms)) = terms_val.clone().into() else {
            return None; // not a List => no match
        };

        match &self.index_map {
            //----------------------
            // 1) LIST
            //----------------------
            _ => {
                match terms.as_slice() {
                    // e.g. [ Word("len") ]
                    [Value::Word(w)] if w.as_str() == "len" => Some(Value::Int(self.index_map.len() as i64)),
                    // e.g. [ Word("foreach"), Word("x"), Block(b) ]
                    [Value::Word(w), Value::Word(var), Value::Block(b)] if w.as_str() == "foreach" => {
                        // iter keys as var, value not included
                        for item in self.index_map.keys() {
                            let mut env = env.clone_handler_scope().await;
                            env.set(var, Value::String(item.clone())).await;
                            b.evaluate(&mut env).await;
                        }
                        Some(Value::Null)
                    }
                    _ => None,
                }
            }
        }
    }
}

impl From<IndexMap<String, Value>> for Dict {
    fn from(index_map: IndexMap<String, Value>) -> Self {
        Dict { index_map }
    }
}

impl FromIterator<(String, Value)> for Dict {
    fn from_iter<T: IntoIterator<Item=(String, Value)>>(iter: T) -> Self {
        Dict {
            index_map: IndexMap::from_iter(iter),
        }
    }
}

impl From<Dict> for IndexMap<String, Value> {
    fn from(dict: Dict) -> Self {
        dict.index_map
    }
}

impl From<&Dict> for IndexMap<String, Value> {
    fn from(dict: &Dict) -> Self {
        dict.index_map.clone()
    }
}

impl AsRef<IndexMap<String, Value>> for Dict {
    fn as_ref(&self) -> &IndexMap<String, Value> {
        &self.index_map
    }
}


impl Dict {
    pub fn new() -> Self {
        Dict {
            index_map: IndexMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: Value) {
        self.index_map.insert(key, value);
    }

    pub fn extend(&mut self, other: Dict) {
        for (k, v) in other.index_map {
            self.index_map.insert(k, v);
        }
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.index_map.get(key)
    }

    pub fn remove(&mut self, key: &str) -> Option<Value> {
        self.index_map.remove(key)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.index_map.contains_key(key)
    }

    pub fn keys(&self) -> Vec<&String> {
        self.index_map.keys().collect()
    }

    pub fn values(&self) -> Vec<&Value> {
        self.index_map.values().collect()
    }

    pub fn iter(&self) -> impl Iterator<Item=(&String, &Value)> {
        self.index_map.iter()
    }

    /// Produces (Value, Value) instead of (String, Value).
    /// This is for use by komrad code, whereas internally we generally use (String, Value).
    pub fn iter_external(&self) -> impl Iterator<Item=(Value, Value)> {
        self.index_map.iter().map(|(k, v)| (Value::String(k.clone()), v.clone()))
    }

    pub fn len(&self) -> usize {
        self.index_map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.index_map.is_empty()
    }

    pub fn clear(&mut self) {
        self.index_map.clear();
    }

    pub fn to_vec(&self) -> Vec<(String, Value)> {
        self.index_map.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }
}

impl Serialize for Dict {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // turn our Dict into a serde_json::Value::Object...
        let mut map = serde_json::Map::new();
        for (k, v) in &self.index_map {
            map.insert(k.clone(), JsonValue::from(v));
        }
        JsonValue::Object(map).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Dict {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // first deserialize into serde_json::Value
        let jv = JsonValue::deserialize(deserializer)?;
        match jv {
            JsonValue::Object(map) => {
                let mut d = Dict::new();
                for (k, v) in map {
                    let val = Value::try_from(v)
                        .map_err(serde::de::Error::custom)?;
                    d.insert(k, val);
                }
                Ok(d)
            }
            _ => Err(serde::de::Error::custom("expected JSON object")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Value;

    #[test]
    fn test_dict() {
        let mut dict = Dict::new();
        dict.insert("key1".to_string(), Value::Int(1));
        dict.insert("key2".to_string(), Value::String("value2".to_string()));

        assert_eq!(dict.get("key1"), Some(&Value::Int(1)));
        assert_eq!(dict.get("key2"), Some(&Value::String("value2".to_string())));
        assert_eq!(dict.len(), 2);
        assert!(!dict.is_empty());
    }

    #[test]
    fn test_dict_iter() {
        let mut dict = Dict::new();
        dict.insert("key1".to_string(), Value::Int(1));
        dict.insert("key2".to_string(), Value::String("value2".to_string()));

        let keys: Vec<_> = dict.keys();
        assert_eq!(keys, vec![&"key1".to_string(), &"key2".to_string()]);

        let values: Vec<_> = dict.values();
        assert_eq!(values, vec![&Value::Int(1), &Value::String("value2".to_string())]);
    }

    #[test]
    fn test_dict_serialize() {
        let mut dict = Dict::new();
        dict.insert("key1".to_string(), Value::Int(1));
        dict.insert("key2".to_string(), Value::String("value2".to_string()));

        let serialized = serde_json::to_string(&dict).unwrap();
        assert_eq!(serialized, r#"{"key1":1,"key2":"value2"}"#);
    }

    #[test]
    fn test_dict_deserialize() {
        let json = r#"{"key1":1,"key2":"value2"}"#;
        let dict: Dict = serde_json::from_str(json).unwrap();
        assert_eq!(dict.get("key1"), Some(&Value::Int(1)));
        assert_eq!(dict.get("key2"), Some(&Value::String("value2".to_string())));
    }
}
