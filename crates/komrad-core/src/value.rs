use crate::dict::Dict;
use crate::error::RuntimeError;
use crate::{Block, Channel, Spanned};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{self, Value as JsonValue};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Value {
    /// Represents a null value.
    Null,
    Error(Spanned<RuntimeError>),
    /// Represents a channel for sending messages (assume external definition).
    Channel(Channel),
    /// Represents a list of values.
    List(Vec<Value>),
    /// Represents a dict of values.
    Dict(Dict),
    /// Represents a symbol or identifier.
    Word(String),
    /// Represents a boolean value.
    Boolean(bool),
    /// Represents a string value.
    String(String),
    /// An integer value.
    Int(i64),
    /// Represents a floating-point number.
    Float(f64),
    /// A UUID value. New values are time-based v7 UUIDs.
    Uuid(uuid::Uuid),
    /// Represents a sequence of statements, often used as a closure or handler body.
    Block(Arc<Block>),
    /// An error that can't have a Span.
    RemoteError(String),
    /// A byte array.
    Bytes(Arc<bytes::Bytes>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            // null
            (Null, Null) => true,

            // errors (compare both span and error kind)
            (Error(e1), Error(e2)) => e1 == e2,
            (RemoteError(s1), RemoteError(s2)) => s1 == s2,

            // channels (compare channel identity)
            (Channel(c1), Channel(c2)) => {
                c1.uuid() == c2.uuid()
            }

            // lists, dicts, words, booleans, strings, ints
            (List(l1), List(l2)) => l1 == l2,
            (Dict(d1), Dict(d2)) => d1 == d2,
            (Word(w1), Word(w2)) => w1 == w2,
            (Boolean(b1), Boolean(b2)) => b1 == b2,
            (String(s1), String(s2)) => s1 == s2,
            (Int(i1), Int(i2)) => i1 == i2,

            // floats (exact IEEE‑754 equality)
            (Float(f1), Float(f2)) => f1 == f2,

            // UUIDs
            (Uuid(u1), Uuid(u2)) => u1 == u2,

            // blocks (Arc<Block> compares the Block contents)
            (Block(b1), Block(b2)) => b1 == b2,

            // bytes (Arc<Bytes> compares the byte sequences)
            (Bytes(b1), Bytes(b2)) => b1 == b2,

            // anything else (different variants)
            _ => false,
        }
    }
}


impl Value {
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Value::Error(_)) || matches!(self, Value::RemoteError(_))
    }

    pub fn is_word(&self) -> bool {
        matches!(self, Value::Word(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Value::List(_))
    }

    pub fn is_dict(&self) -> bool {
        matches!(self, Value::Dict(_))
    }
}

impl From<&Value> for JsonValue {
    fn from(v: &Value) -> JsonValue {
        match v {
            Value::Null => JsonValue::Null,
            Value::Boolean(b) => JsonValue::Bool(*b),
            Value::Int(i) => JsonValue::Number((*i).into()),
            Value::Float(f) => JsonValue::Number(
                serde_json::Number::from_f64(*f)
                    .unwrap_or_else(|| serde_json::Number::from(0))
            ),
            Value::String(s) => JsonValue::String(s.clone()),
            Value::List(vec) => {
                JsonValue::Array(vec.iter().map(JsonValue::from).collect())
            }
            Value::Dict(d) => {
                let mut map = serde_json::Map::new();
                for (k, v) in d.iter() {
                    map.insert(k.clone(), JsonValue::from(v));
                }
                JsonValue::Object(map)
            }
            // for anything else we fall back to null (or you can error)
            _ => JsonValue::Null,
        }
    }
}

impl TryFrom<JsonValue> for Value {
    type Error = RuntimeError;

    fn try_from(j: JsonValue) -> Result<Self, RuntimeError> {
        Ok(match j {
            JsonValue::Null => Value::Null,
            JsonValue::Bool(b) => Value::Boolean(b),
            JsonValue::Number(n) => {
                if n.is_i64() {
                    Value::Int(n.as_i64().unwrap())
                } else {
                    Value::Float(n.as_f64().unwrap())
                }
            }
            JsonValue::String(s) => Value::String(s),
            JsonValue::Array(arr) => {
                let mut out = Vec::with_capacity(arr.len());
                for el in arr {
                    out.push(Value::try_from(el)?);
                }
                Value::List(out)
            }
            JsonValue::Object(map) => {
                let mut d = Dict::new();
                for (k, v) in map {
                    d.insert(k, Value::try_from(v)?);
                }
                Value::Dict(d)
            }
        })
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Convert our Value into a serde_json::Value, then serialize that.
        let jv = JsonValue::from(self);
        jv.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // First parse into serde_json::Value...
        let jv = JsonValue::deserialize(deserializer)?;
        // ...then convert that into our Value, mapping errors cleanly.
        Value::try_from(jv)
            .map_err(serde::de::Error::custom)
    }
}

impl From<Channel> for Value {
    fn from(c: Channel) -> Self {
        Value::Channel(c)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_string())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl TryFrom<Value> for bool {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(b),
            _ => Err(RuntimeError::TypeError("Expected a boolean".to_string())),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i),
            _ => Err(RuntimeError::TypeError("Expected an integer".to_string())),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Float(f) => Ok(f),
            _ => Err(RuntimeError::TypeError("Expected a float".to_string())),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err(RuntimeError::TypeError("Expected a string".to_string())),
        }
    }
}

impl TryFrom<Value> for Channel {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Channel(c) => Ok(c),
            _ => Err(RuntimeError::TypeError("Expected a channel".to_string())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Null,
    Int,
    Float,
    Word,
    String,
    Boolean,
    List,
    Dict,
    Channel,
    Block,
    Bytes,
    Uuid,
}
