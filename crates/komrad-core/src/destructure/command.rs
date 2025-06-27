use crate::error::RuntimeError;
use crate::value::Value;
use crate::Type;
use crate::destructure::base::{DestructureResult, Destructure};

/// Command destructuring for built-in agents.
pub struct CommandDestructure;
pub struct CommandSignature {
    pub name: String,
    pub args: Vec<Type>,
}

#[derive(Debug, PartialEq)]
pub struct Command {
    pub name: String,
    pub args: Vec<Value>,
}

impl Destructure for CommandDestructure {
    type Target = CommandSignature;
    type Output = Command;

    fn destructure(
        target: &Self::Target,
        input: &Self::Input,
    ) -> DestructureResult<Self::Output, Self::Error> {
        // Only List‐messages are valid commands:
        if let Value::List(vals) = input {
            if vals.is_empty() {
                return DestructureResult::NoMatch;
            }

            // 1) Accept Word or String for the command name:
            let cmd_name = match &vals[0] {
                Value::String(s) => s.clone(),
                Value::Word(s) => s.clone(),
                _ => return DestructureResult::NoMatch,
            };
            if cmd_name != target.name {
                return DestructureResult::NoMatch;
            }

            // 2) Zip up values + expected types:
            let args_result = vals[1..]
                .iter()
                .zip(&target.args)
                .map(|(val, ty)| {
                    match (val, ty) {
                        // numeric cases unchanged:
                        (Value::Int(_), Type::Int) |
                        (Value::Float(_), Type::Float) => Ok(val.clone()),

                        // now accept both String *and* Word for Type::String:
                        (Value::String(_), Type::String) |
                        (Value::Word(_), Type::String) => Ok(val.clone()),

                        // everything else is a type‐mismatch:
                        _ => Err(RuntimeError::TypeError(
                            format!("Expected {:?}, got {:?}", ty, val)
                        )),
                    }
                })
                .collect::<Result<Vec<_>, _>>();

            match args_result {
                Ok(args) => DestructureResult::Match(Command {
                    name: target.name.clone(),
                    args,
                }),
                Err(e) => DestructureResult::Err(e),
            }
        } else {
            DestructureResult::NoMatch
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn test_command_destructure() {
        let signature = CommandSignature {
            name: "test".to_string(),
            args: vec![Type::Int, Type::String],
        };
        let input = Value::List(vec![
            Value::String("test".to_string()),
            Value::Int(42),
            Value::String("hello".to_string()),
        ]);
        let result = CommandDestructure::destructure(&signature, &input);
        let expected = DestructureResult::Match(Command {
            name: signature.name.clone(),
            args: vec![Value::Int(42), Value::String("hello".to_string())],
        });
        assert_eq!(result, expected);
    }
}