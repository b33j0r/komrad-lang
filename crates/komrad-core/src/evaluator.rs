use crate::ast::{AssignmentTarget, Expr, Operator, Spanned, Statement};
// Reuse the destructuring module for assignments.
use crate::destructure::{AssignmentAction, AssignmentDestructure, Destructure, DestructureResult};
use crate::dict::Dict;
use crate::env::Env;
use crate::error::RuntimeError;
use crate::value::Value;
use crate::{AsSpanned, Block, Channel, CommandDestructure, Message, MessageHandler, ToSExpr};
use async_trait::async_trait;
use indexmap::IndexMap;
use std::future::Future;
use std::pin::Pin;
use tracing::{debug, trace};

#[async_trait]
pub trait InternalMessageHandler {
    async fn on_internal_message(&mut self, env: &mut Env, message: &Message) -> Option<Value>;
}

/// A trait specifying how to evaluate an AST node into a `Value`.
#[async_trait]
pub trait Evaluate {
    type Input;
    type Output = Value;

    /// Evaluates the given AST node into a `Value`.
    async fn evaluate(&self, env: &mut Env) -> Value;
}

#[async_trait]
impl Evaluate for Block {
    type Input = Self;
    type Output = Value;

    async fn evaluate(&self, env: &mut Env) -> Value {
        let mut result = Value::Null;
        for stmt in &self.0 {
            result = stmt.evaluate(env).await;
            if result.is_error() {
                trace!("Error in block: {:?}", result);
                break;
            }
        }
        result
    }
}

#[async_trait]
impl Evaluate for Spanned<Expr> {
    type Input = Self;
    type Output = Value;

    async fn evaluate(&self, env: &mut Env) -> Value {
        match self.value.as_ref() {
            // 1) Literal/variable expression.
            Expr::Value(sp_val) => {
                match sp_val.value.as_ref() {
                    // If it's a variable ("Word"), look it up.
                    Value::Word(name) => match env.get(name).await {
                        Some(v) => v,
                        None => Value::Word(name.clone()),
                    },
                    // Otherwise, propagate the literal.
                    other => other.clone(),
                }
            }
            // 2) List expression.
            Expr::List { elements } => {
                let mut evaluated = Vec::with_capacity(elements.len());
                for elem in elements {
                    evaluated.push(elem.evaluate(env).await);
                }
                Value::List(evaluated)
            }
            // 3) Dictionary expression.
            Expr::Dict { index_map } => {
                let mut evaluated = Dict::new();
                for (key, val_expr) in index_map {
                    evaluated.insert(key.clone(), val_expr.evaluate(env).await);
                }
                Value::Dict(evaluated)
            }
            // 4) Binary expression.
            Expr::BinaryExpr { lhs, op, rhs } => {
                let left = lhs.evaluate(env).await;
                let right = rhs.evaluate(env).await;
                match op.value.as_ref() {
                    Operator::Equal => {
                        Value::Boolean(left == right)
                    }
                    Operator::NotEqual => {
                        Value::Boolean(left != right)
                    }
                    Operator::Add => match (left, right) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
                        (Value::String(a), Value::String(b)) => Value::String(a + &b),

                        // String + Int
                        (Value::String(a), Value::Int(n)) => {
                            format!("{a}{n}").into()
                        }
                        (Value::Int(n), Value::String(a)) => {
                            format!("{n}{a}").into()
                        }

                        // String + Float
                        (Value::String(a), Value::Float(n)) => {
                            format!("{a}{n}").into()
                        }
                        (Value::Float(n), Value::String(a)) => {
                            format!("{n}{a}").into()
                        }

                        // List + List
                        (Value::List(mut a), Value::List(b)) => {
                            // make a new list with the elements of both lists
                            let mut new_list = a.clone();
                            new_list.extend(b);
                            Value::List(new_list)
                        }

                        // Dict + Dict
                        (Value::Dict(mut a), Value::Dict(b)) => {
                            // make a new dict with the elements of both dicts
                            let mut new_dict = a.clone();
                            new_dict.extend(b);
                            Value::Dict(new_dict)
                        }

                        // Catch-all
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in addition".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Subtract => match (left, right) {
                        // Int - Int
                        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a - b),

                        // Int - Float
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 - b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f64),

                        // String - String (remove the first occurrence of b from a)
                        (Value::String(a), Value::String(b)) => Value::String(a.replacen(&b, "", 1)),

                        // List - Value (remove the first occurrence of b from a)
                        (Value::List(mut a), b) => {
                            if let Some(pos) = a.iter().position(|x| x == &b) {
                                a.remove(pos);
                            }
                            Value::List(a)
                        }

                        // Catch-all
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in subtraction".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Multiply => match (left, right) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
                        (Value::String(a), Value::Int(b)) => {
                            if b < 0 {
                                return Value::Error(
                                    RuntimeError::ArgumentError("Negative repeat count".to_string())
                                        .as_spanned(rhs.span.clone()),
                                );
                            }
                            Value::String(a.repeat(b as usize))
                        }

                        // List<String> * String (join a with b)
                        (Value::List(a), Value::String(b)) => {
                            let mut result = String::new();
                            for item in a {
                                if let Value::String(s) = item {
                                    result.push_str(&s);
                                    result.push_str(&b);
                                } else {
                                    return Value::Error(
                                        RuntimeError::ArgumentError(
                                            "Type mismatch in list multiplication".to_string(),
                                        )
                                            .as_spanned(rhs.span.clone()),
                                    );
                                }
                            }
                            Value::String(result)
                        }

                        _ => Value::Error(
                            RuntimeError::ArgumentError(
                                "Type mismatch in multiplication".to_string(),
                            )
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Divide => match (left, right) {
                        (Value::Int(_), Value::Int(0)) => Value::Error(
                            RuntimeError::ArgumentError("Divide by zero".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                        (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 / b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a / b as f64),
                        // for strings, split the string by the divisor
                        (Value::String(a), Value::String(b)) => {
                            if b.is_empty() {
                                return Value::Error(
                                    RuntimeError::ArgumentError("Divide by zero".to_string())
                                        .as_spanned(rhs.span.clone()),
                                );
                            }
                            Value::List(a.split(&b).map(|s| Value::String(s.to_string())).collect())
                        }
                        // For lists, remove all occurrences of the divisor from the list
                        (Value::List(mut a), b) => {
                            a.retain(|x| x != &b);
                            Value::List(a)
                        }
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in division".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Mod => match (left, right) {
                        (Value::Int(_), Value::Int(0)) => Value::Error(
                            RuntimeError::ArgumentError("Divide by zero".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                        (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 % b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a % b as f64),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in modulus".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    _ => Value::Error(
                        RuntimeError::UnknownError("Operator not implemented".to_string())
                            .as_spanned(op.span.clone()),
                    ),
                }
            }
            // Ask expression – send_and_recv.
            Expr::Ask { target, value } => {
                // TODO: merge with the `tell` statement
                let targ = target.evaluate(env).await;
                let val = value.evaluate(env).await;
                match targ {
                    Value::Channel(ch) => match ch.send_and_recv(val).await {
                        Ok(reply) => reply,
                        Err(e) => Value::Error(e.as_spanned(value.span.clone())),
                    },
                    Value::Dict(mut dict) => {
                        let msg = Message::new(val, None);
                        match dict.on_internal_message(env, &msg).await {
                            Some(reply) => {
                                reply
                            }
                            None => {
                                Value::Error(
                                    RuntimeError::ArgumentError("No handler found".to_string())
                                        .as_spanned(value.span.clone()),
                                )
                            }
                        }
                    }
                    Value::List(mut vs) => {
                        let msg = Message::new(val, None);
                        match vs.on_internal_message(env, &msg).await {
                            Some(reply) => {
                                reply
                            }
                            None => {
                                Value::Error(
                                    RuntimeError::ArgumentError("No handler found".to_string())
                                        .as_spanned(value.span.clone()),
                                )
                            }
                        }
                    }
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a channel".to_string())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }
            // Slice expression – index into a list.
            Expr::SliceExpr { target, index } => {
                let container = target.evaluate(env).await;
                let idx = index.evaluate(env).await;
                match (container, idx) {
                    (Value::List(vs), Value::Int(i)) => {
                        if i >= 0 && (i as usize) < vs.len() {
                            vs[i as usize].clone()
                        } else {
                            Value::Error(
                                RuntimeError::ArgumentError("Index out of bounds".to_string())
                                    .as_spanned(index.span.clone()),
                            )
                        }
                    }
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Type mismatch in slice".to_string())
                            .as_spanned(index.span.clone()),
                    ),
                }
            }
            // Expander expression – expand a block or list into scope
            Expr::Expander { target } => {
                // Evaluate the thing after the `*`
                let targ = target.evaluate(env).await;
                match targ {
                    // old block‐expansion stays the same
                    Value::Block(b) => b.evaluate(env).await,

                    // new: list‐expansion turns `[ch arg1 arg2 …]` into `ch.send_and_recv([arg1,arg2,…])`
                    Value::List(vs) => {
                        if vs.is_empty() {
                            return Value::Error(
                                RuntimeError::ArgumentError("Cannot expand empty list".to_string())
                                    .as_spanned(target.span.clone()),
                            );
                        }
                        // split off head + tail
                        let (head, args) = vs.split_first().unwrap();
                        match head {
                            Value::Channel(ch) => {
                                let payload = Value::List(args.to_vec());
                                tracing::warn!("Expanding list as message to channel: {:?}", payload);
                                match ch.send_and_recv(payload).await {
                                    Ok(reply) => reply,
                                    Err(e) => Value::Error(e.as_spanned(target.span.clone())),
                                }
                            }
                            _ => Value::Error(
                                RuntimeError::ArgumentError("First element is not a channel".to_string())
                                    .as_spanned(target.span.clone()),
                            ),
                        }
                    }

                    // everything else is an error
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a block or list".to_string())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }
        }
    }
}

#[async_trait]
impl Evaluate for Spanned<Statement> {
    type Input = Self;
    type Output = Value;

    async fn evaluate(&self, env: &mut Env) -> Value {
        match self.value.as_ref() {
            Statement::BlankLine => Value::Null,
            Statement::Comment(_) => Value::Null,
            Statement::Expr(expr) => expr.evaluate(env).await,
            Statement::InvalidBlock => Value::Error(
                RuntimeError::ArgumentError("Invalid block".to_string())
                    .as_spanned(self.span.clone()),
            ),
            Statement::Handler(handler) => {
                env.push_handler(handler.clone()).await;
                trace!("Handler pushed: {:?}", handler.to_sexpr());
                Value::Null
            }
            // For assignment, we now call our shared destructuring function.
            Statement::Assign { target, value } => {
                let evaluated_val = value.evaluate(env).await;
                match AssignmentDestructure::destructure(&target.value, &evaluated_val) {
                    DestructureResult::Match(actions) => {
                        apply_assignment_actions(actions, target.span.clone(), env).await
                    }
                    DestructureResult::NoMatch => Value::Error(
                        RuntimeError::ArgumentError("Assignment pattern mismatch".into())
                            .as_spanned(target.span.clone()),
                    ),
                    DestructureResult::Err(e) => Value::Error(e.as_spanned(target.span.clone())),
                }
            }
            Statement::Tell { target, value } => {
                let targ = target.evaluate(env).await;
                let val = value.evaluate(env).await;
                match targ {
                    Value::Channel(ch) => match ch.send(val).await {
                        Ok(_) => Value::Null,
                        Err(e) => Value::Error(e.as_spanned(value.span.clone())),
                    },
                    // TODO: merge with the `ask` expression?
                    Value::Dict(mut dict) => {
                        let msg = Message::new(val, None);
                        match dict.on_internal_message(env, &msg).await {
                            Some(reply) => reply,
                            None => {
                                Value::Error(
                                    RuntimeError::ArgumentError("No handler found".to_string())
                                        .as_spanned(value.span.clone()),
                                )
                            }
                        }
                    }
                    // TODO: merge with the `ask` expression?
                    Value::List(mut vs) => {
                        let msg = Message::new(val, None);
                        match vs.on_internal_message(env, &msg).await {
                            Some(reply) => reply,
                            None => {
                                Value::Error(
                                    RuntimeError::ArgumentError("No handler found".to_string())
                                        .as_spanned(value.span.clone()),
                                )
                            }
                        }
                    }
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a channel".to_string())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }
        }
    }
}

// ----------------------------------------------------------------
// Apply assignment actions produced by destructuring.
// ----------------------------------------------------------------
async fn apply_assignment_actions(
    actions: Vec<AssignmentAction>,
    target_span: crate::ast::Span,
    env: &mut Env,
) -> Value {
    for action in actions {
        match action {
            AssignmentAction::AssignVariable { name, value } => {
                env.set(&name, value).await;
            }
            AssignmentAction::AssignSlice {
                container,
                indices,
                value,
            } => {
                // Get the container from the environment.
                if let Some(current) = env.get(&container).await {
                    match current {
                        Value::List(mut vs) => {
                            if indices.len() != 1 {
                                return Value::Error(
                                    RuntimeError::ArgumentError(
                                        "Only single-index slice assignment supported".into(),
                                    )
                                        .as_spanned(target_span.clone()),
                                );
                            }
                            if let Value::Int(i) = indices[0].clone() {
                                if i < 0 || (i as usize) >= vs.len() {
                                    return Value::Error(
                                        RuntimeError::ArgumentError(
                                            "Index out of bounds in slice assignment".into(),
                                        )
                                            .as_spanned(target_span.clone()),
                                    );
                                }
                                vs[i as usize] = value;
                                env.set(&container, Value::List(vs)).await;
                            } else {
                                return Value::Error(
                                    RuntimeError::ArgumentError("Index must be an integer".into())
                                        .as_spanned(target_span.clone()),
                                );
                            }
                        }
                        _ => {
                            return Value::Error(
                                RuntimeError::ArgumentError(
                                    "Target for slice assignment is not a list".into(),
                                )
                                    .as_spanned(target_span.clone()),
                            );
                        }
                    }
                } else {
                    return Value::Error(
                        RuntimeError::ArgumentError("Container not found in environment".into())
                            .as_spanned(target_span.clone()),
                    );
                }
            }
        }
    }
    Value::Null
}

// ----------------------------------------------------------------
// Helper functions for slice assignment: get and set target values.
// These functions recursively retrieve or update the value of the assignment target.
// ----------------------------------------------------------------
fn get_target_value<'a>(
    target: &'a Spanned<AssignmentTarget>,
    env: &'a mut Env,
) -> Pin<Box<dyn Future<Output=Value> + Send + 'a>> {
    Box::pin(async move {
        match target.value.as_ref() {
            AssignmentTarget::Variable(name) => env.get(name).await.unwrap_or(Value::Null),
            AssignmentTarget::Slice {
                target: slice_target,
                index,
            } => {
                let container_val = get_target_value(slice_target, env).await;
                let idx_val = index.evaluate(env).await;
                match (container_val, idx_val) {
                    (Value::List(vs), Value::Int(i)) => {
                        if i >= 0 && (i as usize) < vs.len() {
                            vs[i as usize].clone()
                        } else {
                            Value::Null
                        }
                    }
                    _ => Value::Null,
                }
            }
            AssignmentTarget::List { .. } => Value::Null,
        }
    })
}

fn set_target_value<'a>(
    target: &'a Spanned<AssignmentTarget>,
    new_val: Value,
    env: &'a mut Env,
) -> Pin<Box<dyn Future<Output=Value> + Send + 'a>> {
    Box::pin(async move {
        match target.value.as_ref() {
            AssignmentTarget::Variable(name) => {
                env.set(name, new_val.clone()).await;
                new_val
            }
            AssignmentTarget::Slice {
                target: slice_target,
                index,
            } => {
                let mut container_val = get_target_value(slice_target, env).await;
                let idx_val = index.evaluate(env).await;
                match (container_val, idx_val) {
                    (Value::List(mut vs), Value::Int(i)) if i >= 0 && (i as usize) < vs.len() => {
                        vs[i as usize] = new_val.clone();
                        let updated = Value::List(vs);
                        set_target_value(slice_target, updated, env).await
                    }
                    _ => Value::Null,
                }
            }
            AssignmentTarget::List { .. } => Value::Null,
        }
    })
}

// -------------------------------
// Tests
// -------------------------------
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Operator, Span, Spanned};
    use crate::value::Value as AstValue;

    #[tokio::test]
    async fn test_evaluate_value() {
        let mut env = Env::default();
        let expr = Spanned::new(
            Span {
                file_id: 1,
                start: 0,
                end: 0,
            },
            Expr::Value(Spanned::new(
                Span {
                    file_id: 1,
                    start: 0,
                    end: 0,
                },
                AstValue::Int(10),
            )),
        );
        let result = expr.evaluate(&mut env).await;
        match result {
            Value::Int(n) => assert_eq!(n, 10),
            _ => panic!("Expected Value::Int(10)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_list() {
        let mut env = Env::default();
        let expr = Spanned::new(
            Span {
                file_id: 1,
                start: 0,
                end: 0,
            },
            Expr::List {
                elements: vec![
                    Spanned::new(
                        Span {
                            file_id: 1,
                            start: 0,
                            end: 0,
                        },
                        Expr::Value(Spanned::new(
                            Span {
                                file_id: 1,
                                start: 0,
                                end: 0,
                            },
                            AstValue::Int(1),
                        )),
                    ),
                    Spanned::new(
                        Span {
                            file_id: 1,
                            start: 1,
                            end: 1,
                        },
                        Expr::Value(Spanned::new(
                            Span {
                                file_id: 1,
                                start: 1,
                                end: 1,
                            },
                            AstValue::Int(2),
                        )),
                    ),
                ],
            },
        );
        let result = expr.evaluate(&mut env).await;
        match result {
            Value::List(vs) => {
                assert_eq!(vs.len(), 2);
                assert_eq!(vs[0], Value::Int(1));
                assert_eq!(vs[1], Value::Int(2));
            }
            _ => panic!("Expected a list of two integers."),
        }
    }

    #[tokio::test]
    async fn test_evaluate_binary_add() {
        let mut env = Env::default();
        let expr = Spanned::new(
            Span {
                file_id: 1,
                start: 0,
                end: 0,
            },
            Expr::BinaryExpr {
                lhs: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 0,
                        end: 0,
                    },
                    Expr::Value(Spanned::new(
                        Span {
                            file_id: 1,
                            start: 0,
                            end: 0,
                        },
                        AstValue::Int(2),
                    )),
                ),
                op: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 1,
                        end: 1,
                    },
                    Operator::Add,
                ),
                rhs: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 2,
                        end: 2,
                    },
                    Expr::Value(Spanned::new(
                        Span {
                            file_id: 1,
                            start: 2,
                            end: 2,
                        },
                        AstValue::Int(3),
                    )),
                ),
            },
        );
        let result = expr.evaluate(&mut env).await;
        match result {
            Value::Int(n) => assert_eq!(n, 5),
            _ => panic!("Expected Value::Int(5)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_binary_divide_by_zero() {
        let mut context = Env::default();
        let expr = Spanned::new(
            Span {
                file_id: 1,
                start: 0,
                end: 0,
            },
            Expr::BinaryExpr {
                lhs: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 0,
                        end: 0,
                    },
                    Expr::Value(Spanned::new(
                        Span {
                            file_id: 1,
                            start: 0,
                            end: 0,
                        },
                        AstValue::Int(10),
                    )),
                ),
                op: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 1,
                        end: 1,
                    },
                    Operator::Divide,
                ),
                rhs: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 2,
                        end: 2,
                    },
                    Expr::Value(Spanned::new(
                        Span {
                            file_id: 1,
                            start: 2,
                            end: 2,
                        },
                        AstValue::Int(0),
                    )),
                ),
            },
        );
        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Error(Spanned {
                             value: box RuntimeError::ArgumentError(msg),
                             ..
                         }) => {
                assert_eq!(msg, "Divide by zero");
            }
            _ => panic!("Expected a division by zero error"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_slice_expr() {
        let mut env = Env::default();
        let expr = Spanned::new(
            Span {
                file_id: 1,
                start: 0,
                end: 0,
            },
            Expr::SliceExpr {
                target: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 0,
                        end: 0,
                    },
                    Expr::List {
                        elements: vec![
                            Spanned::new(
                                Span {
                                    file_id: 1,
                                    start: 0,
                                    end: 0,
                                },
                                Expr::Value(Spanned::new(
                                    Span {
                                        file_id: 1,
                                        start: 0,
                                        end: 0,
                                    },
                                    AstValue::Int(10),
                                )),
                            ),
                            Spanned::new(
                                Span {
                                    file_id: 1,
                                    start: 1,
                                    end: 1,
                                },
                                Expr::Value(Spanned::new(
                                    Span {
                                        file_id: 1,
                                        start: 1,
                                        end: 1,
                                    },
                                    AstValue::Int(20),
                                )),
                            ),
                        ],
                    },
                ),
                index: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 2,
                        end: 2,
                    },
                    Expr::Value(Spanned::new(
                        Span {
                            file_id: 1,
                            start: 2,
                            end: 2,
                        },
                        AstValue::Int(1),
                    )),
                ),
            },
        );
        let result = expr.evaluate(&mut env).await;
        match result {
            Value::Int(n) => assert_eq!(n, 20),
            _ => panic!("Expected Value::Int(20)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_slice_expr_out_of_bounds_get_simulated_spanned_info() {
        let mut env = Env::default();
        let expr = Spanned::new(
            Span {
                file_id: 1,
                start: 0,
                end: 0,
            },
            Expr::SliceExpr {
                target: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 0,
                        end: 0,
                    },
                    Expr::List {
                        elements: vec![
                            Spanned::new(
                                Span {
                                    file_id: 1,
                                    start: 0,
                                    end: 0,
                                },
                                Expr::Value(Spanned::new(
                                    Span {
                                        file_id: 1,
                                        start: 0,
                                        end: 0,
                                    },
                                    AstValue::Int(10),
                                )),
                            ),
                            Spanned::new(
                                Span {
                                    file_id: 1,
                                    start: 1,
                                    end: 1,
                                },
                                Expr::Value(Spanned::new(
                                    Span {
                                        file_id: 1,
                                        start: 1,
                                        end: 1,
                                    },
                                    AstValue::Int(20),
                                )),
                            ),
                        ],
                    },
                ),
                index: Spanned::new(
                    Span {
                        file_id: 1,
                        start: 2,
                        end: 2,
                    },
                    Expr::Value(Spanned::new(
                        Span {
                            file_id: 1,
                            start: 2,
                            end: 2,
                        },
                        AstValue::Int(5),
                    )),
                ),
            },
        );
        let result = expr.evaluate(&mut env).await;
        match result {
            Value::Error(Spanned {
                             span,
                             value: box RuntimeError::ArgumentError(msg),
                         }) => {
                assert_eq!(msg, "Index out of bounds");
                assert_eq!(span.file_id, 1);
                assert_eq!(span.start, 2);
                assert_eq!(span.end, 2);
            }
            _ => panic!("Expected an out-of-bounds error"),
        }
    }
}
