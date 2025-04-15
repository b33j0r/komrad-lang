use crate::ast::{AssignmentTarget, Expr, Operator, Spanned, Statement};
use crate::env::Env;
use crate::AsSpanned;
use async_trait::async_trait;
use indexmap::IndexMap;
use std::future::Future;
use std::pin::Pin;

// Reuse the destructuring module for assignments.
use crate::destructure::{AssignmentAction, AssignmentDestructure, Destructure, DestructureResult};
use crate::error::RuntimeError;
use crate::value::Value;

#[allow(dead_code)]
pub struct EvaluationContext {
    pub env: Env,
}

impl Default for EvaluationContext {
    fn default() -> Self {
        EvaluationContext {
            env: Env::default(),
        }
    }
}

/// A trait specifying how to evaluate an AST node into a `Value`.
#[async_trait]
pub trait Evaluate {
    type Input;
    type Output = Value;

    /// Evaluates the given AST node into a `Value`.
    async fn evaluate(&self, context: &mut EvaluationContext) -> Value;
}

#[async_trait]
impl Evaluate for Spanned<Expr> {
    type Input = Self;
    type Output = Value;

    async fn evaluate(&self, context: &mut EvaluationContext) -> Value {
        match self.value.as_ref() {
            // 1) Literal/variable expression.
            Expr::Value(sp_val) => {
                match sp_val.value.as_ref() {
                    // If it's a variable ("Word"), look it up.
                    Value::Word(name) => match context.env.get(name).await {
                        Some(v) => v,
                        None => Value::Error(
                            RuntimeError::ArgumentError(format!("Undefined variable: {}", name))
                                .as_spanned(sp_val.span.clone()),
                        ),
                    },
                    // Otherwise, propagate the literal.
                    other => other.clone(),
                }
            }
            // 2) List expression.
            Expr::List { elements } => {
                let mut evaluated = Vec::with_capacity(elements.len());
                for elem in elements {
                    evaluated.push(elem.evaluate(context).await);
                }
                Value::List(evaluated)
            }
            // 3) Dictionary expression.
            Expr::Dict { index_map } => {
                let mut evaluated = IndexMap::new();
                for (key, val_expr) in index_map {
                    evaluated.insert(key.clone(), val_expr.evaluate(context).await);
                }
                Value::Dict(evaluated)
            }
            // 4) Binary expression.
            Expr::BinaryExpr { lhs, op, rhs } => {
                let left = lhs.evaluate(context).await;
                let right = rhs.evaluate(context).await;
                match op.value.as_ref() {
                    Operator::Add => match (left, right) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in addition".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Subtract => match (left, right) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in subtraction".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Multiply => match (left, right) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
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
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in division".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    _ => Value::Error(
                        RuntimeError::UnknownError("Unknown operator".to_string())
                            .as_spanned(op.span.clone()),
                    ),
                }
            }
            // 5) Ask expression – send_and_recv.
            Expr::Ask { target, value } => {
                let targ = target.evaluate(context).await;
                let val = value.evaluate(context).await;
                match targ {
                    Value::Channel(ch) => match ch.send_and_recv(val).await {
                        Ok(reply) => reply,
                        Err(e) => Value::Error(e.as_spanned(value.span.clone())),
                    },
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a channel".to_string())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }
            // 6) Slice expression – index into a list.
            Expr::SliceExpr { target, index } => {
                let container = target.evaluate(context).await;
                let idx = index.evaluate(context).await;
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
        }
    }
}

#[async_trait]
impl Evaluate for Spanned<Statement> {
    type Input = Self;
    type Output = Value;

    async fn evaluate(&self, context: &mut EvaluationContext) -> Value {
        match self.value.as_ref() {
            Statement::BlankLine => Value::Null,
            Statement::Comment(_) => Value::Null,
            Statement::Expr(expr) => expr.evaluate(context).await,
            Statement::InvalidBlock => Value::Error(
                RuntimeError::ArgumentError("Invalid block".to_string())
                    .as_spanned(self.span.clone()),
            ),
            Statement::Handler(handler) => {
                context.env.push_handler(handler.clone()).await;
                Value::Null
            }
            // For assignment, we now call our shared destructuring function.
            Statement::Assign { target, value } => {
                let evaluated_val = value.evaluate(context).await;
                match AssignmentDestructure::destructure(&target.value, &evaluated_val) {
                    DestructureResult::Match(actions) => {
                        apply_assignment_actions(actions, target.span.clone(), context).await
                    }
                    DestructureResult::NoMatch => Value::Error(
                        RuntimeError::ArgumentError("Assignment pattern mismatch".into())
                            .as_spanned(target.span.clone()),
                    ),
                    DestructureResult::Err(e) => Value::Error(e.as_spanned(target.span.clone())),
                }
            }
            Statement::Tell { target, value } => {
                let targ = target.evaluate(context).await;
                let val = value.evaluate(context).await;
                match targ {
                    Value::Channel(ch) => match ch.send(val).await {
                        Ok(_) => Value::Null,
                        Err(e) => Value::Error(e.as_spanned(value.span.clone())),
                    },
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a channel".to_string())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }
            Statement::Expand { target } => {
                let targ = target.evaluate(context).await;
                match targ {
                    Value::Block(b) => {
                        let mut result = Value::Null;
                        for stmt in &b.0 {
                            result = stmt.evaluate(context).await;
                        }
                        result
                    }
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a block".to_string())
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
    context: &mut EvaluationContext,
) -> Value {
    for action in actions {
        match action {
            AssignmentAction::AssignVariable { name, value } => {
                context.env.set(&name, value).await;
            }
            AssignmentAction::AssignSlice {
                container,
                indices,
                value,
            } => {
                // Get the container from the environment.
                if let Some(current) = context.env.get(&container).await {
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
                                context.env.set(&container, Value::List(vs)).await;
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
    context: &'a mut EvaluationContext,
) -> Pin<Box<dyn Future<Output=Value> + Send + 'a>> {
    Box::pin(async move {
        match target.value.as_ref() {
            AssignmentTarget::Variable(name) => context.env.get(name).await.unwrap_or(Value::Null),
            AssignmentTarget::Slice {
                target: slice_target,
                index,
            } => {
                let container_val = get_target_value(slice_target, context).await;
                let idx_val = index.evaluate(context).await;
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
    context: &'a mut EvaluationContext,
) -> Pin<Box<dyn Future<Output=Value> + Send + 'a>> {
    Box::pin(async move {
        match target.value.as_ref() {
            AssignmentTarget::Variable(name) => {
                context.env.set(name, new_val.clone()).await;
                new_val
            }
            AssignmentTarget::Slice {
                target: slice_target,
                index,
            } => {
                let mut container_val = get_target_value(slice_target, context).await;
                let idx_val = index.evaluate(context).await;
                match (container_val, idx_val) {
                    (Value::List(mut vs), Value::Int(i)) if i >= 0 && (i as usize) < vs.len() => {
                        vs[i as usize] = new_val.clone();
                        let updated = Value::List(vs);
                        set_target_value(slice_target, updated, context).await
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
        let mut context = EvaluationContext::default();
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
        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Int(n) => assert_eq!(n, 10),
            _ => panic!("Expected Value::Int(10)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_list() {
        let mut context = EvaluationContext::default();
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
        let result = expr.evaluate(&mut context).await;
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
        let mut context = EvaluationContext::default();
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
        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Int(n) => assert_eq!(n, 5),
            _ => panic!("Expected Value::Int(5)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_binary_divide_by_zero() {
        let mut context = EvaluationContext::default();
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
        let mut context = EvaluationContext::default();
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
        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Int(n) => assert_eq!(n, 20),
            _ => panic!("Expected Value::Int(20)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_slice_expr_out_of_bounds_get_simulated_spanned_info() {
        let mut context = EvaluationContext::default();
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
        let result = expr.evaluate(&mut context).await;
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
