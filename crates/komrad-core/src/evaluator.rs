use crate::ast::{Expr, Operator, RuntimeError, Spanned, Value};
use crate::env::Env;
use crate::AsSpanned;
use async_trait::async_trait;
use indexmap::IndexMap;

#[allow(dead_code)]
pub struct EvaluationContext {
    env: Env,
}

impl Default for EvaluationContext {
    fn default() -> Self {
        EvaluationContext {
            env: Env::default(),
        }
    }
}

/// A trait specifying how to evaluate an `Expr` into a `Value`.
#[async_trait]
pub trait Evaluate {
    type Input;
    type Output = Value;

    /// Evaluates the given AST node into a `Value`.
    async fn evaluate(&self, context: &mut EvaluationContext) -> Value;
}

#[async_trait]
impl Evaluate for Expr {
    type Input = Self;

    async fn evaluate(&self, context: &mut EvaluationContext) -> Value {
        match self {
            // Bind a word (variable) to a value
            Expr::Value(Spanned {
                            value: box Value::Word(name),
                            span,
                        }) => Value::Error(
                RuntimeError::ArgumentError(format!("Undefined variable: {}", name))
                    .as_spanned(span.clone()),
            ),

            // Value is just propagated
            Expr::Value(Spanned { box value, .. }) => value.clone(),

            // Evaluate each element in the list
            Expr::List { elements } => {
                let mut evaluated_elements = Vec::with_capacity(elements.len());
                for elem in elements {
                    let value = elem.value.evaluate(context).await;
                    evaluated_elements.push(value);
                }
                Value::List(evaluated_elements)
            }

            // Evaluate each value in the dictionary
            Expr::Dict { index_map } => {
                let mut evaluated_map = IndexMap::new();
                for (key, value) in index_map {
                    let key_val = key.clone();
                    let value_val = value.value.evaluate(context).await;
                    evaluated_map.insert(key_val, value_val);
                }
                Value::Dict(evaluated_map)
            }

            // Evaluate the binary expression recursively
            Expr::BinaryExpr { lhs, op, rhs } => {
                let left_val = lhs.value.evaluate(context).await;
                let right_val = rhs.value.evaluate(context).await;
                match op.value {
                    box Operator::Add => match (left_val, right_val) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in addition".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    box Operator::Subtract => match (left_val, right_val) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in subtraction".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    box Operator::Multiply => match (left_val, right_val) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError(
                                "Type mismatch in multiplication".to_string(),
                            )
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    box Operator::Divide => match (left_val, right_val) {
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

            // TODO: send the ask with channel.send_and_recv
            Expr::Ask { target, value } => {
                let target_val = target.value.evaluate(context).await;
                let value_val = value.value.evaluate(context).await;
                Value::List(vec![target_val, value_val])
            }

            // return the item at the index for lists. return OutOfBounds if index is out of range
            Expr::SliceExpr { target, index } => {
                let target_val = target.value.evaluate(context).await;
                let index_val = index.value.evaluate(context).await;

                match (target_val, index_val) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Spanned};

    #[tokio::test]
    async fn test_evaluate_value() {
        let mut context = &mut EvaluationContext::default();
        let expr = Expr::Value(Spanned {
            value: Box::new(Value::Int(10)),
            span: Span {
                file_id: 1,
                start: 0,
                end: 0,
            },
        });
        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Int(n) => assert_eq!(n, 10),
            _ => panic!("Expected Value::Int(10)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_list() {
        let mut context = &mut EvaluationContext::default();
        let expr = Expr::List {
            elements: vec![
                Spanned::new(
                    Span {
                        file_id: 1,
                        start: 0,
                        end: 0,
                    },
                    Expr::Value(Spanned {
                        value: Box::new(Value::Int(1)),
                        span: Span {
                            file_id: 1,
                            start: 0,
                            end: 0,
                        },
                    }),
                ),
                Spanned::new(
                    Span {
                        file_id: 1,
                        start: 1,
                        end: 1,
                    },
                    Expr::Value(Spanned {
                        value: Box::new(Value::Int(2)),
                        span: Span {
                            file_id: 1,
                            start: 1,
                            end: 1,
                        },
                    }),
                ),
            ],
        };
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
        let mut context = &mut EvaluationContext::default();
        let expr = Expr::BinaryExpr {
            lhs: Spanned::new(
                Span {
                    file_id: 1,
                    start: 0,
                    end: 0,
                },
                Expr::Value(Spanned {
                    value: Box::new(Value::Int(2)),
                    span: Span {
                        file_id: 1,
                        start: 0,
                        end: 0,
                    },
                }),
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
                Expr::Value(Spanned {
                    value: Box::new(Value::Int(3)),
                    span: Span {
                        file_id: 1,
                        start: 2,
                        end: 2,
                    },
                }),
            ),
        };
        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Int(n) => assert_eq!(n, 5),
            _ => panic!("Expected Value::Int(5)"),
        }
    }

    // Change the pattern matching in the test function to expect a boxed error
    #[tokio::test]
    async fn test_evaluate_binary_divide_by_zero() {
        let mut context = &mut EvaluationContext::default();
        let expr = Expr::BinaryExpr {
            lhs: Spanned::new(
                Span {
                    file_id: 1,
                    start: 0,
                    end: 0,
                },
                Expr::Value(Spanned {
                    value: Box::new(Value::Int(10)),
                    span: Span {
                        file_id: 1,
                        start: 0,
                        end: 0,
                    },
                }),
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
                Expr::Value(Spanned {
                    value: Box::new(Value::Int(0)),
                    span: Span {
                        file_id: 1,
                        start: 2,
                        end: 2,
                    },
                }),
            ),
        };
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
        let mut context = &mut EvaluationContext::default();
        let expr = Expr::SliceExpr {
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
                            Expr::Value(Spanned {
                                value: Box::new(Value::Int(10)),
                                span: Span {
                                    file_id: 1,
                                    start: 0,
                                    end: 0,
                                },
                            }),
                        ),
                        Spanned::new(
                            Span {
                                file_id: 1,
                                start: 1,
                                end: 1,
                            },
                            Expr::Value(Spanned {
                                value: Box::new(Value::Int(20)),
                                span: Span {
                                    file_id: 1,
                                    start: 1,
                                    end: 1,
                                },
                            }),
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
                Expr::Value(Spanned {
                    value: Box::new(Value::Int(1)),
                    span: Span {
                        file_id: 1,
                        start: 2,
                        end: 2,
                    },
                }),
            ),
        };

        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Int(n) => assert_eq!(n, 20),
            _ => panic!("Expected Value::Int(20)"),
        }
    }

    #[tokio::test]
    async fn test_evaluate_slice_expr_out_of_bounds_get_simulated_spanned_info() {
        let mut context = &mut EvaluationContext::default();
        let expr = Expr::SliceExpr {
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
                                Value::Int(10),
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
                                Value::Int(20),
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
                    Value::Int(5),
                )),
            ),
        };
        let result = expr.evaluate(&mut context).await;
        match result {
            Value::Error(Spanned {
                             span,
                             value: box RuntimeError::ArgumentError(msg),
                             ..
                         }) => {
                assert_eq!(msg, "Index out of bounds");
                assert_eq!(span.file_id, 1);
                assert_eq!(span.start, 2);
                assert_eq!(span.end, 2);
            }
            _ => panic!("Expected an out-of-bounds error."),
        }
    }
}
