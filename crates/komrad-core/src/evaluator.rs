use crate::ast::{
    AssignmentTarget, Expr, Operator, RuntimeError, Spanned, Statement, Value,
};
use crate::env::Env;
use crate::AsSpanned;
use async_trait::async_trait;
use indexmap::IndexMap;
use std::pin::Pin;

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
        // Spanned<Expr> has self.value: Box<Expr>. We'll match on self.value.as_ref() => &Expr
        match self.value.as_ref() {
            // 1) A wrapped literal or variable
            Expr::Value(sp_val) => {
                // sp_val is a Spanned<Value>. Inside that, sp_val.value is Box<Value>.
                // Use sp_val.value.as_ref() => &Value
                match sp_val.value.as_ref() {
                    // If it's a Word, we look it up in the environment.
                    Value::Word(name) => {
                        // env.get expects &str, so borrow name
                        match context.env.get(name).await {
                            Some(v) => v,
                            None => Value::Error(
                                RuntimeError::ArgumentError(format!("Undefined variable: {}", name))
                                    .as_spanned(sp_val.span.clone()),
                            ),
                        }
                    }
                    // Otherwise, it's some other literal (Int, String, etc.): just clone it.
                    other_val => other_val.clone(),
                }
            }

            // 2) A list expression
            Expr::List { elements } => {
                let mut evaluated = Vec::with_capacity(elements.len());
                for elem in elements {
                    evaluated.push(elem.evaluate(context).await);
                }
                Value::List(evaluated)
            }

            // 3) A dictionary expression
            Expr::Dict { index_map } => {
                let mut evaluated = IndexMap::new();
                for (key, val_expr) in index_map {
                    let val = val_expr.evaluate(context).await;
                    evaluated.insert(key.clone(), val);
                }
                Value::Dict(evaluated)
            }

            // 4) A binary expression
            Expr::BinaryExpr { lhs, op, rhs } => {
                let left_val = lhs.evaluate(context).await;
                let right_val = rhs.evaluate(context).await;

                // op is Spanned<Operator>. So op.value is Box<Operator>.
                match op.value.as_ref() {
                    Operator::Add => match (left_val, right_val) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in addition".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Subtract => match (left_val, right_val) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in subtraction".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Multiply => match (left_val, right_val) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                        _ => Value::Error(
                            RuntimeError::ArgumentError("Type mismatch in multiplication".to_string())
                                .as_spanned(rhs.span.clone()),
                        ),
                    },
                    Operator::Divide => match (left_val, right_val) {
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

            // 5) Ask expression => send_and_recv
            Expr::Ask { target, value } => {
                let target_val = target.evaluate(context).await;
                let value_val = value.evaluate(context).await;
                match target_val {
                    Value::Channel(ch) => match ch.send_and_recv(value_val).await {
                        Ok(reply) => reply,
                        Err(e) => Value::Error(e.as_spanned(value.span.clone())),
                    },
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a channel".to_string())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }

            // 6) Slice expression => index into a list
            Expr::SliceExpr { target, index } => {
                let container_val = target.evaluate(context).await;
                let index_val = index.evaluate(context).await;
                match (container_val, index_val) {
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
        // For statements, do the same: match self.value.as_ref() => &Statement
        match self.value.as_ref() {
            Statement::BlankLine => Value::Null,

            Statement::Comment(_) => Value::Null,

            // Evaluate a single expression
            Statement::Expr(expr) => expr.evaluate(context).await,

            Statement::InvalidBlock => Value::Error(
                RuntimeError::ArgumentError("Invalid block".to_string())
                    .as_spanned(self.span.clone()),
            ),

            // Register a handler into the environment
            Statement::Handler(handler) => {
                context.env.push_handler(handler.clone()).await;
                Value::Null
            }

            // Assignment
            Statement::Assign { target, value } => {
                evaluate_assignment(
                    target,
                    value,
                    context,
                ).await
            }

            // "Tell" => send a message but do not await a reply
            Statement::Tell { target, value } => {
                let target_val = target.evaluate(context).await;
                let value_val = value.evaluate(context).await;
                match target_val {
                    Value::Channel(ch) => match ch.send(value_val).await {
                        Ok(_) => Value::Null,
                        Err(e) => Value::Error(e.as_spanned(value.span.clone())),
                    },
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Target is not a channel".to_string())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }

            // "Expand" => if the target is a block, run each statement in it
            Statement::Expand { target } => {
                let target_val = target.evaluate(context).await;
                match target_val {
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


// ----------------------------------------------------------------------------
// Evaluate assignment targets
// ----------------------------------------------------------------------------
fn evaluate_assignment<'a>(
    target: &'a Spanned<AssignmentTarget>,
    value_expr: &'a Spanned<Expr>,
    context: &'a mut EvaluationContext,
) -> Pin<Box<dyn Future<Output=Value> + Send + 'a>> {
    Box::pin(async move {
        let val = value_expr.evaluate(context).await;
        match target.value.as_ref() {
            AssignmentTarget::Variable(name) => {
                // Simple variable assignment
                context.env.set(name, val).await;
                Value::Null
            }

            // A slice target, e.g. `x[5] = ...`
            AssignmentTarget::Slice { target: slice_target, index } => {
                // 1) Retrieve the container
                let container_value = get_target_value(slice_target, context).await;
                // 2) Evaluate the slice index
                let idx_val = index.evaluate(context).await;

                match (container_value, idx_val) {
                    (Value::List(mut vs), Value::Int(i)) => {
                        if i < 0 || i as usize >= vs.len() {
                            return Value::Error(
                                RuntimeError::ArgumentError("Index out of bounds in slice assign".into())
                                    .as_spanned(index.span.clone()),
                            );
                        }
                        // 3) Update the list
                        vs[i as usize] = val;

                        // 4) Store updated container back into the environment or parent
                        let updated = Value::List(vs);
                        set_target_value(slice_target, updated, context).await
                    }
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Type mismatch or invalid slice assignment".into())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }

            // List destructuring, e.g. `(x, y) = some_list`
            // That means we match each subtarget to each element
            AssignmentTarget::List { elements } => {
                match val {
                    Value::List(vs) => {
                        if vs.len() != elements.len() {
                            return Value::Error(
                                RuntimeError::ArgumentError(format!(
                                    "List destructuring mismatch: got {}, expected {}",
                                    vs.len(),
                                    elements.len()
                                ))
                                    .as_spanned(target.span.clone()),
                            );
                        }
                        // For each sub-target, recursively assign from the corresponding item
                        for (i, elem_target) in elements.iter().enumerate() {
                            // Wrap vs[i] in a temporary Value::Expr spanned
                            // so that we can feed it to evaluate_assignment
                            let artificial_expr = Spanned::new(
                                elem_target.span.clone(),
                                Expr::Value(Spanned::new(
                                    elem_target.span.clone(),
                                    vs[i].clone(),
                                )),
                            );
                            let result = evaluate_assignment(elem_target, &artificial_expr, context).await;
                            if let Value::Error(_) = result {
                                return result; // propagate the error
                            }
                        }
                        Value::Null
                    }
                    _ => Value::Error(
                        RuntimeError::ArgumentError("Right-hand side not a list for destructuring".into())
                            .as_spanned(target.span.clone()),
                    ),
                }
            }
        }
    })
}

/// Recursively retrieves the current value of an assignment target
/// so we can mutate it. For `Variable`, we get from the env.
/// For `Slice`, we recursively retrieve the container, then do the slice.
fn get_target_value<'a>(
    target: &'a Spanned<AssignmentTarget>,
    context: &'a mut EvaluationContext,
) -> Pin<Box<dyn Future<Output=Value> + Send + 'a>> {
    Box::pin(async move {
        match target.value.as_ref() {
            AssignmentTarget::Variable(name) => {
                context.env.get(name).await.unwrap_or(Value::Null)
            }
            AssignmentTarget::Slice { target: slice_target, index } => {
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
            AssignmentTarget::List { elements: _ } => {
                // Handle list destructuring (if needed)
                Value::Null
            }
        }
    })
}


/// Recursively sets the updated container after a slice assignment
fn set_target_value<'a>(
    target: &'a Spanned<AssignmentTarget>,
    new_val: Value,
    context: &'a mut EvaluationContext,
) -> Pin<Box<dyn Future<Output=Value> + Send + 'a>> {
    Box::pin(async move {
        match target.value.as_ref() {
            AssignmentTarget::Variable(name) => {
                // Store new_val in your environment
                context.env.set(name, new_val.clone()).await;
                new_val
            }
            AssignmentTarget::Slice { target: slice_target, index } => {
                // 1. Retrieve container
                let mut container_val = get_target_value(slice_target, context).await;
                // 2. Evaluate index
                let idx_val = index.evaluate(context).await;
                // 3. If container is a List and idx_val is an Int, replace
                //    the specified element and update the container in env
                match (container_val, idx_val) {
                    (Value::List(mut vs), Value::Int(i)) if i >= 0 && (i as usize) < vs.len() => {
                        vs[i as usize] = new_val.clone();
                        // Update container
                        let updated = Value::List(vs);
                        set_target_value(slice_target, updated, context).await
                    }
                    _ => Value::Null,
                }
            }
            AssignmentTarget::List { elements: _ } => {
                // If you allow setting directly to a list pattern, handle it here
                // Or leave it as a no-op
                Value::Null
            }
        }
    })
}


// -------------------------------
// Tests
// -------------------------------
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Operator, Span, Spanned, Value as AstValue};

    #[tokio::test]
    async fn test_evaluate_value() {
        let mut context = &mut EvaluationContext::default();
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
        let mut context = &mut EvaluationContext::default();
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
        let mut context = &mut EvaluationContext::default();
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

    // Checks that dividing by zero returns an ArgumentError
    #[tokio::test]
    async fn test_evaluate_binary_divide_by_zero() {
        let mut context = &mut EvaluationContext::default();
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
        let mut context = &mut EvaluationContext::default();
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
        let mut context = &mut EvaluationContext::default();
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
            _ => panic!("Expected an out-of-bounds error."),
        }
    }
}
