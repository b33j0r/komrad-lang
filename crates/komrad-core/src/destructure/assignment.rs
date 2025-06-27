use crate::error::RuntimeError;
use crate::value::Value;
use crate::{AssignmentTarget, Expr, Spanned};
use crate::destructure::base::{DestructureResult, Destructure};

/// Represents a low-level assignment action.
/// For a simple variable assignment we use `AssignVariable`.
/// For slice assignment we flatten the chain and produce `AssignSlice`.
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentAction {
    AssignVariable { name: String, value: Value },
    AssignSlice { container: String, indices: Vec<Value>, value: Value },
}

/// Implements destructuring logic for AssignmentTarget.
pub struct AssignmentDestructure;

impl Destructure for AssignmentDestructure {
    type Target = crate::ast::AssignmentTarget;
    type Output = Vec<AssignmentAction>;

    fn destructure(
        target: &Self::Target,
        input: &Self::Input,
    ) -> DestructureResult<Self::Output, Self::Error> {
        match target {
            AssignmentTarget::Variable(name) => {
                DestructureResult::Match(vec![AssignmentAction::AssignVariable {
                    name: name.clone(),
                    value: input.clone(),
                }])
            }
            AssignmentTarget::List { elements } => {
                match input {
                    Value::List(vals) => {
                        if elements.len() != vals.len() {
                            return DestructureResult::Err(
                                RuntimeError::ArgumentError(format!(
                                    "List pattern length mismatch: expected {}, got {}",
                                    elements.len(),
                                    vals.len()
                                )),
                            );
                        }

                        let mut actions = Vec::new();
                        for (elem, val) in elements.iter().zip(vals.iter()) {
                            match Self::destructure(&elem.value, val) {
                                DestructureResult::Match(mut acts) => actions.append(&mut acts),
                                DestructureResult::NoMatch => {
                                    return DestructureResult::Err(RuntimeError::ArgumentError(
                                        "Nested list element did not match".to_string(),
                                    ));
                                }
                                DestructureResult::Err(e) => return DestructureResult::Err(e),
                            }
                        }

                        DestructureResult::Match(actions)
                    }

                    other => {
                        DestructureResult::Err(RuntimeError::ArgumentError(format!(
                            "Cannot destructure {:?} into a list pattern",
                            other
                        )))
                    }
                }
            }

            AssignmentTarget::Slice { .. } => {
                match flatten_slice(target) {
                    Ok((container, indices_spanned)) => {
                        let mut indices = Vec::new();
                        for expr in indices_spanned {
                            match eval_index(&expr) {
                                Ok(idx_val) => indices.push(idx_val),
                                Err(e) => return DestructureResult::Err(e),
                            }
                        }
                        DestructureResult::Match(vec![AssignmentAction::AssignSlice {
                            container,
                            indices,
                            value: input.clone(),
                        }])
                    }
                    Err(e) => DestructureResult::Err(e),
                }
            }
        }
    }
}

fn flatten_slice(
    target: &crate::ast::AssignmentTarget,
) -> Result<(String, Vec<Spanned<Expr>>), RuntimeError> {
    match target {
        crate::ast::AssignmentTarget::Variable(name) => Ok((name.clone(), vec![])),
        crate::ast::AssignmentTarget::Slice { target: inner, index } => {
            let (var, mut indices) = flatten_slice(&inner.value)?;
            indices.push(index.clone());
            Ok((var, indices))
        }
        crate::ast::AssignmentTarget::List { .. } => Err(RuntimeError::PatternMatchError(
            "Invalid slice assignment target".to_string(),
        )),
    }
}

fn eval_index(expr: &Spanned<Expr>) -> Result<Value, RuntimeError> {
    if let Expr::Value(inner) = &*expr.value {
        match &*inner.value {
            Value::Int(_) | Value::Float(_) => Ok(*inner.value.clone()),
            _ => Err(RuntimeError::TypeError(
                "Index expression must be an integer or float".to_string(),
            )),
        }
    } else {
        Err(RuntimeError::TypeError(
            "Unsupported index expression in assignment target".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AssignmentTarget, Expr, Span, Spanned};
    use crate::value::Value;

    fn dummy_span() -> Span {
        Span { file_id: 0, start: 0, end: 0 }
    }

    #[test]
    fn test_variable_assignment() {
        let target = AssignmentTarget::Variable("a".to_string());
        let input = Value::Int(1);
        let result = AssignmentDestructure::destructure(&target, &input);
        match result {
            DestructureResult::Match(actions) => {
                assert_eq!(actions.len(), 1);
                assert_eq!(actions[0], AssignmentAction::AssignVariable { name: "a".to_string(), value: Value::Int(1) });
            }
            _ => panic!("Expected Match for variable assignment"),
        }
    }

    #[test]
    fn test_list_destructuring() {
        let target = AssignmentTarget::List {
            elements: vec![
                Spanned::new(dummy_span(), AssignmentTarget::Variable("a".to_string())),
                Spanned::new(dummy_span(), AssignmentTarget::Variable("b".to_string())),
                Spanned::new(dummy_span(), AssignmentTarget::Variable("c".to_string())),
            ]
        };
        let input = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = AssignmentDestructure::destructure(&target, &input);
        match result {
            DestructureResult::Match(actions) => {
                assert_eq!(actions.len(), 3);
                assert!(actions.contains(&AssignmentAction::AssignVariable { name: "a".to_string(), value: Value::Int(1) }));
                assert!(actions.contains(&AssignmentAction::AssignVariable { name: "b".to_string(), value: Value::Int(2) }));
                assert!(actions.contains(&AssignmentAction::AssignVariable { name: "c".to_string(), value: Value::Int(3) }));
            }
            _ => panic!("Expected Match for list destructuring"),
        }
    }

    #[test]
    fn test_slice_assignment() {
        let var_target = Spanned::new(dummy_span(), AssignmentTarget::Variable("vec".to_string()));
        let index_expr = Spanned::new(dummy_span(), Expr::Value(Spanned::new(dummy_span(), Value::Int(2))));
        let target = AssignmentTarget::Slice { target: var_target, index: index_expr };
        let input = Value::Float(12.0);
        let result = AssignmentDestructure::destructure(&target, &input);
        match result {
            DestructureResult::Match(actions) => {
                assert_eq!(actions.len(), 1);
                let expected = AssignmentAction::AssignSlice { container: "vec".to_string(), indices: vec![Value::Int(2)], value: Value::Float(12.0) };
                assert_eq!(actions[0], expected);
            }
            _ => panic!("Expected Match for slice assignment"),
        }
    }

    #[test]
    fn test_list_destructuring_mismatch() {
        let target = AssignmentTarget::List {
            elements: vec![
                Spanned::new(dummy_span(), AssignmentTarget::Variable("a".to_string())),
                Spanned::new(dummy_span(), AssignmentTarget::Variable("b".to_string())),
            ]
        };
        let input = Value::List(vec![Value::Int(1)]);
        let result = AssignmentDestructure::destructure(&target, &input);
        assert!(matches!(result, DestructureResult::Err(_)));
    }
}