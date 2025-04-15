use crate::ast::Pattern;
use crate::value::Value;
use crate::{Expr, Operator, Predicate, RuntimeError, Spanned};
use std::collections::HashMap;

/// Match result type for destructuring:
/// - `Match(O)` means successful destructuring
/// - `NoMatch` is a soft failure (e.g. for handler filters)
/// - `Err(E)` is a hard error (e.g. invalid predicate logic)
pub enum DestructureResult<O, E> {
    Match(O),
    NoMatch,
    Err(E),
}

/// A pattern matching low-level destructuring trait.
///
/// Used by handlers in both the built-in and dynamic agents.
/// Used to implement destructured assignments in the evaluator.
pub trait Destructure {
    type Target;
    type Output;
    type Input = Value;
    type Error = RuntimeError;

    fn destructure(
        target: &Self::Target,
        input: &Self::Input,
    ) -> DestructureResult<Self::Output, Self::Error>;
}

/// Implements destructuring logic for matching handler patterns against a value.
pub struct PatternDestructure;

impl Destructure for PatternDestructure {
    type Target = Pattern;
    type Output = HashMap<String, Value>;

    fn destructure(
        target: &Self::Target,
        input: &Self::Input,
    ) -> DestructureResult<Self::Output, Self::Error> {
        let mut bindings = HashMap::new();

        match (target, input) {
            // Exact match on literals
            (Pattern::ValueMatch(expected), actual) => {
                if expected == actual {
                    DestructureResult::Match(bindings)
                } else {
                    DestructureResult::NoMatch
                }
            }

            // Capture a variable
            (Pattern::VariableCapture(name), value) => {
                bindings.insert(name.clone(), value.clone());
                DestructureResult::Match(bindings)
            }

            // Capture a block if it's actually a block
            (Pattern::BlockCapture(name), value @ Value::Block(_)) => {
                bindings.insert(name.clone(), value.clone());
                DestructureResult::Match(bindings)
            }
            (Pattern::BlockCapture(_), _) => DestructureResult::NoMatch,

            // Match a predicate like _(x > 3)
            (Pattern::PredicateCapture(pred), value) => match evaluate_predicate(&*pred, value) {
                Ok(true) => DestructureResult::Match(bindings),
                Ok(false) => DestructureResult::NoMatch,
                Err(e) => DestructureResult::Err(e),
            },

            // List destructuring with nested elements
            (Pattern::List(pats), Value::List(vals)) => {
                if pats.len() != vals.len() {
                    return DestructureResult::NoMatch;
                }
                for (p, v) in pats.iter().zip(vals.iter()) {
                    match Self::destructure(p, v) {
                        DestructureResult::Match(nested) => {
                            bindings.extend(nested);
                        }
                        DestructureResult::NoMatch => return DestructureResult::NoMatch,
                        DestructureResult::Err(e) => return DestructureResult::Err(e),
                    }
                }
                DestructureResult::Match(bindings)
            }

            // Default fallback: mismatch
            _ => DestructureResult::NoMatch,
        }
    }
}

// Top-level function: evaluates a predicate against an input Value and returns a boolean.
// It expects the evaluated result to be a Value::Boolean; otherwise it fails.
fn evaluate_predicate(pred: &Predicate, input: &Value) -> Result<bool, RuntimeError> {
    let val = eval_predicate_expr(pred, input)?;
    match val {
        Value::Boolean(b) => Ok(b),
        _ => Err(RuntimeError::TypeError(
            "Predicate did not evaluate to a boolean".to_string(),
        )),
    }
}

// Recursively evaluates the predicate expression and returns a Value.
// In our case, any free variable (Predicate::Variable) is treated as the input value.
fn eval_predicate_expr(pred: &Predicate, input: &Value) -> Result<Value, RuntimeError> {
    match pred {
        // A literal in the predicate simply returns its value.
        Predicate::Value(v) => Ok(v.clone()),
        // A free variable is assumed to represent the input value.
        Predicate::Variable(_name) => Ok(input.clone()),
        // A binary expression is evaluated by recursively evaluating its operands
        // and then applying the operator to the results.
        Predicate::BinaryExpr { lhs, op, rhs } => {
            let left = eval_predicate_expr(&lhs.value, input)?;
            let right = eval_predicate_expr(&rhs.value, input)?;
            apply_operator(&left, &op.value, &right)
        }
    }
}

// Applies a binary operator to two operand Values.
// Supports basic arithmetic and comparison operators.
// Returns a Value (usually a numeric result for arithmetic or a Boolean for comparisons).
fn apply_operator(lhs: &Value, op: &Operator, rhs: &Value) -> Result<Value, RuntimeError> {
    match op {
        Operator::Add => match (lhs, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64) + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + (*b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            _ => Err(RuntimeError::TypeError(
                "Add operator requires numeric operands".to_string(),
            )),
        },
        Operator::Subtract => match (lhs, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64) - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - (*b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            _ => Err(RuntimeError::TypeError(
                "Subtract operator requires numeric operands".to_string(),
            )),
        },
        Operator::Multiply => match (lhs, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64) * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * (*b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            _ => Err(RuntimeError::TypeError(
                "Multiply operator requires numeric operands".to_string(),
            )),
        },
        Operator::Divide => {
            // Check for division by zero in any numeric combination.
            match (lhs, rhs) {
                (Value::Int(_), Value::Int(0))
                | (Value::Float(_), Value::Float(0.0))
                | (Value::Int(_), Value::Float(0.0))
                | (Value::Float(_), Value::Int(0)) => {
                    Err(RuntimeError::TypeError("Division by zero".to_string()))
                }
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64) / b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / (*b as f64))),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                _ => Err(RuntimeError::TypeError(
                    "Divide operator requires numeric operands".to_string(),
                )),
            }
        }
        Operator::Equal => Ok(Value::Boolean(lhs == rhs)),
        Operator::NotEqual => Ok(Value::Boolean(lhs != rhs)),
        Operator::GreaterThan => match (lhs, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Boolean(a > b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Boolean((*a as f64) > *b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Boolean(*a > (*b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a > b)),
            _ => Err(RuntimeError::TypeError(
                "GreaterThan operator requires numeric operands".to_string(),
            )),
        },
        Operator::LessThan => match (lhs, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Boolean(a < b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Boolean((*a as f64) < *b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Boolean(*a < (*b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a < b)),
            _ => Err(RuntimeError::TypeError(
                "LessThan operator requires numeric operands".to_string(),
            )),
        },
    }
}

/// Represents a low-level assignment action.
/// For a simple variable assignment we use `AssignVariable`.
/// For slice assignment we flatten the chain (e.g. vec[2][3] becomes container "vec" with indices [2, 3])
/// and produce an `AssignSlice` action.
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentAction {
    AssignVariable {
        name: String,
        value: Value,
    },
    AssignSlice {
        container: String,
        indices: Vec<Value>,
        value: Value,
    },
}

/// Implements destructuring logic for AssignmentTarget.
pub struct AssignmentDestructure;

impl Destructure for AssignmentDestructure {
    type Target = crate::ast::AssignmentTarget;
    type Output = Vec<AssignmentAction>;
    type Input = Value;
    type Error = RuntimeError;

    fn destructure(
        target: &Self::Target,
        input: &Self::Input,
    ) -> DestructureResult<Self::Output, Self::Error> {
        match target {
            // Simple variable assignment, e.g. a = 1
            crate::ast::AssignmentTarget::Variable(name) => {
                DestructureResult::Match(vec![AssignmentAction::AssignVariable {
                    name: name.clone(),
                    value: input.clone(),
                }])
            }
            // List destructuring, e.g. [a b c] = [1 2 3]
            crate::ast::AssignmentTarget::List { elements } => {
                if let Value::List(vals) = input {
                    if elements.len() != vals.len() {
                        return DestructureResult::NoMatch;
                    }
                    let mut actions = Vec::new();
                    for (elem, val) in elements.iter().zip(vals.iter()) {
                        match Self::destructure(&elem.value, val) {
                            DestructureResult::Match(mut acts) => actions.append(&mut acts),
                            DestructureResult::NoMatch => return DestructureResult::NoMatch,
                            DestructureResult::Err(e) => return DestructureResult::Err(e),
                        }
                    }
                    DestructureResult::Match(actions)
                } else {
                    DestructureResult::NoMatch
                }
            }
            // Slice assignment, e.g. vec[2] = 12.0
            crate::ast::AssignmentTarget::Slice { .. } => {
                // Flatten the slice chain into a base container and the list of index expressions.
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

/// Recursively flattens a slice assignment target into its base variable and index expressions.
/// For example, converting `vec[2][3]` into (`"vec"`, [expr_for_2, expr_for_3]).
fn flatten_slice(
    target: &crate::ast::AssignmentTarget,
) -> Result<(String, Vec<Spanned<Expr>>), RuntimeError> {
    match target {
        crate::ast::AssignmentTarget::Variable(name) => Ok((name.clone(), Vec::new())),
        crate::ast::AssignmentTarget::Slice {
            target: inner,
            index,
        } => {
            let (var, mut indices) = flatten_slice(&inner.value)?;
            indices.push(index.clone());
            Ok((var, indices))
        }
        crate::ast::AssignmentTarget::List { .. } => Err(RuntimeError::PatternMatchError(
            "Invalid slice assignment target".to_string(),
        )),
    }
}

/// Evaluates an index expression for slice assignments.
/// Only literal integer or float expressions are supported.
fn eval_index(expr: &Spanned<Expr>) -> Result<Value, RuntimeError> {
    match &*expr.value {
        Expr::Value(inner) => match &*inner.value {
            Value::Int(_) | Value::Float(_) => Ok(*inner.value.clone()),
            _ => Err(RuntimeError::TypeError(
                "Index expression must be an integer or float".to_string(),
            )),
        },
        _ => Err(RuntimeError::TypeError(
            "Unsupported index expression in assignment target".to_string(),
        )),
    }
}

// -----------------------------------------------------------------
// Tests for AssignmentDestructure
// -----------------------------------------------------------------

#[cfg(test)]
mod tests_assignment {
    use super::*;
    use crate::ast::{AssignmentTarget, Expr, Span, Spanned};
    use crate::value::Value;

    fn dummy_span() -> Span {
        Span {
            file_id: 0,
            start: 0,
            end: 0,
        }
    }

    #[test]
    fn test_variable_assignment() {
        // Test: a = 1
        let target = AssignmentTarget::Variable("a".to_string());
        let input = Value::Int(1);
        let result = AssignmentDestructure::destructure(&target, &input);
        match result {
            DestructureResult::Match(actions) => {
                assert_eq!(actions.len(), 1);
                assert_eq!(
                    actions[0],
                    AssignmentAction::AssignVariable {
                        name: "a".to_string(),
                        value: Value::Int(1)
                    }
                );
            }
            _ => panic!("Expected Match for variable assignment"),
        }
    }

    #[test]
    fn test_list_destructuring() {
        // Test: [a b c] = [1 2 3]
        let target = AssignmentTarget::List {
            elements: vec![
                Spanned::new(dummy_span(), AssignmentTarget::Variable("a".to_string())),
                Spanned::new(dummy_span(), AssignmentTarget::Variable("b".to_string())),
                Spanned::new(dummy_span(), AssignmentTarget::Variable("c".to_string())),
            ],
        };
        let input = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = AssignmentDestructure::destructure(&target, &input);
        match result {
            DestructureResult::Match(actions) => {
                // Expect three assignment actions.
                assert_eq!(actions.len(), 3);
                assert!(actions.contains(&AssignmentAction::AssignVariable {
                    name: "a".to_string(),
                    value: Value::Int(1)
                }));
                assert!(actions.contains(&AssignmentAction::AssignVariable {
                    name: "b".to_string(),
                    value: Value::Int(2)
                }));
                assert!(actions.contains(&AssignmentAction::AssignVariable {
                    name: "c".to_string(),
                    value: Value::Int(3)
                }));
            }
            _ => panic!("Expected Match for list destructuring"),
        }
    }

    #[test]
    fn test_slice_assignment() {
        // Test: vec[2] = 12.0
        // Build target: Slice { target: Variable("vec"), index: literal 2 }
        let var_target = Spanned::new(dummy_span(), AssignmentTarget::Variable("vec".to_string()));
        let index_expr = Spanned::new(
            dummy_span(),
            Expr::Value(Spanned::new(dummy_span(), Value::Int(2))),
        );
        let target = AssignmentTarget::Slice {
            target: var_target,
            index: index_expr,
        };
        let input = Value::Float(12.0);
        let result = AssignmentDestructure::destructure(&target, &input);
        match result {
            DestructureResult::Match(actions) => {
                assert_eq!(actions.len(), 1);
                let expected = AssignmentAction::AssignSlice {
                    container: "vec".to_string(),
                    indices: vec![Value::Int(2)],
                    value: Value::Float(12.0),
                };
                assert_eq!(actions[0], expected);
            }
            _ => panic!("Expected Match for slice assignment"),
        }
    }

    #[test]
    fn test_list_destructuring_mismatch() {
        // Test list destructuring failure due to mismatched lengths.
        let target = AssignmentTarget::List {
            elements: vec![
                Spanned::new(dummy_span(), AssignmentTarget::Variable("a".to_string())),
                Spanned::new(dummy_span(), AssignmentTarget::Variable("b".to_string())),
            ],
        };
        let input = Value::List(vec![Value::Int(1)]);
        let result = AssignmentDestructure::destructure(&target, &input);
        match result {
            DestructureResult::NoMatch => {}
            _ => panic!("Expected NoMatch due to list length mismatch"),
        }
    }
}

#[cfg(test)]
mod tests_pattern_destructure {
    use super::*;
    use crate::Span;

    // Utility: create a dummy span.
    fn dummy_span() -> Span {
        Span {
            file_id: 0,
            start: 0,
            end: 0,
        }
    }

    #[test]
    fn test_evaluate_predicate_literal() {
        // Predicate::Value with a boolean literal returns its value.
        let pred = Predicate::Value(Value::Boolean(true));
        let input = Value::Int(42); // input is irrelevant in this case
        let result = evaluate_predicate(&pred, &input);
        assert_eq!(result, Ok(true));
    }

    #[test]
    fn test_evaluate_predicate_variable() {
        // Predicate::Variable is treated as a free variable that returns the input.
        let pred = Predicate::Variable("x".into());
        let input = Value::Boolean(true);
        let result = evaluate_predicate(&pred, &input);
        assert_eq!(result, Ok(true));

        let input = Value::Boolean(false);
        let result = evaluate_predicate(&pred, &input);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_evaluate_predicate_binary_expr() {
        // Build a predicate for "input > 3":
        // Equivalent to: (Variable > 3)
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".into()));
        let spanned_three = Spanned::new(dummy_span(), Predicate::Value(Value::Int(3)));
        let op = Spanned::new(dummy_span(), Operator::GreaterThan);
        let pred = Predicate::BinaryExpr {
            lhs: spanned_var,
            op,
            rhs: spanned_three,
        };

        // For an input of 10, we expect true.
        let input = Value::Int(10);
        let result = evaluate_predicate(&pred, &input);
        assert_eq!(result, Ok(true));

        // For an input of 2, we expect false.
        let input = Value::Int(2);
        let result = evaluate_predicate(&pred, &input);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_destructure_value_match() {
        // Pattern::ValueMatch should succeed if the literal matches.
        let pattern = Pattern::ValueMatch(Value::Int(10));
        let input = Value::Int(10);
        let result = PatternDestructure::destructure(&pattern, &input);
        match result {
            DestructureResult::Match(bindings) => {
                // No bindings are expected in a literal match.
                assert!(
                    bindings.is_empty(),
                    "Expected no bindings for literal match"
                );
            }
            _ => panic!("Expected Match for value literal"),
        }

        // With a non-matching literal, we should get NoMatch.
        let input = Value::Int(11);
        let result = PatternDestructure::destructure(&pattern, &input);
        match result {
            DestructureResult::NoMatch => (),
            _ => panic!("Expected NoMatch for literal mismatch"),
        }
    }

    #[test]
    fn test_destructure_variable_capture() {
        // Pattern::VariableCapture binds the variable to the input.
        let pattern = Pattern::VariableCapture("x".into());
        let input = Value::String("hello".into());
        let result = PatternDestructure::destructure(&pattern, &input);
        match result {
            DestructureResult::Match(bindings) => {
                assert_eq!(bindings.get("x"), Some(&input));
            }
            _ => panic!("Expected a Match for variable capture"),
        }
    }

    #[test]
    fn test_destructure_list() {
        // Pattern::List example: [VariableCapture "a", ValueMatch 20]
        let p1 = Spanned::new(dummy_span(), Pattern::VariableCapture("a".into()));
        let p2 = Spanned::new(dummy_span(), Pattern::ValueMatch(Value::Int(20)));
        let pattern = Pattern::List(vec![p1, p2]);

        // Input matches list structure: [10, 20]
        let input = Value::List(vec![Value::Int(10), Value::Int(20)]);
        let result = PatternDestructure::destructure(&pattern, &input);
        match result {
            DestructureResult::Match(bindings) => {
                assert_eq!(bindings.get("a"), Some(&Value::Int(10)));
            }
            _ => panic!("Expected a Match for list destructuring"),
        }

        // Mismatched list length: should result in NoMatch.
        let input = Value::List(vec![Value::Int(10)]);
        let result = PatternDestructure::destructure(&pattern, &input);
        match result {
            DestructureResult::NoMatch => (),
            _ => panic!("Expected NoMatch for list length mismatch"),
        }
    }

    #[test]
    fn test_destructure_predicate_capture() {
        // Pattern::PredicateCapture: capture if input > 5.
        // Build predicate: (Variable > 5)
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".into()));
        let spanned_five = Spanned::new(dummy_span(), Predicate::Value(Value::Int(5)));
        let op = Spanned::new(dummy_span(), Operator::GreaterThan);
        let pred = Spanned::new(
            dummy_span(),
            Predicate::BinaryExpr {
                lhs: spanned_var,
                op,
                rhs: spanned_five,
            },
        );
        let pattern = Pattern::PredicateCapture(pred);

        // With an input greater than 5, the predicate should evaluate to true.
        let input = Value::Int(10);
        let result = PatternDestructure::destructure(&pattern, &input);
        match result {
            DestructureResult::Match(bindings) => {
                // No bindings are expected from a predicate match.
                assert!(
                    bindings.is_empty(),
                    "Expected no bindings for predicate match"
                );
            }
            _ => panic!("Expected Match for predicate capture when condition is true"),
        }

        // With an input not greater than 5, we should get NoMatch.
        let input = Value::Int(3);
        let result = PatternDestructure::destructure(&pattern, &input);
        match result {
            DestructureResult::NoMatch => (),
            _ => panic!("Expected NoMatch for predicate capture when condition is false"),
        }
    }
}
