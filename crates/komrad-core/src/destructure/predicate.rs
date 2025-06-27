use crate::error::RuntimeError;
use crate::value::Value;
use crate::{Operator, Predicate};
use tracing::trace;

// Top-level function: evaluates a predicate against an input Value and returns a boolean.
// It expects the evaluated result to be a Value::Boolean; otherwise it fails.
pub fn evaluate_predicate(pred: &Predicate, input: &Value) -> Result<bool, RuntimeError> {
    trace!("Evaluating predicate: {:?} with input: {:?}", pred, input);
    let val = eval_predicate_expr(pred, input)?;
    match val {
        Value::Boolean(b) => {
            trace!("Predicate evaluated to: {:?}", b);
            Ok(b)
        }
        _ => Err(RuntimeError::NotImplemented(
            "Predicate only supports basic booleans".into(),
        )),
    }
}

// Recursively evaluates the predicate expression and returns a Value.
// In our case, any free variable (Predicate::Variable) is treated as the input value.
pub fn eval_predicate_expr(pred: &Predicate, input: &Value) -> Result<Value, RuntimeError> {
    match pred {
        Predicate::Value(val) => Ok(val.clone()),
        Predicate::Variable(_) => Ok(input.clone()),
        Predicate::BinaryExpr { lhs, op, rhs } => {
            let left = eval_predicate_expr(&lhs.value, input)?;
            let right = eval_predicate_expr(&rhs.value, input)?;
            apply_operator(&left, &op.value, &right)
        }
    }
}

// Applies a binary operator to two operand Values.
// Supports basic arithmetic and comparison operators.
// Returns a Value (numeric for arithmetic or Boolean for comparisons).
pub fn apply_operator(lhs: &Value, op: &Operator, rhs: &Value) -> Result<Value, RuntimeError> {
    use Value::*;

    match op {
        Operator::Add => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Int(a + b)),
            (Int(a), Float(b)) => Ok(Float((*a as f64) + b)),
            (Float(a), Int(b)) => Ok(Float(a + (*b as f64))),
            (Float(a), Float(b)) => Ok(Float(a + b)),
            _ => Err(RuntimeError::TypeError("Add operator requires numeric operands".into())),
        },

        Operator::Subtract => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Int(a - b)),
            (Int(a), Float(b)) => Ok(Float((*a as f64) - b)),
            (Float(a), Int(b)) => Ok(Float(a - (*b as f64))),
            (Float(a), Float(b)) => Ok(Float(a - b)),
            _ => Err(RuntimeError::TypeError("Subtract operator requires numeric operands".into())),
        },

        Operator::Multiply => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Int(a * b)),
            (Int(a), Float(b)) => Ok(Float((*a as f64) * b)),
            (Float(a), Int(b)) => Ok(Float(a * (*b as f64))),
            (Float(a), Float(b)) => Ok(Float(a * b)),
            _ => Err(RuntimeError::TypeError("Multiply operator requires numeric operands".into())),
        },

        Operator::Divide => {
            match (lhs, rhs) {
                (Int(_), Int(0))
                | (Float(_), Float(0.0))
                | (Int(_), Float(0.0))
                | (Float(_), Int(0)) => {
                    return Err(RuntimeError::TypeError("Division by zero".into()));
                }
                _ => {}
            }
            match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Int(a / b)),
                (Int(a), Float(b)) => Ok(Float((*a as f64) / b)),
                (Float(a), Int(b)) => Ok(Float(a / (*b as f64))),
                (Float(a), Float(b)) => Ok(Float(a / b)),
                _ => Err(RuntimeError::TypeError("Divide operator requires numeric operands".into())),
            }
        }

        Operator::Mod => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Int(a % b)),
            (Int(a), Float(b)) => Ok(Float((*a as f64) % b)),
            (Float(a), Int(b)) => Ok(Float(a % (*b as f64))),
            (Float(a), Float(b)) => Ok(Float(a % b)),
            _ => Err(RuntimeError::TypeError("Mod operator requires numeric operands".into())),
        },

        Operator::Equal => Ok(Boolean(lhs == rhs)),
        Operator::NotEqual => Ok(Boolean(lhs != rhs)),

        Operator::GreaterThan => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Boolean(a > b)),
            (Int(a), Float(b)) => Ok(Boolean((*a as f64) > *b)),
            (Float(a), Int(b)) => Ok(Boolean(*a > (*b as f64))),
            (Float(a), Float(b)) => Ok(Boolean(a > b)),
            _ => Err(RuntimeError::TypeError("GreaterThan operator requires numeric operands".into())),
        },

        Operator::LessThan => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Boolean(a < b)),
            (Int(a), Float(b)) => Ok(Boolean((*a as f64) < *b)),
            (Float(a), Int(b)) => Ok(Boolean(*a < (*b as f64))),
            (Float(a), Float(b)) => Ok(Boolean(a < b)),
            _ => Err(RuntimeError::TypeError("LessThan operator requires numeric operands".into())),
        },

        Operator::GreaterThanOrEqual => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Boolean(a >= b)),
            (Int(a), Float(b)) => Ok(Boolean((*a as f64) >= *b)),
            (Float(a), Int(b)) => Ok(Boolean(*a >= (*b as f64))),
            (Float(a), Float(b)) => Ok(Boolean(a >= b)),
            _ => Err(RuntimeError::TypeError("GreaterThanOrEqual operator requires numeric operands".into())),
        },

        Operator::LessThanOrEqual => match (lhs, rhs) {
            (Int(a), Int(b)) => Ok(Boolean(a <= b)),
            (Int(a), Float(b)) => Ok(Boolean((*a as f64) <= *b)),
            (Float(a), Int(b)) => Ok(Boolean(*a <= (*b as f64))),
            (Float(a), Float(b)) => Ok(Boolean(a <= b)),
            _ => Err(RuntimeError::TypeError("LessThanOrEqual operator requires numeric operands".into())),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Span, Spanned};

    fn dummy_span() -> Span {
        Span { file_id: 0, start: 0, end: 0 }
    }

    #[test]
    fn test_evaluate_predicate_literal() {
        let pred = Predicate::Value(Value::Boolean(true));
        let input = Value::Int(42);
        assert_eq!(evaluate_predicate(&pred, &input), Ok(true));
    }

    #[test]
    fn test_evaluate_predicate_variable() {
        let pred = Predicate::Variable("x".into());
        let input_true = Value::Boolean(true);
        assert_eq!(evaluate_predicate(&pred, &input_true), Ok(true));
        let input_false = Value::Boolean(false);
        assert_eq!(evaluate_predicate(&pred, &input_false), Ok(false));
    }

    #[test]
    fn test_evaluate_predicate_binary_expr() {
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".into()));
        let spanned_three = Spanned::new(dummy_span(), Predicate::Value(Value::Int(3)));
        let op = Spanned::new(dummy_span(), Operator::GreaterThan);
        let pred = Predicate::BinaryExpr { lhs: spanned_var, op, rhs: spanned_three };
        assert_eq!(evaluate_predicate(&pred, &Value::Int(10)), Ok(true));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(2)), Ok(false));
    }

    #[test]
    fn test_greater_than_or_equal() {
        // Test with integers
        assert_eq!(
            apply_operator(&Value::Int(5), &Operator::GreaterThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Int(6), &Operator::GreaterThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Int(4), &Operator::GreaterThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(false))
        );

        // Test with floats
        assert_eq!(
            apply_operator(&Value::Float(5.0), &Operator::GreaterThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Float(5.1), &Operator::GreaterThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Float(4.9), &Operator::GreaterThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(false))
        );

        // Test with mixed types
        assert_eq!(
            apply_operator(&Value::Int(5), &Operator::GreaterThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Float(5.0), &Operator::GreaterThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(true))
        );
    }

    #[test]
    fn test_less_than_or_equal() {
        // Test with integers
        assert_eq!(
            apply_operator(&Value::Int(5), &Operator::LessThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Int(4), &Operator::LessThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Int(6), &Operator::LessThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(false))
        );

        // Test with floats
        assert_eq!(
            apply_operator(&Value::Float(5.0), &Operator::LessThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Float(4.9), &Operator::LessThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Float(5.1), &Operator::LessThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(false))
        );

        // Test with mixed types
        assert_eq!(
            apply_operator(&Value::Int(5), &Operator::LessThanOrEqual, &Value::Float(5.0)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_operator(&Value::Float(5.0), &Operator::LessThanOrEqual, &Value::Int(5)),
            Ok(Value::Boolean(true))
        );
    }

    #[test]
    fn test_evaluate_predicate_with_greater_than_or_equal() {
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".into()));
        let spanned_five = Spanned::new(dummy_span(), Predicate::Value(Value::Int(5)));
        let op = Spanned::new(dummy_span(), Operator::GreaterThanOrEqual);
        let pred = Predicate::BinaryExpr { lhs: spanned_var, op, rhs: spanned_five };

        // Test with value greater than 5
        assert_eq!(evaluate_predicate(&pred, &Value::Int(10)), Ok(true));

        // Test with value equal to 5
        assert_eq!(evaluate_predicate(&pred, &Value::Int(5)), Ok(true));

        // Test with value less than 5
        assert_eq!(evaluate_predicate(&pred, &Value::Int(3)), Ok(false));
    }

    #[test]
    fn test_evaluate_predicate_with_less_than_or_equal() {
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".into()));
        let spanned_five = Spanned::new(dummy_span(), Predicate::Value(Value::Int(5)));
        let op = Spanned::new(dummy_span(), Operator::LessThanOrEqual);
        let pred = Predicate::BinaryExpr { lhs: spanned_var, op, rhs: spanned_five };

        // Test with value less than 5
        assert_eq!(evaluate_predicate(&pred, &Value::Int(3)), Ok(true));

        // Test with value equal to 5
        assert_eq!(evaluate_predicate(&pred, &Value::Int(5)), Ok(true));

        // Test with value greater than 5
        assert_eq!(evaluate_predicate(&pred, &Value::Int(10)), Ok(false));
    }
}
