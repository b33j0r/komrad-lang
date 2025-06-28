//! This module implements predicate evaluation for pattern matching in Komrad.
//!
//! Predicates are expressions that can be evaluated against an input value to determine
//! if a pattern matches. They can also extract variable bindings from the input.
//!
//! The module supports:
//! - Simple value matching (e.g., matching a literal value)
//! - Variable binding (e.g., capturing an input value in a variable)
//! - Binary expressions (e.g., arithmetic and comparison operations)
//! - Inverse operations for solving equations (e.g., if pattern is `x - 2`, and input is `3`, 
//!   then `x` is bound to `5`)

use crate::error::RuntimeError;
use crate::value::Value;
use crate::{Operator, Predicate};
use std::collections::HashMap;
use tracing::trace;

/// Evaluates a predicate against an input value and returns a boolean result and any variable bindings.
///
/// This is the top-level function for predicate evaluation. It expects the evaluated result 
/// to be a boolean value or an expression that produces bindings.
///
/// # Arguments
/// * `pred` - The predicate to evaluate
/// * `input` - The input value to evaluate against
///
/// # Returns
/// * `Ok((bool, HashMap<String, Value>))` - Whether the predicate matches and any variable bindings
/// * `Err(RuntimeError)` - If evaluation fails
pub fn evaluate_predicate(pred: &Predicate, input: &Value) -> Result<(bool, HashMap<String, Value>), RuntimeError> {
    trace!("Evaluating predicate: {:?} with input: {:?}", pred, input);

    let (val, bindings) = eval_predicate_expr(pred, input)?;
    match val {
        Value::Boolean(b) => {
            trace!("Predicate evaluated to: {:?}", b);
            Ok((b, bindings))
        }
        // For arithmetic expressions like (x - 2), we consider them a match if we successfully
        // computed the bindings, regardless of the actual value
        _ => {
            trace!("Predicate evaluated to non-boolean value: {:?}", val);
            // If we have bindings, consider it a match
            if !bindings.is_empty() {
                Ok((true, bindings))
            } else {
                Err(RuntimeError::NotImplemented(
                    "Predicate only supports basic booleans".into(),
                ))
            }
        }
    }
}

/// Recursively evaluates a predicate expression and returns the resulting value and any variable bindings.
///
/// This function handles the different types of predicates:
/// - Value literals (returning the value itself)
/// - Variables (binding the input to the variable name)
/// - Binary expressions (evaluating both sides and applying the operator)
///
/// For patterns like `x + 2`, this function will solve for `x` by applying the inverse operation.
///
/// # Arguments
/// * `pred` - The predicate to evaluate
/// * `input` - The input value to evaluate against
///
/// # Returns
/// * `Ok((Value, HashMap<String, Value>))` - The evaluated value and any variable bindings
/// * `Err(RuntimeError)` - If evaluation fails
pub fn eval_predicate_expr(pred: &Predicate, input: &Value) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
    match pred {
        Predicate::Value(v) => {
            if let Value::Boolean(b) = v {
                // For boolean literals, return the boolean value itself
                Ok((Value::Boolean(*b), HashMap::new()))
            } else {
                // Return non-boolean literals as-is
                Ok((v.clone(), HashMap::new()))
            }
        }
        Predicate::Variable(name) => {
            // Bind the input value to the variable name
            let mut bindings = HashMap::new();
            bindings.insert(name.clone(), input.clone());
            Ok((input.clone(), bindings))
        }
        Predicate::BinaryExpr { lhs, op, rhs } => {
            // Handle pattern like (x op literal) - variable on left
            if let Predicate::Variable(var_name) = &*lhs.value {
                if let Predicate::Value(val) = &*rhs.value {
                    let solved_value = apply_inverse_operator(input, &op.value, val)?;

                    let mut bindings = HashMap::new();

                    // For boolean results, we need to extract the actual variable value
                    if let Value::Boolean(false) = solved_value {
                        // For comparison operators, we still want to bind the variable
                        // to the input value even if the comparison is false
                        bindings.insert(var_name.clone(), input.clone());
                        return Ok((Value::Boolean(false), bindings));
                    }

                    bindings.insert(var_name.clone(), solved_value.clone());
                    return Ok((input.clone(), bindings));
                }
            }
            // Handle pattern like (literal op x) - variable on right
            else if let Predicate::Variable(var_name) = &*rhs.value {
                if let Predicate::Value(val) = &*lhs.value {
                    let solved_value = apply_inverse_operator_right_var(val, &op.value, input)?;

                    let mut bindings = HashMap::new();

                    // For boolean results, we need to extract the actual variable value
                    if let Value::Boolean(false) = solved_value {
                        // For comparison operators, we still want to bind the variable
                        // to the input value even if the comparison is false
                        bindings.insert(var_name.clone(), input.clone());
                        return Ok((Value::Boolean(false), bindings));
                    }

                    bindings.insert(var_name.clone(), solved_value.clone());
                    return Ok((input.clone(), bindings));
                }
            }

            // For general binary expressions, evaluate both sides
            let (left_val, mut left_bindings) = eval_predicate_expr(&lhs.value, input)?;
            let (right_val, right_bindings) = eval_predicate_expr(&rhs.value, input)?;

            left_bindings.extend(right_bindings);
            let result = apply_operator(&left_val, &op.value, &right_val)?;

            Ok((result, left_bindings))
        }
    }
}

/// Applies the inverse of a binary operator to solve for a variable in a pattern.
///
/// This function is used when we have a pattern like `(x op literal) = input` and need to solve for `x`.
/// It applies the appropriate inverse operation based on the operator type.
///
/// # Examples
/// - For addition: if `(x + 2) = 5`, then `x = 5 - 2 = 3`
/// - For subtraction: if `(x - 2) = 3`, then `x = 3 + 2 = 5`
/// - For multiplication: if `(x * 2) = 6`, then `x = 6 / 2 = 3`
/// - For division: if `(x / 2) = 3`, then `x = 3 * 2 = 6`
/// - For comparison: if `(x == 5)` and input is `5`, then `x = 5`
///
/// # Arguments
/// * `input` - The input value (right side of the equation)
/// * `op` - The operator in the pattern
/// * `literal` - The literal value in the pattern (right side of the operator)
///
/// # Returns
/// * `Ok(Value)` - The solved value for the variable
/// * `Err(RuntimeError)` - If the inverse operation cannot be performed
pub fn apply_inverse_operator(input: &Value, op: &Operator, literal: &Value) -> Result<Value, RuntimeError> {
    use Value::*;

    tracing::trace!("apply_inverse_operator called with input: {:?}, op: {:?}, literal: {:?}", input, op, literal);

    match op {
        // Addition: (x + literal) = input -> x = input - literal
        Operator::Add => match (input, literal) {
            (Int(input_val), Int(lit_val)) => Ok(Int(input_val - lit_val)),
            (Int(input_val), Float(lit_val)) => Ok(Float((*input_val as f64) - lit_val)),
            (Float(input_val), Int(lit_val)) => Ok(Float(input_val - (*lit_val as f64))),
            (Float(input_val), Float(lit_val)) => Ok(Float(input_val - lit_val)),
            _ => Err(RuntimeError::TypeError("Add inverse operation requires numeric operands".into())),
        },

        // Subtraction: (x - literal) = input -> x = input + literal
        Operator::Subtract => match (input, literal) {
            (Int(input_val), Int(lit_val)) => Ok(Int(input_val + lit_val)),
            (Int(input_val), Float(lit_val)) => Ok(Float((*input_val as f64) + lit_val)),
            (Float(input_val), Int(lit_val)) => Ok(Float(input_val + (*lit_val as f64))),
            (Float(input_val), Float(lit_val)) => Ok(Float(input_val + lit_val)),
            _ => Err(RuntimeError::TypeError("Subtract inverse operation requires numeric operands".into())),
        },

        // Multiplication: (x * literal) = input -> x = input / literal
        Operator::Multiply => match (input, literal) {
            // Check for division by zero
            (Int(_), Int(0)) | (Float(_), Int(0)) | (Int(_), Float(0.0)) | (Float(_), Float(0.0)) => {
                Err(RuntimeError::TypeError("Division by zero in inverse operation".into()))
            }
            (Int(input_val), Int(lit_val)) => Ok(Int(input_val / lit_val)),
            (Int(input_val), Float(lit_val)) => Ok(Float((*input_val as f64) / lit_val)),
            (Float(input_val), Int(lit_val)) => Ok(Float(input_val / (*lit_val as f64))),
            (Float(input_val), Float(lit_val)) => Ok(Float(input_val / lit_val)),
            _ => Err(RuntimeError::TypeError("Multiply inverse operation requires numeric operands".into())),
        },

        // Division: (x / literal) = input -> x = input * literal
        Operator::Divide => match (input, literal) {
            (Int(input_val), Int(lit_val)) => Ok(Int(input_val * lit_val)),
            (Int(input_val), Float(lit_val)) => Ok(Float((*input_val as f64) * lit_val)),
            (Float(input_val), Int(lit_val)) => Ok(Float(input_val * (*lit_val as f64))),
            (Float(input_val), Float(lit_val)) => Ok(Float(input_val * lit_val)),
            _ => Err(RuntimeError::TypeError("Divide inverse operation requires numeric operands".into())),
        },

        // Equality: (x == literal) = input -> x = literal if input == literal
        Operator::Equal => {
            if input == literal {
                Ok(literal.clone())
            } else {
                // Return false to indicate non-match without generating an error
                Ok(Value::Boolean(false))
            }
        }

        // Inequality: (x != literal) = input -> x = input if input != literal
        Operator::NotEqual => {
            if input != literal {
                Ok(input.clone())
            } else {
                Ok(Value::Boolean(false))
            }
        }

        // Comparison operators check if the input value satisfies the comparison
        Operator::GreaterThan => match (input, literal) {
            (Int(input_val), Int(lit_val)) => {
                if input_val > lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(input_val), Float(lit_val)) => {
                if (*input_val as f64) > *lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Int(lit_val)) => {
                if *input_val > (*lit_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Float(lit_val)) => {
                if input_val > lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("GreaterThan inverse operation requires numeric operands".into())),
        },

        Operator::LessThan => match (input, literal) {
            (Int(input_val), Int(lit_val)) => {
                if input_val < lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(input_val), Float(lit_val)) => {
                if (*input_val as f64) < *lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Int(lit_val)) => {
                if *input_val < (*lit_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Float(lit_val)) => {
                if input_val < lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("LessThan inverse operation requires numeric operands".into())),
        },

        Operator::GreaterThanOrEqual => match (input, literal) {
            (Int(input_val), Int(lit_val)) => {
                if input_val >= lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(input_val), Float(lit_val)) => {
                if (*input_val as f64) >= *lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Int(lit_val)) => {
                if *input_val >= (*lit_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Float(lit_val)) => {
                if input_val >= lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("GreaterThanOrEqual inverse operation requires numeric operands".into())),
        },

        Operator::LessThanOrEqual => match (input, literal) {
            (Int(input_val), Int(lit_val)) => {
                if input_val <= lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(input_val), Float(lit_val)) => {
                if (*input_val as f64) <= *lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Int(lit_val)) => {
                if *input_val <= (*lit_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(input_val), Float(lit_val)) => {
                if input_val <= lit_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("LessThanOrEqual inverse operation requires numeric operands".into())),
        },

        // Other operators don't have meaningful inverse operations
        _ => Err(RuntimeError::NotImplemented(
            format!("Inverse operation not implemented for operator {:?}", op).into(),
        )),
    }
}

/// Applies the inverse of a binary operator to solve for a variable on the right side of an operator.
///
/// This function is used when we have a pattern like `(literal op x) = input` and need to solve for `x`.
/// It handles the case where the variable is on the right side of the operator, which requires
/// different inverse operations compared to when the variable is on the left.
///
/// # Examples
/// - For addition: if `(2 + x) = 5`, then `x = 5 - 2 = 3`
/// - For subtraction: if `(2 - x) = 3`, then `x = 2 - 3 = -1`
/// - For multiplication: if `(2 * x) = 6`, then `x = 6 / 2 = 3`
/// - For division: if `(2 / x) = 0.5`, then `x = 2 / 0.5 = 4`
///
/// # Arguments
/// * `literal` - The literal value in the pattern (left side of the operator)
/// * `op` - The operator in the pattern
/// * `input` - The input value (right side of the equation)
///
/// # Returns
/// * `Ok(Value)` - The solved value for the variable
/// * `Err(RuntimeError)` - If the inverse operation cannot be performed
pub fn apply_inverse_operator_right_var(literal: &Value, op: &Operator, input: &Value) -> Result<Value, RuntimeError> {
    use Value::*;

    match op {
        Operator::Add => match (literal, input) {
            // For (literal + x) = input, solve for x: x = input - literal
            (Int(lit_val), Int(input_val)) => Ok(Int(input_val - lit_val)),
            (Int(lit_val), Float(input_val)) => Ok(Float(input_val - (*lit_val as f64))),
            (Float(lit_val), Int(input_val)) => Ok(Float(lit_val - (*input_val as f64))),
            (Float(lit_val), Float(input_val)) => Ok(Float(input_val - lit_val)),
            _ => Err(RuntimeError::TypeError("Add inverse operation requires numeric operands".into())),
        },

        Operator::Subtract => match (literal, input) {
            // For (literal - x) = input, solve for x: x = literal - input
            (Int(lit_val), Int(input_val)) => Ok(Int(lit_val - input_val)),
            (Int(lit_val), Float(input_val)) => Ok(Float((*lit_val as f64) - input_val)),
            (Float(lit_val), Int(input_val)) => Ok(Float(lit_val - (*input_val as f64))),
            (Float(lit_val), Float(input_val)) => Ok(Float(lit_val - input_val)),
            _ => Err(RuntimeError::TypeError("Subtract inverse operation requires numeric operands".into())),
        },

        Operator::Multiply => match (literal, input) {
            // For (literal * x) = input, solve for x: x = input / literal
            (Int(0), _) | (Float(0.0), _) => {
                Err(RuntimeError::TypeError("Division by zero in inverse operation".into()))
            }
            (Int(lit_val), Int(input_val)) => Ok(Int(input_val / lit_val)),
            (Int(lit_val), Float(input_val)) => Ok(Float(input_val / (*lit_val as f64))),
            (Float(lit_val), Int(input_val)) => Ok(Float((*input_val as f64) / lit_val)),
            (Float(lit_val), Float(input_val)) => Ok(Float(input_val / lit_val)),
            _ => Err(RuntimeError::TypeError("Multiply inverse operation requires numeric operands".into())),
        },

        Operator::Divide => match (literal, input) {
            // For (literal / x) = input, solve for x: x = literal / input
            (_, Int(0)) | (_, Float(0.0)) => {
                Err(RuntimeError::TypeError("Division by zero in inverse operation".into()))
            }
            (Int(lit_val), Int(input_val)) => Ok(Int(lit_val / input_val)),
            (Int(lit_val), Float(input_val)) => Ok(Float((*lit_val as f64) / input_val)),
            (Float(lit_val), Int(input_val)) => Ok(Float(lit_val / (*input_val as f64))),
            (Float(lit_val), Float(input_val)) => Ok(Float(lit_val / input_val)),
            _ => Err(RuntimeError::TypeError("Divide inverse operation requires numeric operands".into())),
        },

        // For comparison operators, we need to check if the input satisfies the condition
        // If it does, we return the input value as the binding for the variable
        Operator::Equal => {
            if literal == input {
                Ok(input.clone())
            } else {
                // If the literal doesn't equal the input, the pattern doesn't match
                // Return a boolean false value to indicate a non-match
                // This will be treated as a non-match without generating an error
                Ok(Value::Boolean(false))
            }
        }

        Operator::NotEqual => {
            if literal != input {
                // For not equal, any value that's not equal to the literal is valid
                // We return the input as the binding
                Ok(input.clone())
            } else {
                // If the literal equals the input, the pattern doesn't match
                Ok(Value::Boolean(false))
            }
        }

        Operator::GreaterThan => match (literal, input) {
            (Int(lit_val), Int(input_val)) => {
                if lit_val > input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(lit_val), Float(input_val)) => {
                if (*lit_val as f64) > *input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Int(input_val)) => {
                if *lit_val > (*input_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Float(input_val)) => {
                if lit_val > input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("GreaterThan inverse operation requires numeric operands".into())),
        },

        Operator::LessThan => match (literal, input) {
            (Int(lit_val), Int(input_val)) => {
                if lit_val < input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(lit_val), Float(input_val)) => {
                if (*lit_val as f64) < *input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Int(input_val)) => {
                if *lit_val < (*input_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Float(input_val)) => {
                if lit_val < input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("LessThan inverse operation requires numeric operands".into())),
        },

        Operator::GreaterThanOrEqual => match (literal, input) {
            (Int(lit_val), Int(input_val)) => {
                if lit_val >= input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(lit_val), Float(input_val)) => {
                if (*lit_val as f64) >= *input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Int(input_val)) => {
                if *lit_val >= (*input_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Float(input_val)) => {
                if lit_val >= input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("GreaterThanOrEqual inverse operation requires numeric operands".into())),
        },

        Operator::LessThanOrEqual => match (literal, input) {
            (Int(lit_val), Int(input_val)) => {
                if lit_val <= input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Int(lit_val), Float(input_val)) => {
                if (*lit_val as f64) <= *input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Int(input_val)) => {
                if *lit_val <= (*input_val as f64) {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            (Float(lit_val), Float(input_val)) => {
                if lit_val <= input_val {
                    Ok(input.clone())
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => Err(RuntimeError::TypeError("LessThanOrEqual inverse operation requires numeric operands".into())),
        },

        // For other operators, we don't have a meaningful inverse operation
        _ => Err(RuntimeError::NotImplemented(
            format!("Inverse operation not implemented for operator {:?}", op).into(),
        )),
    }
}

/// Applies a binary operator to two values.
///
/// This function evaluates expressions like `lhs op rhs` where `op` is a binary operator.
/// It supports arithmetic operations (add, subtract, multiply, divide, modulo) and
/// comparison operations (equal, not equal, greater than, less than, etc.).
///
/// # Arguments
/// * `lhs` - The left-hand side value
/// * `op` - The operator to apply
/// * `rhs` - The right-hand side value
///
/// # Returns
/// * `Ok(Value)` - The result of applying the operator
/// * `Err(RuntimeError)` - If the operation cannot be performed (e.g., type mismatch, division by zero)
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
        let mut expected_bindings = HashMap::new();
        assert_eq!(evaluate_predicate(&pred, &input), Ok((true, expected_bindings)));
    }

    #[test]
    fn test_evaluate_predicate_variable() {
        let pred = Predicate::Variable("x".into());
        let input_true = Value::Boolean(true);
        let mut expected_bindings_true = HashMap::new();
        expected_bindings_true.insert("x".to_string(), input_true.clone());
        assert_eq!(evaluate_predicate(&pred, &input_true), Ok((true, expected_bindings_true)));

        let input_false = Value::Boolean(false);
        let mut expected_bindings_false = HashMap::new();
        expected_bindings_false.insert("x".to_string(), input_false.clone());
        assert_eq!(evaluate_predicate(&pred, &input_false), Ok((false, expected_bindings_false)));
    }

    #[test]
    fn test_evaluate_predicate_binary_expr() {
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".into()));
        let spanned_three = Spanned::new(dummy_span(), Predicate::Value(Value::Int(3)));
        let op = Spanned::new(dummy_span(), Operator::GreaterThan);
        let pred = Predicate::BinaryExpr { lhs: spanned_var, op, rhs: spanned_three };

        let mut expected_bindings_10 = HashMap::new();
        expected_bindings_10.insert("x".to_string(), Value::Int(10));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(10)), Ok((true, expected_bindings_10)));

        let mut expected_bindings_2 = HashMap::new();
        expected_bindings_2.insert("x".to_string(), Value::Int(2));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(2)), Ok((false, expected_bindings_2)));
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
        let mut expected_bindings_10 = HashMap::new();
        expected_bindings_10.insert("x".to_string(), Value::Int(10));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(10)), Ok((true, expected_bindings_10)));

        // Test with value equal to 5
        let mut expected_bindings_5 = HashMap::new();
        expected_bindings_5.insert("x".to_string(), Value::Int(5));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(5)), Ok((true, expected_bindings_5)));

        // Test with value less than 5
        let mut expected_bindings_3 = HashMap::new();
        expected_bindings_3.insert("x".to_string(), Value::Int(3));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(3)), Ok((false, expected_bindings_3)));
    }

    #[test]
    fn test_evaluate_predicate_with_less_than_or_equal() {
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".into()));
        let spanned_five = Spanned::new(dummy_span(), Predicate::Value(Value::Int(5)));
        let op = Spanned::new(dummy_span(), Operator::LessThanOrEqual);
        let pred = Predicate::BinaryExpr { lhs: spanned_var, op, rhs: spanned_five };

        // Test with value less than 5
        let mut expected_bindings_3 = HashMap::new();
        expected_bindings_3.insert("x".to_string(), Value::Int(3));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(3)), Ok((true, expected_bindings_3)));

        // Test with value equal to 5
        let mut expected_bindings_5 = HashMap::new();
        expected_bindings_5.insert("x".to_string(), Value::Int(5));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(5)), Ok((true, expected_bindings_5)));

        // Test with value greater than 5
        let mut expected_bindings_10 = HashMap::new();
        expected_bindings_10.insert("x".to_string(), Value::Int(10));
        assert_eq!(evaluate_predicate(&pred, &Value::Int(10)), Ok((false, expected_bindings_10)));
    }
}
