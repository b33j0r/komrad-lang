use crate::ast::Pattern;
use crate::error::RuntimeError;
use crate::value::Value;
use crate::{Expr, Operator, Predicate, Spanned, Type};
use std::collections::HashMap;

/// Match result type for destructuring:
/// - `Match(O)` means successful destructuring
/// - `NoMatch` is a soft failure (e.g. for handler filters)
/// - `Err(E)` is a hard error (e.g. invalid predicate logic)
#[derive(Debug, Clone, PartialEq)]
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
                if &**expected == actual {
                    DestructureResult::Match(bindings)
                } else {
                    DestructureResult::NoMatch
                }
            }

            // Capture a variable
            (Pattern::VariableCapture(name), value) => {
                bindings.insert(*name.value.clone(), value.clone());
                DestructureResult::Match(bindings)
            }

            // Capture a block if it's actually a block
            (Pattern::BlockCapture(name), value @ Value::Block(_)) => {
                bindings.insert(*name.value.clone(), value.clone());
                DestructureResult::Match(bindings)
            }
            (Pattern::BlockCapture(_), _) => DestructureResult::NoMatch,

            // Match a predicate like _(x > 3)
            (Pattern::PredicateCapture(pred), value) => match evaluate_predicate(&pred.value, value) {
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
                    match Self::destructure(&p.value, v) {
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
        Predicate::Value(v) => Ok(v.clone()),
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
fn apply_operator(lhs: &Value, op: &Operator, rhs: &Value) -> Result<Value, RuntimeError> {
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
    }
}


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
            crate::ast::AssignmentTarget::Variable(name) => {
                DestructureResult::Match(vec![AssignmentAction::AssignVariable {
                    name: name.clone(),
                    value: input.clone(),
                }])
            }
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
            crate::ast::AssignmentTarget::Slice { .. } => {
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
        if let Value::List(vals) = input {
            if vals.len() < 1 {
                return DestructureResult::NoMatch;
            }
            let cmd_name = match &vals[0] {
                Value::String(s) => s.clone(),
                _ => return DestructureResult::NoMatch,
            };
            if cmd_name != target.name {
                return DestructureResult::NoMatch;
            }
            let args = vals[1..]
                .iter()
                .zip(&target.args)
                .map(|(val, ty)| match val {
                    Value::Int(_) if *ty == Type::Int => Ok(val.clone()),
                    Value::Float(_) if *ty == Type::Float => Ok(val.clone()),
                    Value::String(_) if *ty == Type::String => Ok(val.clone()),
                    _ => Err(RuntimeError::TypeError(format!(
                        "Expected argument of type {:?}, got {:?}",
                        ty, val
                    ))),
                })
                .collect::<Result<Vec<_>, _>>();
            match args {
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
mod tests_command {
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

// -----------------------------------------------------------------
// Tests for AssignmentDestructure
// -----------------------------------------------------------------

#[cfg(test)]
mod tests_assignment {
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
        assert_eq!(result, DestructureResult::NoMatch);
    }
}

#[cfg(test)]
mod tests_pattern_destructure {
    use super::*;
    use crate::{AsSpanned, Span};

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
    fn test_destructure_value_match() {
        let pattern = Pattern::ValueMatch(Value::Int(10).as_spanned(
            dummy_span(),
        ));
        assert!(matches!(PatternDestructure::destructure(&pattern, &Value::Int(10)), DestructureResult::Match(_)));
        assert_eq!(PatternDestructure::destructure(&pattern, &Value::Int(11)), DestructureResult::NoMatch);
    }

    #[test]
    fn test_destructure_variable_capture() {
        let pattern = Pattern::VariableCapture("x".to_string().as_spanned(Span { file_id: 0, start: 0, end: 0 }));
        let input = Value::String("hello".into());
        let result = PatternDestructure::destructure(&pattern, &input);
        if let DestructureResult::Match(bindings) = result {
            assert_eq!(bindings.get("x"), Some(&input));
        } else {
            panic!("Expected a Match for variable capture");
        }
    }

    #[test]
    fn test_destructure_list() {
        let p1 = Spanned::new(dummy_span(), Pattern::VariableCapture("a".to_string().as_spanned(Span { file_id: 0, start: 0, end: 0 })));
        let p2 = Spanned::new(dummy_span(), Pattern::ValueMatch(Value::Int(20).as_spanned(dummy_span())));
        let pattern = Pattern::List(vec![p1, p2]);
        let input_ok = Value::List(vec![Value::Int(10), Value::Int(20)]);
        let input_bad = Value::List(vec![Value::Int(10)]);
        assert!(matches!(PatternDestructure::destructure(&pattern, &input_ok), DestructureResult::Match(_)));
        assert_eq!(PatternDestructure::destructure(&pattern, &input_bad), DestructureResult::NoMatch);
    }

    #[test]
    fn test_destructure_predicate_capture() {
        let spanned_var = Spanned::new(dummy_span(), Predicate::Variable("x".to_string()));
        let spanned_five = Spanned::new(dummy_span(), Predicate::Value(Value::Int(5)));
        let op = Spanned::new(dummy_span(), Operator::GreaterThan);
        let pred = Spanned::new(dummy_span(), Predicate::BinaryExpr { lhs: spanned_var, op, rhs: spanned_five });
        let pattern = Pattern::PredicateCapture(pred);
        assert_eq!(PatternDestructure::destructure(&pattern, &Value::Int(10)), DestructureResult::Match(HashMap::new()));
        assert_eq!(PatternDestructure::destructure(&pattern, &Value::Int(3)), DestructureResult::NoMatch);
    }
}
