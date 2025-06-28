use crate::ast::Pattern;
use crate::destructure::base::Destructure;
use crate::destructure::base::DestructureResult;
use crate::destructure::predicate::evaluate_predicate;
use crate::value::Value;
use std::collections::HashMap;

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
                Ok((true, pred_bindings)) => {
                    // Add the bindings from the predicate evaluation
                    bindings.extend(pred_bindings);
                    DestructureResult::Match(bindings)
                },
                Ok((false, _)) => DestructureResult::NoMatch,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{AsSpanned, Operator, Predicate, Span, Spanned};

    fn dummy_span() -> Span {
        Span { file_id: 0, start: 0, end: 0 }
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

        // For value 10, the predicate x > 5 is true, and x should be bound to 10
        let mut expected_bindings_10 = HashMap::new();
        expected_bindings_10.insert("x".to_string(), Value::Int(10));
        assert_eq!(PatternDestructure::destructure(&pattern, &Value::Int(10)), DestructureResult::Match(expected_bindings_10));

        // For value 3, the predicate x > 5 is false
        assert_eq!(PatternDestructure::destructure(&pattern, &Value::Int(3)), DestructureResult::NoMatch);
    }
}
