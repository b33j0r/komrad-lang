use crate::error::RuntimeError;
use crate::value::Value;

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