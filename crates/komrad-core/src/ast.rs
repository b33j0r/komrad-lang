use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use thiserror::Error;
use crate::Channel;

pub enum TopLevel {
    Statement(Statement),
    Block(Block),
}

/// Represents a span of source code. Placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file_id: usize,
    pub start: usize,
    pub end: usize,
}

impl Default for Span {
    fn default() -> Self {
        Span { file_id: 0, start: 0, end: 0 }
    }
}

/// A convenience wrapper to keep track of a node along with its source location.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T, S = Span> {
    pub span: S,
    pub value: Box<T>,
}

impl<T> Spanned<T> {
    /// Construct a new Spanned<T>.
    pub fn new(span: Span, value: T) -> Self {
        Spanned {
            span,
            value: Box::new(value),
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value.deref()
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value.deref_mut()
    }
}

#[derive(Debug, Clone, Error, PartialEq)]
pub enum RuntimeError {
    #[error("Syntax error: {0}")]
    SyntaxError(String),

    #[error("Argument error: {0}")]
    ArgumentError(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Unknown error: {0}")]
    UnknownError(String),

    #[error("Unknown operator: {0}")]
    UnknownOperator(String),

    #[error("Unknown variable: {0}")]
    UnknownVariable(String),

    #[error("Pattern match error: {0}")]
    PatternMatchError(String),

    #[error("Channel error: {0}")]
    ChannelError(String),
}

impl RuntimeError {
    pub(crate) fn as_spanned(&self, span: Span) -> Spanned<RuntimeError> {
        Spanned::new(span, self.clone())
    }
}

/// A simple wrapper for a series of statements, each possibly with its own span.
#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Spanned<Statement>>);

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Error(Spanned<RuntimeError>),
    /// Represents a channel for sending messages (assume external definition).
    Channel(crate::channel::Channel),
    /// Represents a list of values.
    List(Vec<Value>),
    /// Represents a symbol or identifier.
    Word(String),
    Boolean(bool),
    String(String),
    Int(i64),
    Float(f64),
    Uuid(uuid::Uuid),
    /// Represents a sequence of statements, often used as a closure or handler body.
    Block(Arc<Block>),
}

impl From<Channel> for Value {
    fn from(c: Channel) -> Self {
        Value::Channel(c)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_string())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl TryFrom<Value> for bool {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(b),
            _ => Err(RuntimeError::TypeError("Expected a boolean".to_string())),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i),
            _ => Err(RuntimeError::TypeError("Expected an integer".to_string())),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Float(f) => Ok(f),
            _ => Err(RuntimeError::TypeError("Expected a float".to_string())),
        }
    }
}


impl TryFrom<Value> for String {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err(RuntimeError::TypeError("Expected a string".to_string())),
        }
    }
}

impl TryFrom<Value> for Channel {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Channel(c) => Ok(c),
            _ => Err(RuntimeError::TypeError("Expected a channel".to_string())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Null,
    Int,
    Float,
    String,
    Boolean,
    List,
    Channel,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A literal or direct value (e.g. number, string, boolean, block, etc.)
    Value(Spanned<Value>),

    /// Send that evaluates to a reply value (e.g., `target ? value`).
    Ask {
        target: Spanned<Expr>,
        value: Spanned<Expr>,
    },

    /// A list expression, each element spanned.
    List {
        elements: Vec<Spanned<Expr>>,
    },

    /// A binary operation (e.g., `lhs + rhs`).
    BinaryExpr {
        lhs: Spanned<Expr>,
        op: Spanned<Operator>,
        rhs: Spanned<Expr>,
    },

    /// An indexing/slicing expression (e.g. `expr[x]`).
    SliceExpr {
        // The target expression to slice.
        target: Spanned<Expr>,

        // The expression inside the slice brackets.
        index: Spanned<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Evaluates an expression without binding the result (e.g. `123` -> `123`).
    Expr(Expr),

    /// Binds the value of an expression to a target.
    Assign {
        target: Spanned<AssignmentTarget>,
        type_name: Option<Spanned<Type>>,
        value: Spanned<Expr>,
    },

    /// Send that does not evaluate to a reply value.
    Tell {
        target: Spanned<Expr>,
        value: Spanned<Expr>,
    },

    /// A pattern/expr pair
    Handler(Arc<Handler>),

    /// Turns a list into a call or a block into an evaluated result in the outer scope.
    Expand {
        target: Spanned<Expr>,
    },

    /// A block that is invalid or was parsed incorrectly.
    InvalidBlock,
}

/// Represents a handler with a pattern and an expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Handler {
    pub pattern: Spanned<Pattern>,
    pub expr: Spanned<Expr>,
}

/// Represents the target of an assignment.
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    Variable(String),
    Slice {
        target: Spanned<AssignmentTarget>,
        index: Spanned<Expr>,
    },
    List {
        elements: Vec<Spanned<AssignmentTarget>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Operator::*;
        // Define precedence for operators
        let precedence = match self {
            Add | Subtract => 1,
            Multiply | Divide => 2,
            Equal | NotEqual | GreaterThan | LessThan => 3,
        };
        let other_precedence = match other {
            Add | Subtract => 1,
            Multiply | Divide => 2,
            Equal | NotEqual | GreaterThan | LessThan => 3,
        };
        precedence.partial_cmp(&other_precedence)
    }
}

/// A simplified expression used for pattern-based predicates (no sends or blocks).
#[derive(Debug, Clone, PartialEq)]
pub enum Predicate {
    Value(Value),
    Variable(String),
    BinaryExpr {
        lhs: Spanned<Predicate>,
        op: Spanned<Operator>,
        rhs: Spanned<Predicate>,
    },
}

/// Pattern captures or matches on certain shapes of incoming values or messages.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    ValueMatch(Value),
    VariableCapture(String),
    BlockCapture(String),
    PredicateCapture(Spanned<Predicate>),
    List(Vec<Spanned<Pattern>>),
}

impl Pattern {
    #[cfg(test)]
    pub(crate) fn new_word(word: String) -> Spanned<Pattern> {
        Spanned::new(
            Span { file_id: 0, start: 0, end: word.len() },
            Pattern::VariableCapture(word),
        )
    }

    #[cfg(test)]
    pub(crate) fn new_block_capture(name: String) -> Spanned<Pattern> {
        Spanned::new(
            Span { file_id: 0, start: 0, end: name.len() },
            Pattern::BlockCapture(name),
        )
    }

    #[cfg(test)]
    pub(crate) fn new_predicate_capture(pred: Predicate) -> Spanned<Pattern> {
        Spanned::new(
            Span { file_id: 0, start: 0, end: 1 },
            Pattern::PredicateCapture(Spanned::new(
                Span { file_id: 0, start: 0, end: 1 },
                pred,
            )),
        )
    }

    #[cfg(test)]
    pub(crate) fn new_list(patterns: Vec<Spanned<Pattern>>) -> Spanned<Pattern> {
        Spanned::new(
            Span { file_id: 0, start: 0, end: 1 },
            Pattern::List(patterns),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Block, Pattern, Span, Spanned, Statement};
    use super::*;

    #[test]
    fn test_spanned_construction() {
        let span = Span { file_id: 1, start: 0, end: 5 };
        let expr = Expr::Value(Spanned::new(span.clone(), Value::Int(10)));
        let sp_expr = Spanned::new(span.clone(), expr.clone());
        assert_eq!(sp_expr.span, span);
        assert_eq!(*sp_expr, expr);
    }

    #[test]
    fn test_block_is_stored() {
        let block = Block(vec![
            Spanned::new(
                Span { file_id: 1, start: 0, end: 1 },
                Statement::InvalidBlock
            )
        ]);
        let val = Value::Block(Arc::new(block));
        match val {
            Value::Block(b) => {
                assert_eq!(b.0.len(), 1);
            }
            _ => panic!("Expected Value::Block"),
        }
    }

    #[test]
    fn test_pattern_capture() {
        let span = Span { file_id: 1, start: 10, end: 12 };
        let pat = Pattern::VariableCapture("x".to_string());
        let sp_pat = Spanned::new(span.clone(), pat.clone());
        assert_eq!(sp_pat.span, span);
        assert_eq!(*sp_pat, pat);
    }
}