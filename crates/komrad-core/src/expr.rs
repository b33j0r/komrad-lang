use std::ops::{Deref, DerefMut};
use thiserror::Error;

pub enum TopLevel {
    Expr(Expr),
    Block(Block),
}

/// Represents a span of source code. Placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file_id: usize,
    pub start: usize,
    pub end: usize,
}

/// A convenience wrapper to keep track of a node along with its source location.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T, S=Span> {
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
    Block(Block),
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
    /// Evaluates an expression without binding the result (e.g. `expr;`).
    Expr(Expr),

    /// Binds the value of an expression to a target (e.g. `let x = expr;`).
    Assign {
        target: Spanned<AssignmentTarget>,
        type_name: Option<Spanned<Type>>,
        value: Spanned<Expr>,
    },

    /// Send that does not evaluate to a reply value (e.g., `target ! value;`).
    Tell {
        target: Spanned<Expr>,
        value: Spanned<Expr>,
    },

    /// A block with a pattern-based handler within an assignment statement (or top-level).
    Handler {
        /// e.g. `[a 2 _b _(x>3) _{c}] => expression`
        pattern: Spanned<Pattern>,
        handler: Spanned<Expr>,
    },

    /// Turns a list into a call or a block into an evaluated result
    Expand {
        target: Spanned<Expr>,
    },

    /// A block that is invalid or was parsed incorrectly.
    InvalidBlock,
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

#[cfg(test)]
mod tests {
    use crate::expr::{Block, Pattern, Span, Spanned, Statement};
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
        let val = Value::Block(block);
        match val {
            Value::Block(b) => {
                assert_eq!(b.0.len(), 1);
            },
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