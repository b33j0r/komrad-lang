use crate::value::Value;
use indexmap::IndexMap;
use nom::error::{FromExternalError, ParseError as NomParseError};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
pub enum TopLevel {
    Statement(Spanned<Statement>),
    Block(Block),
}

/// Represents a span of source code. Placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file_id: usize,
    pub start: usize,
    pub end: usize,
}

#[cfg(test)]
impl Default for Span {
    fn default() -> Self {
        Span {
            file_id: 0,
            start: 0,
            end: 0,
        }
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

pub trait AsSpanned<T> {
    fn as_spanned(self, span: Span) -> Spanned<T>;
}

impl<T> AsSpanned<T> for T {
    fn as_spanned(self, span: Span) -> Spanned<T> {
        Spanned::new(span, self)
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

/// A simple wrapper for a series of statements, each possibly with its own span.
#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Spanned<Statement>>);

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
    List { elements: Vec<Spanned<Expr>> },

    /// A dictionary expression, each key-value pair spanned.
    Dict {
        index_map: IndexMap<String, Spanned<Expr>>,
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
    BlankLine,

    Comment(String),

    /// Evaluates an expression without binding the result (e.g. `123` -> `123`).
    Expr(Spanned<Expr>),

    /// Binds the value of an expression to a target.
    Assign {
        target: Spanned<AssignmentTarget>,
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
    Mod,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
}

/// Represents the associativity of an operator.
///
/// Associativity determines how operators of the same precedence are grouped
/// in the absence of parentheses. For example, in the expression `a - b - c`,
/// the subtraction operator is left associative, so it is grouped as `(a - b) - c`.
///
/// In the parser, this works by checking the associativity of the operator when
/// parsing expressions. If two operators have the same precedence, the associativity
/// is used to determine which operator to apply first as follows:
///
/// - Left associative: The leftmost operator is applied first.
/// - Right associative: The rightmost operator is applied first.
/// - None: The operators are not grouped together and are treated as separate operations.
#[derive(Debug, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    None,
}

impl Operator {
    /// Get the associativity of the operator.
    ///
    /// The associativity is used to determine how operators of the same precedence
    /// are grouped in the absence of parentheses.
    pub fn associativity(&self) -> Associativity {
        use Operator::*;
        match self {
            Add | Subtract => Associativity::Left,
            Mod | Multiply | Divide => Associativity::Left,
            Equal | NotEqual | GreaterThan | LessThan => Associativity::None,
        }
    }

    /// Get the precedence of the operator.
    ///
    /// The precedence is used to determine the order of operations in expressions.
    pub fn precedence(&self) -> u8 {
        use Operator::*;
        match self {
            Add | Subtract => 1,
            Multiply | Divide => 2,
            Mod => 3,
            Equal | NotEqual => 4,
            GreaterThan | LessThan => 5,
        }
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.precedence() == other.precedence() {
            Some(std::cmp::Ordering::Equal)
        } else {
            Some(self.precedence().cmp(&other.precedence()))
        }
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
    ValueMatch(Spanned<Value>),
    VariableCapture(Spanned<String>),
    BlockCapture(Spanned<String>),
    PredicateCapture(Spanned<Predicate>),
    List(Vec<Spanned<Pattern>>),
}

#[allow(dead_code)]
impl Pattern {
    #[cfg(test)]
    pub(crate) fn new_word(word: String) -> Spanned<Pattern> {
        Spanned::new(
            Span {
                file_id: 0,
                start: 0,
                end: word.len(),
            },
            Pattern::VariableCapture(word),
        )
    }

    #[cfg(test)]
    pub(crate) fn new_block_capture(name: String) -> Spanned<Pattern> {
        Spanned::new(
            Span {
                file_id: 0,
                start: 0,
                end: name.len(),
            },
            Pattern::BlockCapture(name),
        )
    }

    #[cfg(test)]
    pub(crate) fn new_predicate_capture(pred: Predicate) -> Spanned<Pattern> {
        Spanned::new(
            Span {
                file_id: 0,
                start: 0,
                end: 1,
            },
            Pattern::PredicateCapture(Spanned::new(
                Span {
                    file_id: 0,
                    start: 0,
                    end: 1,
                },
                pred,
            )),
        )
    }

    #[cfg(test)]
    pub(crate) fn new_list(patterns: Vec<Spanned<Pattern>>) -> Spanned<Pattern> {
        Spanned::new(
            Span {
                file_id: 0,
                start: 0,
                end: 1,
            },
            Pattern::List(patterns),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, Pattern, Span, Spanned, Statement};

    #[test]
    fn test_spanned_construction() {
        let span = Span {
            file_id: 1,
            start: 0,
            end: 5,
        };
        let expr = Expr::Value(Spanned::new(span.clone(), Value::Int(10)));
        let sp_expr = Spanned::new(span.clone(), expr.clone());
        assert_eq!(sp_expr.span, span);
        assert_eq!(*sp_expr, expr);
    }

    #[test]
    fn test_block_is_stored() {
        let block = Block(vec![Spanned::new(
            Span {
                file_id: 1,
                start: 0,
                end: 1,
            },
            Statement::InvalidBlock,
        )]);
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
        let span = Span {
            file_id: 1,
            start: 10,
            end: 12,
        };
        let pat = Pattern::VariableCapture("x".to_string());
        let sp_pat = Spanned::new(span.clone(), pat.clone());
        assert_eq!(sp_pat.span, span);
        assert_eq!(*sp_pat, pat);
    }
}
