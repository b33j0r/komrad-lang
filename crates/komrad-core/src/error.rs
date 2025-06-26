use crate::{ParserSpan, Span};
use nom::error::{FromExternalError, ParseError as NomParseError};
use thiserror::Error;

#[derive(Debug, Clone, Error, PartialEq)]
pub enum ParseError {
    // Variant for nom-style errors
    #[error("Nom error: {kind:?} at {span:?}")]
    Nom {
        kind: nom::error::ErrorKind,
        span: Span,
    },
    // Incomplete input error
    #[error("Incomplete input: {remaining}")]
    Incomplete { remaining: String, span: Span },
    // Error when a number cannot be parsed
    #[error("Invalid number: {value}")]
    InvalidNumber {
        value: String,
        message: String,
        span: Span,
    },
}

// --------------------------------------------
// Implement nom::error::ParseError for your ParseError.
// --------------------------------------------
impl NomParseError<ParserSpan<'_>> for ParseError {
    fn from_error_kind(input: ParserSpan, kind: nom::error::ErrorKind) -> Self {
        ParseError::Nom {
            kind,
            span: Span::from(input),
        }
    }

    fn append(input: ParserSpan, kind: nom::error::ErrorKind, other: Self) -> Self {
        other.append(kind, &Span::from(input))
    }

    fn or(self, _other: Self) -> Self {
        self
    }
}

impl FromExternalError<ParserSpan<'_>, ParseError> for ParseError {
    fn from_external_error(
        input: ParserSpan<'_>,
        _kind: nom::error::ErrorKind,
        e: ParseError,
    ) -> Self {
        e.with_span(&Span::from(input))
    }
}

impl ParseError {
    pub fn from_nom_error(e: nom::error::Error<ParserSpan<'_>>) -> Self {
        ParseError::Nom {
            kind: e.code,
            span: Span::from(e.input),
        }
    }
}

impl ParseError {
    /// Attaches a new span to the error.
    /// This method “replaces” the span in the error with the provided one.
    pub fn with_span(mut self, span: &Span) -> Self {
        match &mut self {
            ParseError::Nom { span: s, .. } => *s = span.clone(),
            ParseError::Incomplete { span: s, .. } => *s = span.clone(),
            ParseError::InvalidNumber { span: s, .. } => *s = span.clone(),
        }
        self
    }

    /// Appends additional error context by creating a new Nom error.
    pub fn append(self, kind: nom::error::ErrorKind, span: &Span) -> Self {
        // For simplicity, we return a new Nom error variant.
        ParseError::Nom {
            kind,
            span: span.clone(),
        }
    }
}

#[derive(Debug, Clone, Error, PartialEq)]
pub enum RuntimeError {
    #[error("Parse error: {0}")]
    ParseError(#[from] ParseError),

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

    #[error("Not implemented: {0}")]
    NotImplemented(String),
}