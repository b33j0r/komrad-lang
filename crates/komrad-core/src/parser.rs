// parser.rs

use crate::ast::{ParseError, Statement};
use crate::codemap::{CodeMaps, ParserSpan};
use std::path::PathBuf;

use crate::{Expr, Span, Spanned, Value};
use nom::branch::alt;
use nom::character::complete::{digit1, line_ending, multispace0, space0};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded};
use nom::{
    combinator::map_res,
    error::{FromExternalError, ParseError as NomParseError},
    Parser,
};

/// Nom-style result using ParseError for unified error reporting
pub type PResult<'a, T> = Result<(ParserSpan<'a>, T), nom::Err<ParseError>>;

/// Implement needed Nom traits so we can bubble up our `ParseError`.
impl<'a> FromExternalError<ParserSpan<'a>, ParseError> for ParseError {
    fn from_external_error(
        input: ParserSpan<'a>,
        _kind: nom::error::ErrorKind,
        e: ParseError,
    ) -> Self {
        // Attach the current span to 'e'
        e.with_span(&Span::from(input))
    }
}

impl<'a> NomParseError<ParserSpan<'a>> for ParseError {
    fn from_error_kind(input: ParserSpan<'a>, kind: nom::error::ErrorKind) -> Self {
        ParseError::Nom {
            kind,
            span: Span::from(input),
        }
    }

    fn append(input: ParserSpan<'a>, kind: nom::error::ErrorKind, other: Self) -> Self {
        other.append(kind, &Span::from(input))
    }
}

pub fn parse_file_complete(
    codemaps: &mut CodeMaps,
    source: &str,
    file_path: Option<PathBuf>,
) -> Result<Vec<Spanned<Statement>>, ParseError> {
    // 1) get initial ParserSpan from the codemaps
    let initial_span = codemaps.add_file(source, file_path);

    match parse_statements(initial_span.clone()) {
        Ok((remaining, statements)) => {
            if !remaining.fragment().is_empty() {
                // leftover text => incomplete parse
                Err(ParseError::Incomplete {
                    remaining: remaining.fragment().to_string(),
                    span: Span::from(remaining),
                })
            } else {
                Ok(statements)
            }
        }
        // If the parser returned a Nom error or needed more input
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Err(e),
        Err(nom::Err::Incomplete(_needed)) => Err(ParseError::Incomplete {
            remaining: source.to_string(),
            span: Span::from(initial_span),
        }),
    }
}


fn parse_statements(input: ParserSpan) -> PResult<Vec<Spanned<Statement>>> {
    delimited(
        multispace0,
        many0(
            separated_list0(line_ending, parse_statement)
        ),
        multispace0,
    )
        .map_res(
            |statements| {
                // let _val_span = Span::from(input.clone());
                // Flatten the nested vectors
                Ok(statements.into_iter().flatten().collect::<Vec<Spanned<Statement>>>())
            }
        )
        .parse(input)
}
fn parse_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    let input_clone = input.clone();
    preceded(
        space0,
        alt((
            parse_expression.map(move |spanned_expr| {
                let val_span = Span::from(input_clone.clone()); // Add .clone() here
                // Extract the inner Expr from Spanned<Expr> and use it
                Spanned::new(val_span, Statement::Expr(*spanned_expr.value))
            }),
            // context("assignment", parse_assignment_statement),
            // context("tell", parse_tell_statement),
        )),
    )
        .parse(input)
}

pub fn parse_expression(input: ParserSpan) -> PResult<Spanned<Expr>> {
    let input_for_span = input.clone(); // Clone here, before the closure
    alt((
        parse_number,
        // Add other expression parsers here
    ))
        .map(move |spanned_value| {
            let val_span = Span::from(input_for_span.clone());
            Spanned::new(val_span, Expr::Value(spanned_value))
        })
        .parse(input)
}

pub fn parse_number(input: ParserSpan) -> PResult<Spanned<Value>> {
    let input_clone = input.clone();
    map_res(digit1, |digits: ParserSpan| {
        // Attempt to parse as i64
        let s = digits.fragment();
        s.parse::<i64>().map_err(|e| {
            ParseError::InvalidNumber {
                value: s.to_string(),
                message: e.to_string(),
                span: Span::from(digits.clone()),
            }
        })
    })
        .map(|parsed_i64| {
            // wrap it as a Spanned<Value>
            let val_span = Span::from(input_clone.clone());
            Spanned::new(val_span, Value::Int(parsed_i64))
        })
        .parse(input)
}