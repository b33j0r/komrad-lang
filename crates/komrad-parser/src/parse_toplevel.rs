// parse_toplevel

use komrad_core::{AssignmentTarget, Block, CodeAtlas, ParserSpan, TopLevel};
use komrad_core::{Expr, ParseError, Statement};
use komrad_core::{Operator, Span, Spanned, Value};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, char, line_ending, multispace0, multispace1, not_line_ending, space0};
use nom::combinator::{map, opt};
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::{Err as NomErr, IResult, Parser};
use std::path::PathBuf;
use std::sync::Arc;

// --------------------------------------------
// PResult type alias.
// --------------------------------------------
pub type PResult<'a, T> = IResult<ParserSpan<'a>, T, ParseError>;

// --------------------------------------------
// The spanned(...) helper.
// Wrap a parser returning a T into one returning Spanned<T> using
// the consumed byte offsets.
// --------------------------------------------
fn spanned<'a, O, F>(
    mut inner: F,
) -> impl FnMut(ParserSpan<'a>) -> IResult<ParserSpan<'a>, Spanned<O>, ParseError>
where
    F: FnMut(ParserSpan<'a>) -> IResult<ParserSpan<'a>, O, ParseError>,
{
    move |input: ParserSpan<'a>| {
        let start_offset = input.location_offset();
        let file_id = input.extra.file_id;

        let (remaining, value) = inner(input)?;
        let end_offset = remaining.location_offset();

        let span = Span {
            file_id,
            start: start_offset,
            end: end_offset,
        };
        Ok((remaining, Spanned::new(span, value)))
    }
}

// --------------------------------------------
// Low-level wrappers for built-in combinators.
// (These are used for things like tag and digit1.)
// --------------------------------------------
fn parse_tag<'a>(
    t: &'static str,
) -> impl FnMut(ParserSpan<'a>) -> IResult<ParserSpan<'a>, ParserSpan<'a>, ParseError> {
    move |input: ParserSpan<'a>| nom::bytes::complete::tag(t).parse(input)
}

fn parse_digit1(input: ParserSpan<'_>) -> IResult<ParserSpan<'_>, ParserSpan<'_>, ParseError> {
    nom::character::complete::digit1(input)
}

// --------------------------------------------
// The top-level file parser.
// --------------------------------------------
pub fn parse_file_complete(
    codemaps: &mut CodeAtlas,
    source: &str,
    file_path: Option<PathBuf>,
) -> Result<TopLevel, ParseError> {
    let initial_span = codemaps.add_file(source, file_path);
    match parse_statements(initial_span.clone()) {
        Ok((remaining, statements)) => {
            if !remaining.fragment().is_empty() {
                Err(ParseError::Incomplete {
                    remaining: remaining.fragment().to_string(),
                    span: Span::from(remaining),
                })
            } else {
                Ok(TopLevel::Block(Block(statements)))
            }
        }
        Err(NomErr::Error(e)) | Err(NomErr::Failure(e)) => Err(e),
        Err(NomErr::Incomplete(_needed)) => Err(ParseError::Incomplete {
            remaining: source.to_string(),
            span: Span::from(initial_span),
        }),
    }
}

pub fn parse_snippet_complete(
    codemaps: &mut CodeAtlas,
    source: &str,
) -> Result<TopLevel, ParseError> {
    let initial_span = codemaps.add_file(source, None);
    match parse_statements(initial_span.clone()) {
        Ok((remaining, statements)) => {
            if !remaining.fragment().is_empty() {
                Err(ParseError::Incomplete {
                    remaining: remaining.fragment().to_string(),
                    span: Span::from(remaining),
                })
            } else {
                Ok(TopLevel::Block(Block(statements)))
            }
        }
        Err(NomErr::Error(e)) | Err(NomErr::Failure(e)) => Err(e),
        Err(NomErr::Incomplete(_needed)) => Err(ParseError::Incomplete {
            remaining: source.to_string(),
            span: Span::from(initial_span),
        }),
    }
}

// --------------------------------------------
// parse_statements & parse_statement
// Parse many statements separated by newlines or whitespace.
// --------------------------------------------
fn parse_statements(input: ParserSpan) -> PResult<Vec<Spanned<Statement>>> {
    // Use the old logic: use many1(preceded(opt(space0), line_ending)) as the separator.
    // After parsing, filter out blank lines.
    delimited(
        multispace0,
        separated_list0(many1(preceded(opt(space0), line_ending)), parse_statement),
        multispace0,
    )
        .map(|stmts| {
            stmts
                .into_iter()
                .filter(|s| !matches!(*s.value, Statement::BlankLine))
                .collect()
        })
        .parse(input)
}

fn parse_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    preceded(
        space0,
        alt((
            parse_assignment_statement,
            parse_tell_statement,
            parse_expr_statement,
            parse_blank_line,
            parse_comment_line
        )),
    )
        .parse(input)
}

fn parse_tell_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    // like `bob something 42 + 3 ok`
    spanned(|i| {
        pair(parse_call_target, preceded(space0, parse_call_args))
            .map(|(target, args)| {
                Statement::Tell {
                    target,
                    value: args,
                }
            })
            .parse(i)
    }).parse(input)
}

fn parse_call_target(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned(|i| {
        alt((
            parse_identifier_value.map(|sp_val| {
                Expr::Value(sp_val)
            }),
        )).parse(i)
    })
        .parse(input)
}

fn parse_call_args(input: ParserSpan) -> PResult<Spanned<Expr>> {
    // like `something 42 + 3 ok` as an Expr::List
    spanned(|i| {
        separated_list1(space0, parse_call_arg).map(
            |args| {
                Expr::List {
                    elements: args
                }
            }
        ).parse(i)
    })
        .parse(input)
}

fn parse_call_arg(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned(|i| {
        alt((
            parse_number_value.map(|sp_val| {
                Expr::Value(sp_val)
            }),
            parse_identifier_value.map(|sp_val| {
                Expr::Value(sp_val)
            }),
        )).parse(i)
    })
        .parse(input)
}

/// Assignment statement: parse an assignment and return it as a Statement.
fn parse_assignment_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned(|i| {
        pair(
            parse_assignment_target,
            preceded(preceded(space0, parse_tag("=")), preceded(space0, parse_expression)),
        ).map(
            |(target, expr)| {
                Statement::Assign {
                    target,
                    value: expr,
                }
            }
        ).parse(i)
    })
        .parse(input)
}

/// Assignment target: parse an identifier and return it as a Value::Word.
fn parse_assignment_target(input: ParserSpan) -> PResult<Spanned<AssignmentTarget>> {
    spanned(|i| {
        alt((
            // Try destructuring first.
            parse_destructure_target,
            // Fallback to variable assignment.
            parse_identifier.map(|sp_val| AssignmentTarget::Variable(sp_val))
        ))
            .parse(i)
    })
        .parse(input)
}


fn parse_destructure_target(input: ParserSpan) -> PResult<AssignmentTarget> {
    // Recognize a bracketed list of assignment targets.
    delimited(
        char('['),
        // You might use a combinator that expects one or more assignment targets (separated by whitespace)
        separated_list1(multispace1, parse_assignment_target),
        char(']'),
    )
        .map(|elements| AssignmentTarget::List { elements })
        .parse(input)
}

#[cfg(test)]
mod assignment_tests {
    use super::*;

    #[test]
    fn test_parse_assignment() {
        let input = "x = 42";
        let mut codemaps = CodeAtlas::new();
        let initial_span = codemaps.add_file(input, None);
        let result = parse_assignment_statement(initial_span);

        assert!(result.is_ok(), "Failed to parse assignment: {:?}", result);

        let (remaining, assignment) = result.unwrap();
        assert_eq!(remaining.fragment().to_string(), "");

        if let Statement::Assign { target, value, .. } = *assignment.value {
            if let AssignmentTarget::Variable(var) = *target.value {
                assert_eq!(var, "x");
            } else {
                panic!("Expected a variable assignment");
            }
            if let Expr::Value(ref sp_val) = *value.value {
                if let Value::Int(i) = *sp_val.value {
                    assert_eq!(i, 42);
                } else {
                    panic!("Expected an integer value");
                }
            } else {
                panic!("Expected a value expression");
            }
        } else {
            panic!("Expected an assignment statement");
        }
    }
}

/// Comment line: parse a comment starting with '#' and consume all characters
/// up to but not including the newline. Preceding whitespace is already handled.
fn parse_comment_line(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned(|i| {
        // Expect the comment marker immediately.
        let (i, _) = tag("#")(i)?;
        let (i, _) = space0(i)?;
        // Consume all characters until a newline, without consuming the newline itself.
        let (i, comment_text) = not_line_ending(i)?;
        // Create the Comment statement.
        Ok((i, Statement::Comment(comment_text.fragment().to_string())))
    })
        .parse(input)
}

/// Blank line: simply consume the line and return a Statement::BlankLine.
fn parse_blank_line(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned(|i| {
        let (i, _) = line_ending(i)?;
        Ok((i, Statement::BlankLine))
    })
        .parse(input)
}

/// Expression statement: simply parse an expression and wrap it as a Statement.
fn parse_expr_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned(|i| {
        parse_expression.map(|expr| {
            Statement::Expr(expr)
        }).parse(i)
    })
        .parse(input)
}

// --------------------------------------------
// Expression parsers.
// --------------------------------------------
pub fn parse_expression(input: ParserSpan) -> PResult<Spanned<Expr>> {
    alt((parse_block_or_dict, parse_binary_expression, parse_expr_toplevel)).parse(input)
}

/// A toplevel expression (number or identifier) is wrapped into Expr::Value.
fn parse_expr_toplevel(input: ParserSpan) -> PResult<Spanned<Expr>> {
    alt((
        parse_list_expr,
        parse_number_value.map(|sp_val| Spanned::new(sp_val.span.clone(), Expr::Value(sp_val))),
        parse_identifier_value.map(|sp_val| Spanned::new(sp_val.span.clone(), Expr::Value(sp_val))),
    ))
        .parse(input)
}

/// Parse a list of expressions separated by whitespace or optional commas.
/// A list: `[ item1 item2 item3 ]`. Each item is parsed with `parse_expr_no_call`
// OLD IMPLEMENTATION
// fn parse_list(input: Span) -> KResult<KExpr> {
//     context(
//         "list",
//         delimited(
//             char('['),
//             preceded(
//                 multispace0,
//                 separated_list0(
//                     alt((multispace1, delimited(multispace0, tag(","), multispace0))),
//                     parse_expr_no_call,
//                 ),
//             ),
//             preceded(
//                 preceded(multispace0, opt((tag(","), multispace0))),
//                 char(']'),
//             ),
//         ),
//     )
//         .map(KExpr::List)
//         .parse(input)
// }
// NEW
fn parse_list_expr(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned(|i| {
        delimited(
            char('['),
            preceded(
                multispace0,
                separated_list0(
                    alt((multispace1, delimited(multispace0, tag(","), multispace0))),
                    parse_expr_toplevel,
                ),
            ),
            preceded(
                preceded(multispace0, opt((tag(","), multispace0))),
                char(']'),
            ),
        )
            .map(|elements| Expr::List { elements })
            .parse(i)
    })
        .parse(input)
}

/// Minimal binary expression: `expr operator expr`.
/// The overall span of the binary expression is the range from the start of the left
/// sub-expression to the end of the right sub-expression.
fn parse_binary_expression(input: ParserSpan) -> PResult<Spanned<Expr>> {
    let start = input.clone();
    let (i, left) = (parse_expr_toplevel).parse(input)?;
    let (i, op) = delimited(space0, parse_binary_operator, space0).parse(i)?;
    let (i, right) = parse_expr_toplevel.parse(i)?;

    let span = Span {
        file_id: start.extra.file_id,
        start: start.location_offset(),
        end: i.location_offset(),
    };
    Ok((
        i,
        Spanned::new(
            span,
            Expr::BinaryExpr {
                lhs: left,
                op,
                rhs: right,
            },
        ),
    ))
}

// --------------------------------------------
// Block or dictionary expression: parse a block of statements or a dictionary.

/// Distinguish a `{ block }` vs. `{ dict }` by checking for colon usage, etc.
fn parse_block_or_dict(input: ParserSpan) -> PResult<Spanned<Expr>> {
    parse_block.parse(input)
}

/// A `{ ... }` block is a list of statements in braces.
fn parse_block(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned(
        |i| {
            parse_block_value
                .map(|block| {
                    Expr::Value(block)
                }).parse(i)
        }
    ).parse(input)
}

fn parse_block_value(input: ParserSpan) -> PResult<Spanned<Value>> {
    spanned(|i| {
        delimited(
            char('{'),
            delimited(multispace0, parse_statements, multispace0),
            char('}'),
        )
            .map(|block| Value::Block(Arc::new(Block(block))))
            .parse(i)
    })
        .parse(input)
}

// --------------------------------------------
// Binary operator parser.
// No leading space is consumed; spacing is managed externally.
// --------------------------------------------
fn parse_binary_operator(input: ParserSpan) -> IResult<ParserSpan, Spanned<Operator>, ParseError> {
    spanned(|i| {
        alt((
            map(parse_tag("+"), |_| Operator::Add),
            map(parse_tag("-"), |_| Operator::Subtract),
            map(parse_tag("*"), |_| Operator::Multiply),
            map(parse_tag("/"), |_| Operator::Divide),
            map(parse_tag("=="), |_| Operator::Equal),
            map(parse_tag("!="), |_| Operator::NotEqual),
            map(parse_tag(">"), |_| Operator::GreaterThan),
            map(parse_tag("<"), |_| Operator::LessThan),
        ))
            .parse(i)
    })
        .parse(input)
}

// --------------------------------------------
// Number and Identifier parsers.
// They return Spanned<Value> which we later wrap into an Expr.
// --------------------------------------------
fn parse_number_value(input: ParserSpan) -> IResult<ParserSpan, Spanned<Value>, ParseError> {
    spanned(|i| {
        let (i, digits) = parse_digit1(i)?;
        let s = digits.fragment();
        let val_i64 = s.parse::<i64>().map_err(|e| {
            NomErr::Error(ParseError::InvalidNumber {
                value: s.to_string(),
                message: e.to_string(),
                span: Span::from(digits.clone()),
            })
        })?;
        Ok((i, Value::Int(val_i64)))
    })
        .parse(input)
}

fn parse_identifier(input: ParserSpan) -> IResult<ParserSpan, String, ParseError> {
    // an alpha or underscore followed by alphanum or underscore or hyphen
    (
        alpha1,
        many0(alt((
            alpha1,
            parse_tag("_"),
            parse_tag("-"),
        ))),
    ).map(
        |(first, rest): (ParserSpan, Vec<ParserSpan>)| {
            let mut id = first.fragment().to_string();
            for s in rest {
                id.push_str(s.fragment());
            }
            id
        },
    ).parse(input)
}

fn parse_identifier_value(input: ParserSpan) -> IResult<ParserSpan, Spanned<Value>, ParseError> {
    spanned(|i| {
        parse_identifier.map(
            |s: String| {
                Value::Word(s)
            }
        ).parse(i)
    })
        .parse(input)
}

#[cfg(test)]
pub mod parser_tests {
    // tests/parser_tests.rs
    use super::*;

    /// Helper: Given the source code as a &str, create a new CodeMaps and parse a file.
    fn parse_complete(source: &str) -> Result<Vec<Spanned<Statement>>, ParseError> {
        let mut codemaps = CodeAtlas::new();
        parse_file_complete(&mut codemaps, source, None).map(|top| match top {
            TopLevel::Block(block) => block.0,
            _ => vec![],
        })
    }

    #[test]
    fn test_parse_integer() {
        // A simple integer literal.
        let input = "42";
        let stmts = parse_complete(input).expect("should parse integer");
        assert_eq!(stmts.len(), 1);

        // The statement should be an expression wrapping a value.
        let stmt = &stmts[0];
        if let Statement::Expr(ref expr) = *stmt.value {
            if let Expr::Value(sp_val) = &**expr {
                if let Value::Int(i) = *sp_val.value {
                    assert_eq!(i, 42);
                } else {
                    panic!("Expected an integer literal");
                }
            } else {
                panic!("Expected a value expression");
            }
        } else {
            panic!("Expected an expression statement");
        }

        // Check that the span recovered from input matches the expected substring.
        let span_text = &input[stmt.span.start..stmt.span.end];
        assert_eq!(span_text, "42");
    }

    #[test]
    fn test_parse_identifier() {
        // Test a simple identifier.
        let input = "fooBar";
        let stmts = parse_complete(input).expect("should parse identifier");
        assert_eq!(stmts.len(), 1);

        let stmt = &stmts[0];
        if let Statement::Expr(ref expr) = *stmt.value {
            if let Expr::Value(sp_val) = &**expr {
                if let Value::Word(ref word) = *sp_val.value {
                    assert_eq!(word, "fooBar");
                } else {
                    panic!("Expected an identifier word");
                }
            } else {
                panic!("Expected a value expression");
            }
        } else {
            panic!("Expected an expression statement got {:?}", *stmt);
        }

        // Check span text.
        let span_text = &input[stmt.span.start..stmt.span.end];
        assert_eq!(span_text, "fooBar");
    }

    #[test]
    fn test_parse_addition() {
        // Test binary expression: addition.
        let input = "1+2";
        let stmts = parse_complete(input).expect("should parse binary addition");
        assert_eq!(stmts.len(), 1);
        let stmt = &stmts[0];

        // The outer statement should be a binary expression.
        if let Statement::Expr(ref expr) = *stmt.value {
            if let Expr::BinaryExpr { lhs, op, rhs } = &**expr {
                // lhs should be "1"
                if let Expr::Value(ref sp_val) = *lhs.value {
                    if let Value::Int(i1) = *sp_val.value {
                        assert_eq!(i1, 1);
                    } else {
                        panic!("Left operand is not an integer");
                    }
                } else {
                    panic!("Left operand is not a value expression");
                }
                // op should be Add.
                assert_eq!(*op.value, Operator::Add);
                // rhs should be "2"
                if let Expr::Value(ref sp_val) = *rhs.value {
                    if let Value::Int(i2) = *sp_val.value {
                        assert_eq!(i2, 2);
                    } else {
                        panic!("Right operand is not an integer");
                    }
                } else {
                    panic!("Right operand is not a value expression");
                }
                // Verify overall span recovers the full binary expression.
                let span_text = &input[stmt.span.start..stmt.span.end];
                assert_eq!(span_text, "1+2");
            } else {
                panic!("Expected a binary expression");
            }
        } else {
            panic!("Expected an expression statement");
        }
    }

    #[test]
    fn test_parse_binary_expression_with_whitespace() {
        // Test binary expression with extra spaces.
        let input = "  10   -  5  ";
        let stmts = parse_complete(input).expect("should parse binary expression with whitespace");
        assert_eq!(stmts.len(), 1);
        let stmt = &stmts[0];

        if let Statement::Expr(ref expr) = *stmt.value {
            if let Expr::BinaryExpr { lhs, op, rhs } = &**expr {
                // Check left operand: 10.
                if let Expr::Value(ref sp_val) = *lhs.value {
                    if let Value::Int(i1) = *sp_val.value {
                        assert_eq!(i1, 10);
                    } else {
                        panic!("Left operand not integer");
                    }
                } else {
                    panic!("Left operand not a value expression");
                }
                // Operator should be Subtract.
                assert_eq!(*op.value, Operator::Subtract);
                // Right operand: 5.
                if let Expr::Value(ref sp_val) = *rhs.value {
                    if let Value::Int(i2) = *sp_val.value {
                        assert_eq!(i2, 5);
                    } else {
                        panic!("Right operand not integer");
                    }
                } else {
                    panic!("Right operand not a value expression");
                }
                // Check recovered span.
                // The parser's delimited combinators trim the outer whitespace.
                let span_text = &input[stmt.span.start..stmt.span.end];
                assert_eq!(span_text, "10   -  5");
            } else {
                panic!("Expected a binary expression");
            }
        } else {
            panic!("Expected an expression statement");
        }
    }

    #[test]
    fn test_parse_equality_operator() {
        // Test binary expression with the equality operator.
        let input = "7==8";
        let stmts = parse_complete(input).expect("should parse equality binary expression");
        assert_eq!(stmts.len(), 1);
        let stmt = &stmts[0];

        if let Statement::Expr(ref expr) = *stmt.value {
            if let Expr::BinaryExpr { lhs, op, rhs } = &**expr {
                // Left side should be 7.
                if let Expr::Value(ref sp_val) = *lhs.value {
                    if let Value::Int(i) = *sp_val.value {
                        assert_eq!(i, 7);
                    } else {
                        panic!("Left side is not integer");
                    }
                } else {
                    panic!("Left side not a value expression");
                }
                // Operator should be Equal.
                assert_eq!(*op.value, Operator::Equal);
                // Right side should be 8.
                if let Expr::Value(ref sp_val) = *rhs.value {
                    if let Value::Int(i) = *sp_val.value {
                        assert_eq!(i, 8);
                    } else {
                        panic!("Right side is not integer");
                    }
                } else {
                    panic!("Right side not a value expression");
                }
                let span_text = &input[stmt.span.start..stmt.span.end];
                assert_eq!(span_text, "7==8");
            } else {
                panic!("Expected a binary expression");
            }
        } else {
            panic!("Expected an expression statement");
        }
    }

    #[test]
    fn test_incomplete_error() {
        // Test that extra (unparsed) input yields an Incomplete error.
        let input = "123 extra";
        let result = parse_complete(input);
        assert!(result.is_err());

        if let Err(err) = result {
            match err {
                ParseError::Incomplete { remaining, .. } => {
                    // The parser should stop at the " extra" part.
                    // Note that multispace at beginning of the leftover may be trimmed.
                    assert_eq!(remaining.trim(), "extra");
                }
                _ => panic!("Expected an Incomplete error variant"),
            }
        }
    }

    /// This test verifies that the spanned helper correctly tracks source regions in a realistic scenario.
    #[test]
    fn test_spanned_text_recovery() {
        // Input with leading whitespace that is trimmed by the outer combinator.
        let input = "   789";
        let stmts = parse_complete(input).expect("should parse spanned expression");
        // The only statement should represent the number 789.
        assert_eq!(stmts.len(), 1);
        let stmt = &stmts[0];
        // Because the delimited combinator in parse_statements consumes leading/trailing spaces,
        // the span of the statement should only include "789" (from the original input).
        let recovered_text = &input[stmt.span.start..stmt.span.end];
        assert_eq!(recovered_text, "789");
    }
}
