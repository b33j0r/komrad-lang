use crate::parse_strings::parse_string_value;
use crate::result::PResult;
use crate::spanned;
use komrad_core::Value;
use komrad_core::{AsSpanned, Associativity, Handler};
use komrad_core::{AssignmentTarget, Block, CodeAtlas, ParserSpan, TopLevel};
use komrad_core::{Expr, Statement};
use komrad_core::{Operator, Span, Spanned};
use komrad_core::{ParseError, Pattern};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, char, line_ending, multispace0, multispace1, not_line_ending, space0, space1};
use nom::combinator::{map, opt};
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::{Err as NomErr, IResult, Parser};
use std::path::PathBuf;
use std::sync::Arc;

/// A small wrapper around `nom::bytes::complete::tag` so it returns our error type.
fn parse_tag<'a>(
    t: &'static str,
) -> impl FnMut(ParserSpan<'a>) -> IResult<ParserSpan<'a>, ParserSpan<'a>, ParseError> {
    move |input: ParserSpan<'a>| nom::bytes::complete::tag(t).parse(input)
}

/// A small wrapper around `nom::character::complete::digit1`.
fn parse_digit1(input: ParserSpan<'_>) -> IResult<ParserSpan<'_>, ParserSpan<'_>, ParseError> {
    nom::character::complete::digit1(input)
}

/// The main entry point for parsing an entire file into a `TopLevel::Block`.
pub fn parse_file_complete(
    codemaps: &mut CodeAtlas,
    source: &str,
    file_path: Option<PathBuf>,
) -> Result<TopLevel, ParseError> {
    let initial_span = codemaps.add_file(source, file_path);
    match parse_statements.parse(initial_span.clone()) {
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

/// Similar to `parse_file_complete` but for shorter snippet inputs, no file path.
pub fn parse_snippet_complete(
    codemaps: &mut CodeAtlas,
    source: &str,
) -> Result<TopLevel, ParseError> {
    let initial_span = codemaps.add_file(source, None);
    match parse_statements.parse(initial_span.clone()) {
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

/// Parses multiple statements separated by newlines, filtering out blank lines.
fn parse_statements(input: ParserSpan) -> PResult<Vec<Spanned<Statement>>> {
    // Splits statements by one or more line breaks. Blank lines become Statement::BlankLine.
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

/// Parses a single statement: assignment, handler, tell, expression, blank, or comment.
fn parse_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    preceded(
        space0,
        alt((
            parse_assignment_statement,
            parse_handler_statement,
            parse_tell_statement,
            parse_expr_statement,
            parse_blank_line,
            parse_comment_line,
        )),
    )
        .parse(input)
}

/// Parses a handler statement `[pattern] { block }`.
fn parse_handler_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned::spanned(|i| {
        pair(
            delimited(
                preceded(space0, char('[')),
                preceded(multispace0, parse_pattern),
                preceded(multispace0, char(']')),
            ),
            preceded(space0, parse_block_expr),
        )
            .map(|(pattern, expr)| Statement::Handler(Arc::new(Handler { pattern, expr })))
            .parse(i)
    })
        .parse(input)
}

/// Parses a pattern list (e.g. `_foo 123 "bar"`).
fn parse_pattern(input: ParserSpan) -> PResult<Spanned<Pattern>> {
    spanned::spanned(|i| {
        preceded(
            multispace0,
            separated_list0(multispace1, parse_pattern_element),
        )
            .map(Pattern::List)
            .parse(i)
    })
        .parse(input)
}

/// Parses a single pattern element, which might be `_var`, an integer, string, etc.
fn parse_pattern_element(input: ParserSpan) -> PResult<Spanned<Pattern>> {
    spanned::spanned(|i| {
        alt((
            parse_variable_capture.map(Pattern::VariableCapture),
            parse_block_capture.map(Pattern::BlockCapture),
            parse_identifier_value.map(Pattern::ValueMatch),
            parse_number_value.map(Pattern::ValueMatch),
            parse_string_value.map(Pattern::ValueMatch),
        ))
            .parse(i)
    })
        .parse(input)
}

/// Parses `_abc` as a variable capture.
fn parse_variable_capture(input: ParserSpan) -> PResult<Spanned<String>> {
    spanned::spanned(|i| {
        preceded(tag("_"), parse_identifier)
            .map(|id| id)
            .parse(i)
    })
        .parse(input)
}

/// Parses `_{abc}` as a block capture.
fn parse_block_capture(input: ParserSpan) -> PResult<Spanned<String>> {
    spanned::spanned(|i| {
        delimited(
            preceded(tag("_{"), space0),
            parse_identifier,
            preceded(space0, char('}')),
        )
            .map(|id| id)
            .parse(i)
    })
        .parse(input)
}

/// Parses a `tell` statement, e.g. `xyz alpha+beta something 123`.
/// It uses a specialized argument parser that does not produce `Ask`.
fn parse_tell_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned::spanned(|i| {
        pair(parse_call_target, preceded(space0, parse_call_args_for_tell))
            .map(|(target, args)| Statement::Tell { target, value: args })
            .parse(i)
    })
        .parse(input)
}

/// The "target" portion of a tell statement is usually just a single identifier expression.
fn parse_call_target(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned::spanned(|i| {
        parse_identifier_value.map(Expr::Value).parse(i)
    })
        .parse(input)
}

/// Parses the argument list for a tell statement.
/// We do not want trailing space + subexpressions to become `Ask`, so we call
/// `parse_expression_no_call` for each argument.
fn parse_call_args_for_tell(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned::spanned(|i| {
        separated_list1(space0, parse_expression_no_call)
            .map(|args| Expr::List { elements: args })
            .parse(i)
    })
        .parse(input)
}

/// Parse an assignment statement of the form `target = expression`.
fn parse_assignment_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned::spanned(|i| {
        pair(
            parse_assignment_target,
            preceded(
                preceded(space0, parse_tag("=")),
                preceded(space0, parse_expression),
            ),
        )
            .map(|(target, expr)| Statement::Assign { target, value: expr })
            .parse(i)
    })
        .parse(input)
}

/// Parse the left-hand side of an assignment, either a single variable or a destructuring list.
fn parse_assignment_target(input: ParserSpan) -> PResult<Spanned<AssignmentTarget>> {
    spanned::spanned(|i| {
        alt((parse_destructure_target, parse_identifier.map(AssignmentTarget::Variable))).parse(i)
    })
        .parse(input)
}

/// Parse destructuring brackets `[x y z]`.
fn parse_destructure_target(input: ParserSpan) -> PResult<AssignmentTarget> {
    delimited(
        char('['),
        separated_list1(multispace1, parse_assignment_target),
        char(']'),
    )
        .map(|elements| AssignmentTarget::List { elements })
        .parse(input)
}

/// A test for the assignment parser.
#[cfg(test)]
mod assignment_tests {
    use super::*;

    #[test]
    fn test_parse_assignment() {
        let input = "x = 42";
        let mut codemaps = CodeAtlas::new();
        let initial_span = codemaps.add_file(input, None);

        let (remaining, stmt) = parse_assignment_statement.parse(initial_span).unwrap();
        assert_eq!(remaining.fragment().to_string(), "");

        if let Statement::Assign { target, value } = &*stmt.value {
            // check the variable
            match &*target.value {
                AssignmentTarget::Variable(var_name) => {
                    assert_eq!(var_name, "x");
                }
                _ => panic!("Expected a simple variable"),
            }
            // check the right side
            match &*value.value {
                Expr::Value(sp_val) => match &*sp_val.value {
                    Value::Int(i) => assert_eq!(*i, 42),
                    _ => panic!("Expected integer 42"),
                },
                _ => panic!("Expected a value expression"),
            }
        } else {
            panic!("Expected an assignment statement");
        }
    }
}

/// Parse a comment line that begins with `#`, ignoring trailing text up to the newline.
fn parse_comment_line(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned::spanned(|i| {
        let (i, _) = tag("#").parse(i)?;
        let (i, _) = space0.parse(i)?;
        let (i, text) = not_line_ending.parse(i)?;
        Ok((i, Statement::Comment(text.fragment().to_string())))
    })
        .parse(input)
}

/// Parse a blank line as `Statement::BlankLine`.
fn parse_blank_line(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned::spanned(|i| {
        let (i, _) = line_ending.parse(i)?;
        Ok((i, Statement::BlankLine))
    })
        .parse(input)
}

/// Parse an expression statement, wrapping the expression in `Statement::Expr`.
fn parse_expr_statement(input: ParserSpan) -> PResult<Spanned<Statement>> {
    spanned::spanned(|i| {
        parse_expression
            .map(Statement::Expr)
            .parse(i)
    })
        .parse(input)
}

// --------------------------------------------------------------------
// Public expression parser (for normal expressions):
// This can produce `Ask` if there's a space-suffix with subexpressions.
// --------------------------------------------------------------------

/// Public expression parser that can produce `Ask` for trailing arguments.
fn parse_expression(input: ParserSpan) -> PResult<Spanned<Expr>> {
    parse_binary_expression.parse(input)
}

/// A specialized expression parser used inside `tell` arguments,
/// ensuring we never produce an `Ask` node from trailing space + expressions.
fn parse_expression_no_call(input: ParserSpan) -> PResult<Spanned<Expr>> {
    parse_binary_expression_no_call.parse(input)
}

// --------------------------------------------------------------------
// "Normal" parse_binary_expression can produce `Ask` from a call suffix.
// --------------------------------------------------------------------

fn parse_binary_expression(input: ParserSpan) -> PResult<Spanned<Expr>> {
    let (i, lhs) = parse_precedence_expression(0).parse(input)?;
    let (i, args_opt) = opt(preceded(space1, separated_list1(space1, parse_precedence_expression(0)))).parse(i)?;
    if let Some(args) = args_opt {
        // produce an Ask node
        let span = Span {
            file_id: lhs.span.file_id,
            start: lhs.span.start,
            end: args.last().unwrap().span.end,
        };
        let list_span = Span {
            file_id: lhs.span.file_id,
            start: args.first().unwrap().span.start,
            end: args.last().unwrap().span.end,
        };
        Ok((
            i,
            Expr::Ask {
                target: lhs,
                value: Expr::List { elements: args }.as_spanned(list_span),
            }
                .as_spanned(span),
        ))
    } else {
        Ok((i, lhs))
    }
}

// --------------------------------------------------------------------
// "No-call" version that never tries to parse trailing arguments as `Ask`.
// --------------------------------------------------------------------

fn parse_binary_expression_no_call(input: ParserSpan) -> PResult<Spanned<Expr>> {
    // We parse precedence-based subexpressions but skip the optional suffix.
    parse_precedence_expression(0).parse(input)
}

// --------------------------------------------------------------------
// Precedence-based subexpression parsing used by both normal and no-call versions.
// --------------------------------------------------------------------

fn parse_precedence_expression(
    min_prec: u8,
) -> impl FnMut(ParserSpan) -> PResult<Spanned<Expr>> {
    move |mut input| {
        let (i2, mut lhs) = parse_expr_toplevel.parse(input)?;
        input = i2;

        loop {
            // see if next token is an operator of sufficient precedence
            let op_res = delimited(multispace0, parse_binary_operator, multispace0).parse(input.clone());
            match op_res {
                Ok((i3, op)) => {
                    let prec = op.precedence();
                    if prec < min_prec {
                        break;
                    }
                    let next_min = match op.associativity() {
                        Associativity::Left => prec + 1,
                        Associativity::Right => prec,
                        Associativity::None => prec,
                    };
                    let (i4, rhs) = parse_precedence_expression(next_min).parse(i3)?;
                    lhs = Expr::BinaryExpr {
                        lhs: lhs.clone(),
                        op,
                        rhs: rhs.clone(),
                    }
                        .as_spanned(Span {
                            file_id: lhs.span.file_id,
                            start: lhs.span.start,
                            end: rhs.span.end,
                        });
                    input = i4;
                }
                Err(_) => break,
            }
        }
        Ok((input, lhs))
    }
}

// --------------------------------------------------------------------
// Toplevel subexpressions: parentheses, blocks, dicts, strings, etc.
// --------------------------------------------------------------------

fn parse_expr_toplevel(input: ParserSpan) -> PResult<Spanned<Expr>> {
    alt((
        parse_expander_expr,
        parse_parenthesized_expr,
        parse_list_expr,
        parse_block_or_dict,
        parse_string_expr,
        map(parse_number_value, |val| {
            Spanned::new(val.span.clone(), Expr::Value(val))
        }),
        map(parse_identifier_value, |val| {
            Spanned::new(val.span.clone(), Expr::Value(val))
        }),
    ))
        .parse(input)
}

/// Parse an expander expression like `*{ ... }` or `*[ 1 2 ]`.
fn parse_expander_expr(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned::spanned(|i| {
        preceded(tag("*"), preceded(space0, alt((parse_block_or_dict, parse_list_expr, parse_identifier_expr))))
            .map(|block| Expr::Expander { target: block })
            .parse(i)
    })
        .parse(input)
}

/// Parse a parenthesized expression `( expr )`.
fn parse_parenthesized_expr(input: ParserSpan) -> PResult<Spanned<Expr>> {
    delimited(char('('), preceded(multispace0, parse_expression), preceded(multispace0, char(')')))
        .parse(input)
}

/// Parse a list expression `[ expr1 expr2 ]`.
fn parse_list_expr(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned::spanned(|i| {
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
                preceded(multispace0, opt(pair(tag(","), multispace0))),
                char(']'),
            ),
        )
            .map(|elements| Expr::List { elements })
            .parse(i)
    })
        .parse(input)
}

/// Parse either a block `{ statements }` or a dictionary `{ key: val, ... }`.
fn parse_block_or_dict(input: ParserSpan) -> PResult<Spanned<Expr>> {
    alt((parse_dict_expr, parse_block_expr)).parse(input)
}

/// Parse a block expression `{ ... }` as `Expr::Value(Value::Block(...))`.
fn parse_block_expr(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned::spanned(|i| parse_block_value.map(Expr::Value).parse(i)).parse(input)
}

/// Parse the interior of a block into a `Value::Block`.
fn parse_block_value(input: ParserSpan) -> PResult<Spanned<Value>> {
    spanned::spanned(|i| {
        delimited(
            char('{'),
            delimited(multispace0, parse_statements, multispace0),
            char('}'),
        )
            .map(|stmts| Value::Block(Arc::new(Block(stmts))))
            .parse(i)
    })
        .parse(input)
}

/// Parse a dictionary expression `{ k: v, ... }`.
fn parse_dict_expr(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned::spanned(|i| {
        delimited(
            pair(char('{'), multispace0),
            separated_list0(
                alt((delimited(multispace0, tag(","), multispace0), multispace1)),
                parse_dict_entry,
            ),
            preceded(
                opt(delimited(multispace0, char(','), multispace0)),
                preceded(multispace0, char('}')),
            ),
        )
            .map(|pairs| {
                let mut map = indexmap::IndexMap::new();
                for (k, v) in pairs {
                    map.insert(k, v);
                }
                Expr::Dict { index_map: map }
            })
            .parse(i)
    })
        .parse(input)
}


/// Parse a single dict entry `key: expression`.
fn parse_dict_entry(input: ParserSpan) -> PResult<(String, Spanned<Expr>)> {
    pair(
        parse_identifier,
        preceded(
            preceded(multispace0, tag(":")),
            preceded(multispace0, parse_expression),
        ),
    )
        .parse(input)
}

/// Parse a string expression into `Expr::Value(Value::Str(...))`.
fn parse_string_expr(input: ParserSpan) -> PResult<Spanned<Expr>> {
    spanned::spanned(|i| parse_string_value.map(Expr::Value).parse(i)).parse(input)
}

/// Parse a binary operator token (`+`, `-`, `*`, etc.).
fn parse_binary_operator(input: ParserSpan) -> IResult<ParserSpan, Spanned<Operator>, ParseError> {
    spanned::spanned(|i| {
        alt((
            map(parse_tag("+"), |_| Operator::Add),
            map(parse_tag("-"), |_| Operator::Subtract),
            map(parse_tag("*"), |_| Operator::Multiply),
            map(parse_tag("/"), |_| Operator::Divide),
            map(parse_tag("%"), |_| Operator::Mod),
            map(parse_tag("=="), |_| Operator::Equal),
            map(parse_tag("!="), |_| Operator::NotEqual),
            map(parse_tag(">"), |_| Operator::GreaterThan),
            map(parse_tag("<"), |_| Operator::LessThan),
        ))
            .parse(i)
    })
        .parse(input)
}

/// Parse a numeric literal into `Value::Int`.
fn parse_number_value(input: ParserSpan) -> IResult<ParserSpan, Spanned<Value>, ParseError> {
    spanned::spanned(|i| {
        let (i, digits) = parse_digit1.parse(i)?;
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

/// Parse an identifier (alpha or `_` followed by alphanumerics, `_`, or `-`).
fn parse_identifier(input: ParserSpan) -> IResult<ParserSpan, String, ParseError> {
    (alpha1, many0(alt((alpha1, parse_tag("_"), parse_tag("-")))))
        .map(|(fst, rest): (ParserSpan, Vec<ParserSpan>)| {
            let mut out = fst.fragment().to_string();
            for chunk in rest {
                out.push_str(chunk.fragment());
            }
            out
        })
        .parse(input)
}

/// Wrap an identifier string in `Value::Word`.
fn parse_identifier_value(input: ParserSpan) -> IResult<ParserSpan, Spanned<Value>, ParseError> {
    spanned::spanned(|i| parse_identifier.map(Value::Word).parse(i)).parse(input)
}

/// Parse an identifier into a spanned expression.
fn parse_identifier_expr(input: ParserSpan) -> IResult<ParserSpan, Spanned<Expr>, ParseError> {
    spanned::spanned(|i| parse_identifier_value.map(Expr::Value).parse(i)).parse(input)
}

#[cfg(test)]
pub mod parser_tests {
    use super::*;

    /// A helper function that parses the entire input using `parse_file_complete`,
    /// returning the vector of statements in the top-level block.
    fn parse_complete(source: &str) -> Result<Vec<Spanned<Statement>>, ParseError> {
        let mut codemaps = CodeAtlas::new();
        parse_file_complete(&mut codemaps, source, None).map(|top| match top {
            TopLevel::Block(b) => b.0,
            _ => vec![],
        })
    }

    #[test]
    fn test_parse_integer() {
        let input = "42";
        let stmts = parse_complete(input).expect("should parse integer literal");
        assert_eq!(stmts.len(), 1);

        match &*stmts[0].value {
            Statement::Expr(expr) => match &**expr {
                Expr::Value(sp_val) => match &*sp_val.value {
                    Value::Int(i) => assert_eq!(*i, 42),
                    _ => panic!("Expected int 42"),
                },
                _ => panic!("Expected Expr::Value"),
            },
            _ => panic!("Expected a single expression statement"),
        }

        // verify that the spanned text matches "42"
        let span_text = &input[stmts[0].span.start..stmts[0].span.end];
        assert_eq!(span_text, "42");
    }

    #[test]
    fn test_parse_identifier() {
        let input = "fooBar";
        let stmts = parse_complete(input).expect("should parse identifier");
        assert_eq!(stmts.len(), 1);

        match &*stmts[0].value {
            Statement::Expr(expr) => match &**expr {
                Expr::Value(sp_val) => match &*sp_val.value {
                    Value::Word(w) => assert_eq!(w, "fooBar"),
                    _ => panic!("Expected Word(fooBar)"),
                },
                _ => panic!("Expected Expr::Value for identifier"),
            },
            _ => panic!("Expected an expression statement for identifier"),
        }

        let span_text = &input[stmts[0].span.start..stmts[0].span.end];
        assert_eq!(span_text, "fooBar");
    }

    #[test]
    fn test_parse_addition() {
        // "1+2" => an expression statement with a binary expression.
        let input = "1+2";
        let stmts = parse_complete(input).expect("should parse addition");
        assert_eq!(stmts.len(), 1);

        let stmt = &stmts[0];
        if let Statement::Expr(e) = &*stmt.value {
            if let Expr::BinaryExpr { lhs, op, rhs } = &**e {
                if let Expr::Value(lhs_val) = &*lhs.value {
                    if let Value::Int(i) = *lhs_val.value {
                        assert_eq!(i, 1);
                    } else {
                        panic!("Left operand not int=1");
                    }
                } else {
                    panic!("Left operand not Expr::Value");
                }
                assert_eq!(*op.value, Operator::Add);
                if let Expr::Value(rhs_val) = &*rhs.value {
                    if let Value::Int(i) = *rhs_val.value {
                        assert_eq!(i, 2);
                    } else {
                        panic!("Right operand not int=2");
                    }
                } else {
                    panic!("Right operand not Expr::Value");
                }
            } else {
                panic!("Expected a BinaryExpr");
            }
        } else {
            panic!("Expected an expression statement");
        }

        let span_text = &input[stmt.span.start..stmt.span.end];
        assert_eq!(span_text, "1+2");
    }

    #[test]
    fn test_parse_binary_expression_with_whitespace() {
        let input = "   10  -  5 ";
        let stmts = parse_complete(input).expect("should parse 10-5");
        assert_eq!(stmts.len(), 1);

        if let Statement::Expr(e) = &*stmts[0].value {
            if let Expr::BinaryExpr { lhs, op, rhs } = &**e {
                if let Expr::Value(lhs_val) = &*lhs.value {
                    if let Value::Int(n) = *lhs_val.value {
                        assert_eq!(n, 10);
                    } else {
                        panic!("Left operand not int=10");
                    }
                } else {
                    panic!("Left operand not Expr::Value");
                }
                assert_eq!(*op.value, Operator::Subtract);
                if let Expr::Value(rhs_val) = &*rhs.value {
                    if let Value::Int(n) = *rhs_val.value {
                        assert_eq!(n, 5);
                    } else {
                        panic!("Right operand not int=5");
                    }
                } else {
                    panic!("Right operand not Expr::Value");
                }
            } else {
                panic!("Expected a BinaryExpr");
            }
        } else {
            panic!("Expected an expression statement");
        }
    }

    #[test]
    fn test_parse_equality_operator() {
        let input = "7==8";
        let stmts = parse_complete(input).expect("should parse 7==8");
        assert_eq!(stmts.len(), 1);

        if let Statement::Expr(e) = &*stmts[0].value {
            if let Expr::BinaryExpr { lhs, op, rhs } = &**e {
                if let Expr::Value(lhs_val) = &*lhs.value {
                    if let Value::Int(n) = *lhs_val.value {
                        assert_eq!(n, 7);
                    } else {
                        panic!("Left side not int=7");
                    }
                } else {
                    panic!("Left side not Expr::Value");
                }
                assert_eq!(*op.value, Operator::Equal);
                if let Expr::Value(rhs_val) = &*rhs.value {
                    if let Value::Int(n) = *rhs_val.value {
                        assert_eq!(n, 8);
                    } else {
                        panic!("Right side not int=8");
                    }
                } else {
                    panic!("Right side not Expr::Value");
                }
            } else {
                panic!("Expected a BinaryExpr");
            }
        } else {
            panic!("Expected expression statement");
        }
    }

    #[test]
    fn test_incomplete_error() {
        let input = "123 = /";
        let result = parse_complete(input);
        assert!(result.is_err());
        match result {
            Err(ParseError::Incomplete { remaining, .. }) => {
                assert_eq!(remaining.trim(), "= /");
            }
            _ => panic!("Expected an Incomplete error variant"),
        }
    }

    /// Verifies that the parser's spanned helper includes only the text for the expression.
    #[test]
    fn test_spanned_text_recovery() {
        let input = "   789";
        let stmts = parse_complete(input).expect("should parse integer 789");
        assert_eq!(stmts.len(), 1);

        let stmt_span = &stmts[0].span;
        let snippet = &input[stmt_span.start..stmt_span.end];
        assert_eq!(snippet, "789");
    }
}
