use crate::result::PResult;
use crate::spanned::spanned;
use komrad_core::{ParserSpan, Spanned, Value};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::combinator::map;
use nom::multi::fold_many0;
use nom::Parser;

/// Parses a single, double, or triple quoted string.
pub fn parse_string(input: ParserSpan) -> PResult<String> {
    alt((
        parse_empty_string,
        parse_triple_quoted_string,
        parse_double_quoted_string,
        parse_single_quoted_string,
    ))
        .parse(input)
}

pub fn parse_empty_string(input: ParserSpan) -> PResult<String> {
    alt((tag("''"), tag("\"\"\"\"\"\""), tag("\"\"")))
        .map(|s: ParserSpan| "".to_string())
        .parse(input)
}

/// Parses a single, double, or triple quoted string to a value.
pub fn parse_string_value(input: ParserSpan) -> PResult<Spanned<Value>> {
    spanned(
        |i| {
            parse_string.map(Value::String).parse(i)
        }
    ).parse(input)
}

/// Parses a single-quoted string.
pub fn parse_single_quoted_string(input: ParserSpan) -> PResult<String> {
    let (input, _) = tag("'").parse(input)?;
    let (input, string) = parse_string_inner('\'')(input)?;
    let (input, _) = tag("'").parse(input)?;
    Ok((input, string))
}

/// Parses a double-quoted string.
pub fn parse_double_quoted_string(input: ParserSpan) -> PResult<String> {
    let (input, _) = tag("\"").parse(input)?;
    let (input, string) = parse_string_inner('\"')(input)?;
    let (input, _) = tag("\"").parse(input)?;
    Ok((input, string))
}

/// Parses a triple-quoted string.
pub fn parse_triple_quoted_string(input: ParserSpan) -> PResult<String> {
    let (input, _) = tag("\"\"\"").parse(input)?;
    let (input, string) = parse_triple_string_inner(input)?;
    let (input, _) = tag("\"\"\"").parse(input)?;
    Ok((input, string))
}

/// Optimized function to parse a string until a delimiter.
pub fn parse_string_inner(delimiter: char) -> impl Fn(ParserSpan) -> PResult<String> {
    move |input: ParserSpan| {
        let (mut remaining_input, mut output) = (input, String::new());

        loop {
            let (next_input, chunk) = take_till1(|c| c == delimiter || c == '\\')(remaining_input)?;
            output.push_str(chunk.fragment()); // Bulk copy valid chars

            remaining_input = next_input;

            if remaining_input.fragment().starts_with(delimiter) {
                break; // Stop before consuming the closing delimiter
            }

            if remaining_input.fragment().starts_with('\\') {
                let (next_input, esc) = parse_escape_sequence(remaining_input)?;
                output.push_str(&esc);
                remaining_input = next_input;
            }
        }

        Ok((remaining_input, output))
    }
}

/// Optimized function to parse a triple-quoted string.
pub fn parse_triple_string_inner(input: ParserSpan) -> PResult<String> {
    fold_many0(
        alt((
            // Changed from map_res to map since this transformation can't fail
            map(take_till1(|c| c == '\\' || c == '"'), |s: ParserSpan| {
                s.fragment().to_string()
            }),
            parse_escape_sequence,
        )),
        String::new,
        |mut acc, item| {
            acc.push_str(&item);
            acc
        },
    )
        .parse(input)
}

/// Parses an escape sequence (e.g. `\n`, `\t`, `\\`).
pub fn parse_escape_sequence(input: ParserSpan) -> PResult<String> {
    let (input, _) = tag("\\").parse(input)?;
    let (input, esc) =
        alt((tag("n"), tag("t"), tag("r"), tag("\""), tag("'"), tag("\\"))).parse(input)?;

    let replaced = match *esc.fragment() {
        "n" => "\n",
        "t" => "\t",
        "r" => "\r",
        "\"" => "\"",
        "'" => "'",
        "\\" => "\\",
        _ => unreachable!(),
    };

    Ok((input, replaced.to_string()))
}
