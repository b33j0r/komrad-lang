use crate::ast::{
    AssignmentTarget, Block, Expr, Handler, Operator, Pattern, Predicate, Statement,
};
use crate::{Spanned, TopLevel};
use std::fmt::Debug;
use std::sync::Arc;

/// A simple S-expression data type. Stores *raw* text (no color codes).
pub enum SExpr {
    Nil,
    Atom(String),
    String(String),
    List(Vec<SExpr>),
}

impl Debug for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // For the Debug output, we'll show the colorized version.
        // You could choose the plain version instead if you prefer.
        write!(f, "{}", self.to_plain_string())
    }
}

impl PartialEq for SExpr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SExpr::Nil, SExpr::Nil) => true,
            (SExpr::Atom(a), SExpr::Atom(b)) => a == b,
            (SExpr::String(a), SExpr::String(b)) => a == b,
            (SExpr::List(a), SExpr::List(b)) => {
                if a.len() != b.len() {
                    false
                } else {
                    a.iter().zip(b.iter()).all(|(x, y)| x == y)
                }
            }
            _ => false,
        }
    }
}

/// A small helper function to colorize one "token."  You can customize
/// these rules to highlight special patterns, capitalized words, etc.
fn colorize_token(token: &str) -> String {
    use owo_colors::OwoColorize;

    // Parens
    if token == "(" || token == ")" {
        return token.bright_magenta().to_string();
    }
    // If it starts and ends with `"`, treat it like a string-literal
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        return token.bright_green().to_string();
    }
    // If first char is uppercase, color it bright_blue
    if token
        .chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
    {
        return token.bright_blue().to_string();
    }
    // Otherwise, default color
    token.white().to_string()
}

impl SExpr {
    /// Produces a plain, uncolored string representation of this SExpr
    /// (suitable for logging, testing, or comparing).
    ///
    /// See also `to_colored_string` for a colorized version.
    pub fn to_plain_string(&self) -> String {
        match self {
            SExpr::Nil => "nil".to_string(),
            SExpr::Atom(a) => format!("{}", a.clone()),
            SExpr::String(s) => format!("\"{}\"", s),
            SExpr::List(items) => {
                let mut out = String::from("(");
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&item.to_plain_string());
                }
                out.push(')');
                out
            }
        }
    }

    /// Produces a colorized string representation of this SExpr,
    /// using the simple `colorize_token` helper function to color
    /// parentheses, string literals, capitalized words, etc.
    ///
    /// See also `to_plain_string` for a plain version.
    pub fn to_colored_string(&self) -> String {
        match self {
            SExpr::Nil => colorize_token("nil"),
            SExpr::Atom(a) => colorize_token(a),
            SExpr::String(s) => colorize_token(&format!("\"{}\"", s)),
            SExpr::List(items) => {
                let mut out = colorize_token("(");
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&item.to_colored_string());
                }
                out.push_str(&colorize_token(")"));
                out
            }
        }
    }

    /// Same as `to_colored_string`, but optionally strip out
    /// ANSI codes for display or logs that must be plain text.
    pub fn to_formated_string_with_options(&self, strip_ansi: bool) -> String {
        let colored = self.to_colored_string();
        if strip_ansi {
            strip_ansi_codes(&colored)
        } else {
            colored
        }
    }
}

/// Helper: Strips ANSI escape sequences from a string.
fn strip_ansi_codes(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // Possibly an ANSI sequence
            if let Some(&next) = chars.peek() {
                if next == '[' {
                    chars.next(); // skip '['
                    // skip until we find a letter in '@'..='~'
                    while let Some(&ch) = chars.peek() {
                        if ('@'..='~').contains(&ch) {
                            chars.next(); // skip the letter
                            break;
                        }
                        chars.next();
                    }
                    continue;
                }
            }
        }
        result.push(c);
    }
    result
}

/// Trait for building an `SExpr` from various AST nodes.
pub trait ToSExpr {
    fn to_sexpr(&self) -> SExpr;
}

// -----------------------------------------------------------------------
// Here we remove all color references from the actual SExpr construction.
// We just store raw strings in Atom(...) or String(...).
// -----------------------------------------------------------------------
impl ToSExpr for String {
    fn to_sexpr(&self) -> SExpr {
        SExpr::String(self.clone())
    }
}

impl ToSExpr for &str {
    fn to_sexpr(&self) -> SExpr {
        SExpr::String(self.to_string())
    }
}

impl<T: ToSExpr> ToSExpr for Option<T> {
    fn to_sexpr(&self) -> SExpr {
        match self {
            Some(x) => x.to_sexpr(),
            None => SExpr::Atom("None".to_string()),
        }
    }
}

impl<T: ToSExpr> ToSExpr for Arc<T> {
    fn to_sexpr(&self) -> SExpr {
        self.as_ref().to_sexpr()
    }
}

impl<T: ToSExpr> ToSExpr for crate::ast::Spanned<T> {
    fn to_sexpr(&self) -> SExpr {
        self.value.to_sexpr()
    }
}

impl ToSExpr for RuntimeError {
    fn to_sexpr(&self) -> SExpr {
        // Just store the raw strings; color is done at display time.
        SExpr::List(vec![
            SExpr::Atom("RuntimeError".to_string()),
            SExpr::Atom(self.to_string()),
        ])
    }
}

impl ToSExpr for TopLevel {
    fn to_sexpr(&self) -> SExpr {
        match self {
            TopLevel::Block(block) => block.to_sexpr(),
            TopLevel::Statement(stmt) => stmt.to_sexpr(),
        }
    }
}

impl ToSExpr for Expr {
    fn to_sexpr(&self) -> SExpr {
        use Expr::*;
        match self {
            Value(v) => v.to_sexpr(),
            Ask { target, value } => SExpr::List(vec![
                SExpr::Atom("Ask".to_string()),
                target.to_sexpr(),
                value.to_sexpr(),
            ]),
            List { elements } => {
                let mut vec = vec![SExpr::Atom("List".to_string())];
                for elem in elements {
                    vec.push(elem.to_sexpr());
                }
                SExpr::List(vec)
            }
            Dict { index_map } => {
                let mut vec = vec![SExpr::Atom("Dict".to_string())];
                for (key, value) in index_map {
                    vec.push(SExpr::List(vec![key.to_sexpr(), value.to_sexpr()]));
                }
                SExpr::List(vec)
            }
            BinaryExpr { lhs, op, rhs } => SExpr::List(vec![
                SExpr::Atom("BinaryExpr".to_string()),
                lhs.to_sexpr(),
                op.to_sexpr(),
                rhs.to_sexpr(),
            ]),
            SliceExpr { target, index } => SExpr::List(vec![
                SExpr::Atom("SliceExpr".to_string()),
                target.to_sexpr(),
                index.to_sexpr(),
            ]),
        }
    }
}

impl ToSExpr for Value {
    fn to_sexpr(&self) -> SExpr {
        use crate::value::Value::*;
        match self {
            Null => SExpr::Atom("null".to_string()),
            Error(e) => SExpr::List(vec![SExpr::Atom("Error".to_string()), e.to_sexpr()]),
            Channel(c) => SExpr::List(vec![
                SExpr::Atom("Channel".to_string()),
                SExpr::Atom(format!("{:?}", c)),
            ]),
            List(list) => {
                let mut sexprs = vec![SExpr::Atom("List".to_string())];
                for v in list {
                    sexprs.push(v.to_sexpr());
                }
                SExpr::List(sexprs)
            }
            Dict(dict) => {
                let mut sexprs = vec![SExpr::Atom("Dict".to_string())];
                for (key, value) in dict {
                    sexprs.push(SExpr::List(vec![
                        SExpr::Atom(key.to_string()),
                        value.to_sexpr(),
                    ]));
                }
                SExpr::List(sexprs)
            }
            Word(w) => SExpr::Atom(w.to_string()),
            Boolean(b) => SExpr::Atom(b.to_string()),
            String(s) => SExpr::String(s.clone()),
            Int(i) => SExpr::Atom(i.to_string()),
            Float(f) => SExpr::Atom(f.to_string()),
            Uuid(u) => SExpr::Atom(u.to_string()),
            Block(arc_block) => arc_block.to_sexpr(),
        }
    }
}

impl ToSExpr for Operator {
    fn to_sexpr(&self) -> SExpr {
        let op_str = match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Mod => "%",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::GreaterThan => ">",
            Operator::LessThan => "<",
        };
        SExpr::Atom(op_str.to_string())
    }
}

impl ToSExpr for Statement {
    fn to_sexpr(&self) -> SExpr {
        use Statement::*;
        match self {
            BlankLine => SExpr::Atom("BlankLine".to_string()),
            Comment(s) => SExpr::List(vec![SExpr::Atom("Comment".to_string()), s.to_sexpr()]),
            Expr(e) => e.to_sexpr(),
            Assign { target, value } => {
                let mut vec = vec![SExpr::Atom("Assign".to_string()), target.to_sexpr()];
                vec.push(value.to_sexpr());
                SExpr::List(vec)
            }
            Tell { target, value } => SExpr::List(vec![
                SExpr::Atom("Tell".to_string()),
                target.to_sexpr(),
                value.to_sexpr(),
            ]),
            Handler(handler_arc) => SExpr::List(vec![
                SExpr::Atom("Handler".to_string()),
                handler_arc.to_sexpr(),
            ]),
            Expand { target } => {
                SExpr::List(vec![SExpr::Atom("Expand".to_string()), target.to_sexpr()])
            }
            InvalidBlock => SExpr::Atom("InvalidBlock".to_string()),
        }
    }
}

impl ToSExpr for Handler {
    fn to_sexpr(&self) -> SExpr {
        SExpr::List(vec![
            SExpr::Atom("HandlerStruct".to_string()),
            self.pattern.to_sexpr(),
            self.expr.to_sexpr(),
        ])
    }
}

impl ToSExpr for Block {
    fn to_sexpr(&self) -> SExpr {
        let mut vec = vec![SExpr::Atom("Block".to_string())];
        for stmt in &self.0 {
            vec.push(stmt.to_sexpr());
        }
        SExpr::List(vec)
    }
}

impl ToSExpr for AssignmentTarget {
    fn to_sexpr(&self) -> SExpr {
        use AssignmentTarget::*;
        match self {
            Variable(s) => SExpr::List(vec![
                SExpr::Atom("Variable".to_string()),
                SExpr::Atom(s.to_string()),
            ]),
            Slice { target, index } => SExpr::List(vec![
                SExpr::Atom("Slice".to_string()),
                target.to_sexpr(),
                index.to_sexpr(),
            ]),
            List { elements } => {
                let mut vec = vec![SExpr::Atom("List".to_string())];
                for elem in elements {
                    vec.push(elem.to_sexpr());
                }
                SExpr::List(vec)
            }
        }
    }
}

impl ToSExpr for Pattern {
    fn to_sexpr(&self) -> SExpr {
        use Pattern::*;
        match self {
            ValueMatch(v) => SExpr::List(vec![SExpr::Atom("ValueMatch".to_string()), v.to_sexpr()]),
            VariableCapture(s) => SExpr::List(vec![
                SExpr::Atom("VariableCapture".to_string()),
                s.to_sexpr(),
            ]),
            BlockCapture(s) => {
                SExpr::List(vec![SExpr::Atom("BlockCapture".to_string()), s.to_sexpr()])
            }
            PredicateCapture(sp_pred) => SExpr::List(vec![
                SExpr::Atom("PredicateCapture".to_string()),
                sp_pred.to_sexpr(),
            ]),
            List(patterns) => {
                let mut vec = vec![SExpr::Atom("PatternList".to_string())];
                for p in patterns {
                    vec.push(p.to_sexpr());
                }
                SExpr::List(vec)
            }
        }
    }
}

impl ToSExpr for Predicate {
    fn to_sexpr(&self) -> SExpr {
        use Predicate::*;
        match self {
            Value(v) => SExpr::List(vec![
                SExpr::Atom("PredicateValue".to_string()),
                v.to_sexpr(),
            ]),
            Variable(s) => SExpr::List(vec![
                SExpr::Atom("PredicateVariable".to_string()),
                s.to_sexpr(),
            ]),
            BinaryExpr { lhs, op, rhs } => SExpr::List(vec![
                SExpr::Atom("PredicateBinary".to_string()),
                lhs.to_sexpr(),
                op.to_sexpr(),
                rhs.to_sexpr(),
            ]),
        }
    }
}

impl ToSExpr for Type {
    fn to_sexpr(&self) -> SExpr {
        let type_str = match self {
            Type::Null => "Null",
            Type::Int => "Int",
            Type::Float => "Float",
            Type::String => "String",
            Type::Boolean => "Boolean",
            Type::List => "List",
            Type::Channel => "Channel",
        };
        SExpr::Atom(type_str.to_string())
    }
}

impl ToSExpr for Vec<Spanned<Statement>> {
    fn to_sexpr(&self) -> SExpr {
        let mut vec = vec![SExpr::Atom("StatementList".to_string())];
        for stmt in self {
            vec.push(stmt.to_sexpr());
        }
        SExpr::List(vec)
    }
}

// ----------------------------------------------------------------------------
// SExpr Parser Logic (unchanged, except we store raw strings in SExpr::Atom).
// ----------------------------------------------------------------------------

use crate::error::RuntimeError;
use crate::value::{Type, Value};
use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, is_not, take_while1};
use nom::character::complete::{char, multispace0, multispace1};
use nom::combinator::{all_consuming, map, opt, value};
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::{IResult, Parser};
use nom_locate::LocatedSpan;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq, Clone)]
pub enum SParseError {
    #[error("Failed to parse S-expression")]
    ParseError,
}

pub type SParserSpan<'input> = LocatedSpan<&'input str>;
pub type SResult<'input, T> = IResult<SParserSpan<'input>, T>;

/// Parse a full S-expression from an input string.
pub fn parse_sexpr(input: &str) -> Result<SExpr, SParseError> {
    let span = SParserSpan::from(input);
    match all_consuming(delimited(multispace0, parse_one_sexpr, multispace0)).parse(span) {
        Ok((_, sexpr)) => Ok(sexpr),
        Err(_) => Err(SParseError::ParseError),
    }
}

/// Parse a single S-expression without consuming surrounding whitespace.
pub fn parse_one_sexpr(input: SParserSpan) -> SResult<SExpr> {
    alt((parse_list, parse_string, parse_atom)).parse(input)
}

/// Parse an S-expression list: '(' + items + ')'.
fn parse_list(input: SParserSpan) -> SResult<SExpr> {
    delimited(
        char('('),
        map(
            delimited(
                multispace0,
                separated_list0(multispace1, parse_one_sexpr),
                multispace0,
            ),
            SExpr::List,
        ),
        char(')'),
    )
        .parse(input)
}

/// Parse a double-quoted string. Basic escapes: \" \\ \n \r \t
fn parse_string(input: SParserSpan) -> SResult<SExpr> {
    let esc = escaped_transform(
        is_not("\"\\"),
        '\\',
        alt((
            value("\"", char('"')),
            value("\\", char('\\')),
            value("\n", char('n')),
            value("\r", char('r')),
            value("\t", char('t')),
        )),
    );
    let (input, content) = delimited(char('"'), opt(esc), char('"'))
        .map(|opt| opt.unwrap_or_else(|| "".to_string()))
        .parse(input)?;
    Ok((input, SExpr::String(content)))
}

/// Parse an atom (word) until whitespace or '(' or ')'.
/// If the atom == "nil", return `SExpr::Nil`, else `SExpr::Atom`.
fn parse_atom(input: SParserSpan) -> SResult<SExpr> {
    let (input, atom_text) =
        take_while1(|c: char| !c.is_whitespace() && c != '(' && c != ')')(input)?;
    let s = atom_text.fragment();
    if *s == "nil" {
        Ok((input, SExpr::Nil))
    } else {
        Ok((input, SExpr::Atom(s.to_string())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sexpr() {
        let input = "(hello (world))";
        let result = parse_sexpr(input);
        assert!(result.is_ok());
        if let Ok(sexpr) = result {
            // We'll compare the plain (uncolored) version
            assert_eq!(sexpr.to_plain_string(), "(hello (world))");
        }
    }

    #[test]
    fn test_parse_nil() {
        let input = "nil";
        let result = parse_sexpr(input);
        assert!(result.is_ok());
        if let Ok(sexpr) = result {
            assert_eq!(sexpr.to_plain_string(), "nil");
        }
    }

    #[test]
    fn test_parse_nested_list() {
        let input = "(a (b c) d)";
        let result = parse_sexpr(input);
        assert!(result.is_ok());
        if let Ok(sexpr) = result {
            assert_eq!(sexpr.to_plain_string(), "(a (b c) d)");
        }
    }

    #[test]
    fn test_parse_with_whitespace_inner() {
        let input = "(  hello   (world)   )";
        let result = parse_sexpr(input);
        assert!(result.is_ok());
        if let Ok(sexpr) = result {
            assert_eq!(sexpr.to_plain_string(), "(hello (world))");
        }
    }

    #[test]
    fn test_parse_with_whitespace_outer() {
        let input = "  (hello (world))  ";
        let result = parse_sexpr(input);
        assert!(result.is_ok());
        if let Ok(sexpr) = result {
            assert_eq!(sexpr.to_plain_string(), "(hello (world))");
        }
    }

    #[test]
    fn test_parse_with_whitespace_everywhere() {
        let input = "  (  hello   (world)   )  ";
        let result = parse_sexpr(input);
        assert!(result.is_ok());
        if let Ok(sexpr) = result {
            assert_eq!(sexpr.to_plain_string(), "(hello (world))");
        }
    }

    #[test]
    fn test_parse_string() {
        let input = r#""hello world""#;
        let result = parse_sexpr(input);
        assert!(result.is_ok());
        if let Ok(sexpr) = result {
            assert_eq!(sexpr.to_plain_string(), "\"hello world\"");
        }
    }
}
