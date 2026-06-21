use komrad_core::{
    AssignmentTarget, Block, CodeAtlas, Expr, Operator, Pattern, Span, Spanned, Statement,
    TopLevel, Value,
};
use komrad_parser::parse_toplevel::parse_file_complete;
use palimpsest::{HighlightToken, parse_error_json, parse_success_json, token_from_byte_range};
use wasm_bindgen::prelude::wasm_bindgen;

pub fn parse_tokens(source: &str) -> Result<Vec<HighlightToken>, String> {
    let mut codemaps = CodeAtlas::new();
    let top_level =
        parse_file_complete(&mut codemaps, source, None).map_err(|error| error.to_string())?;

    let mut tokens = Vec::new();
    collect_top_level(source, &top_level, &mut tokens);
    sort_and_dedupe(&mut tokens);
    Ok(tokens)
}

pub fn parse_tokens_json(source: &str) -> String {
    match parse_tokens(source) {
        Ok(tokens) => parse_success_json(&tokens),
        Err(error) => parse_error_json(error),
    }
}

#[wasm_bindgen]
pub fn parse_to_json(source: &str) -> String {
    parse_tokens_json(source)
}

fn collect_top_level(source: &str, top_level: &TopLevel, tokens: &mut Vec<HighlightToken>) {
    match top_level {
        TopLevel::Statement(statement) => collect_statement(source, statement, tokens),
        TopLevel::Block(block) => collect_block(source, block, tokens),
    }
}

fn collect_block(source: &str, block: &Block, tokens: &mut Vec<HighlightToken>) {
    for statement in &block.0 {
        collect_statement(source, statement, tokens);
    }
}

fn collect_statement(
    source: &str,
    statement: &Spanned<Statement>,
    tokens: &mut Vec<HighlightToken>,
) {
    match &*statement.value {
        Statement::BlankLine | Statement::InvalidBlock => {}
        Statement::Comment(_) => push_span(source, "comment", &statement.span, tokens),
        Statement::Expr(expr) => collect_expr(source, expr, tokens),
        Statement::Assign { target, value } => {
            collect_assignment_target(source, target, tokens);
            push_assignment_operator(source, &target.span, &value.span, tokens);
            collect_expr(source, value, tokens);
        }
        Statement::Tell { target, value } => {
            collect_expr(source, target, tokens);
            collect_expr(source, value, tokens);
        }
        Statement::Handler(handler) => {
            collect_pattern(source, &handler.pattern, tokens);
            collect_expr(source, &handler.expr, tokens);
        }
    }
}

fn collect_assignment_target(
    source: &str,
    target: &Spanned<AssignmentTarget>,
    tokens: &mut Vec<HighlightToken>,
) {
    match &*target.value {
        AssignmentTarget::Variable(_) => push_span(source, "symbol", &target.span, tokens),
        AssignmentTarget::Slice { target, index } => {
            collect_assignment_target(source, target, tokens);
            collect_expr(source, index, tokens);
        }
        AssignmentTarget::List { elements } => {
            for element in elements {
                collect_assignment_target(source, element, tokens);
            }
        }
    }
}

fn collect_expr(source: &str, expr: &Spanned<Expr>, tokens: &mut Vec<HighlightToken>) {
    match &*expr.value {
        Expr::Value(value) => collect_value(source, value, tokens),
        Expr::Ask { target, value } => {
            collect_expr(source, target, tokens);
            collect_expr(source, value, tokens);
        }
        Expr::List { elements } => {
            for element in elements {
                collect_expr(source, element, tokens);
            }
        }
        Expr::Dict { index_map } => {
            for value in index_map.values() {
                collect_expr(source, value, tokens);
            }
        }
        Expr::BinaryExpr { lhs, op, rhs } => {
            collect_expr(source, lhs, tokens);
            collect_operator(source, op, tokens);
            collect_expr(source, rhs, tokens);
        }
        Expr::SliceExpr { target, index } => {
            collect_expr(source, target, tokens);
            collect_expr(source, index, tokens);
        }
        Expr::Expander { target } => collect_expr(source, target, tokens),
    }
}

fn collect_value(source: &str, value: &Spanned<Value>, tokens: &mut Vec<HighlightToken>) {
    match &*value.value {
        Value::Word(_) => push_span(source, "symbol", &value.span, tokens),
        Value::Boolean(_) => push_span(source, "keyword", &value.span, tokens),
        Value::String(_) => push_span(source, "string", &value.span, tokens),
        Value::Int(_) | Value::Float(_) => push_span(source, "number", &value.span, tokens),
        Value::Block(block) => collect_block(source, block, tokens),
        Value::List(values) => {
            for value in values {
                collect_unspanned_value(source, value, tokens);
            }
        }
        Value::Dict(dict) => {
            for value in dict.values() {
                collect_unspanned_value(source, value, tokens);
            }
        }
        Value::Null
        | Value::Error(_)
        | Value::Channel(_)
        | Value::Uuid(_)
        | Value::RemoteError(_)
        | Value::Bytes(_) => {}
    }
}

fn collect_unspanned_value(_source: &str, _value: &Value, _tokens: &mut Vec<HighlightToken>) {}

fn collect_pattern(source: &str, pattern: &Spanned<Pattern>, tokens: &mut Vec<HighlightToken>) {
    match &*pattern.value {
        Pattern::ValueMatch(value) => collect_value(source, value, tokens),
        Pattern::VariableCapture(name) | Pattern::BlockCapture(name) => {
            push_span(source, "symbol", &name.span, tokens);
        }
        Pattern::PredicateCapture(predicate) => collect_predicate(source, predicate, tokens),
        Pattern::List(patterns) => {
            for pattern in patterns {
                collect_pattern(source, pattern, tokens);
            }
        }
    }
}

fn collect_predicate(
    source: &str,
    predicate: &Spanned<komrad_core::Predicate>,
    tokens: &mut Vec<HighlightToken>,
) {
    match &*predicate.value {
        komrad_core::Predicate::Value(Value::Boolean(_)) => {
            push_span(source, "keyword", &predicate.span, tokens);
        }
        komrad_core::Predicate::Value(Value::Int(_) | Value::Float(_)) => {
            push_span(source, "number", &predicate.span, tokens);
        }
        komrad_core::Predicate::Value(Value::String(_)) => {
            push_span(source, "string", &predicate.span, tokens);
        }
        komrad_core::Predicate::Value(Value::Word(_)) | komrad_core::Predicate::Variable(_) => {
            push_span(source, "symbol", &predicate.span, tokens);
        }
        komrad_core::Predicate::BinaryExpr { lhs, op, rhs } => {
            collect_predicate(source, lhs, tokens);
            collect_operator(source, op, tokens);
            collect_predicate(source, rhs, tokens);
        }
        komrad_core::Predicate::Value(_) => {}
    }
}

fn collect_operator(source: &str, op: &Spanned<Operator>, tokens: &mut Vec<HighlightToken>) {
    push_span(source, "operator", &op.span, tokens);
}

fn push_assignment_operator(
    source: &str,
    target_span: &Span,
    value_span: &Span,
    tokens: &mut Vec<HighlightToken>,
) {
    if target_span.file_id != value_span.file_id || target_span.end > value_span.start {
        return;
    }

    let gap = &source[target_span.end..value_span.start];
    if let Some(relative_start) = gap.find('=') {
        let start = target_span.end + relative_start;
        push_range(source, "operator", start, start + 1, tokens);
    }
}

fn push_span(source: &str, capture: &str, span: &Span, tokens: &mut Vec<HighlightToken>) {
    push_range(source, capture, span.start, span.end, tokens);
}

fn push_range(
    source: &str,
    capture: &str,
    start: usize,
    end: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    if start < end && end <= source.len() {
        tokens.push(token_from_byte_range(source, capture, start, end));
    }
}

fn sort_and_dedupe(tokens: &mut Vec<HighlightToken>) {
    tokens.sort_by(|left, right| {
        (left.start, left.end, left.capture.as_str()).cmp(&(
            right.start,
            right.end,
            right.capture.as_str(),
        ))
    });
    tokens.dedup_by(|left, right| {
        left.start == right.start && left.end == right.end && left.capture == right.capture
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_official_komrad_into_highlight_tokens() {
        let source = r#"# greet on command
[greet _name] {
message = "hello"
done = true
}
"#;

        let tokens = parse_tokens(source).expect("source should parse");
        let captures: Vec<_> = tokens
            .iter()
            .map(|token| (token.capture.as_str(), token.text.as_str()))
            .collect();

        assert!(captures.contains(&("comment", "# greet on command")));
        assert!(captures.contains(&("symbol", "greet")));
        assert!(captures.contains(&("symbol", "_name")));
        assert!(captures.contains(&("symbol", "message")));
        assert!(captures.contains(&("operator", "=")));
        assert!(captures.contains(&("string", r#""hello""#)));
        assert!(captures.contains(&("keyword", "true")));
    }

    #[test]
    fn returns_error_json_for_invalid_source() {
        let parsed: serde_json::Value = serde_json::from_str(&parse_tokens_json("@")).unwrap();

        assert_eq!(parsed["ok"], false);
        assert!(
            parsed["error"]
                .as_str()
                .unwrap()
                .contains("Incomplete input")
        );
    }
}
