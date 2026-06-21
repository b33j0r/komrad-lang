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
    collect_handler_syntax(source, &mut tokens);
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
            push_handler_delimiters(source, &statement.span, &handler.pattern.span, tokens);
            collect_handler_pattern(source, &handler.pattern, tokens);
            push_block_braces(source, &handler.expr.span, tokens);
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

fn collect_handler_syntax(source: &str, tokens: &mut Vec<HighlightToken>) {
    let bytes = source.as_bytes();
    let mut index = 0;

    while index < bytes.len() {
        if bytes[index] != b'[' {
            index += 1;
            continue;
        }

        let Some(close) = source[index + 1..]
            .find(']')
            .map(|offset| index + 1 + offset)
        else {
            break;
        };
        let after_close = skip_ascii_whitespace(source, close + 1);
        if source.as_bytes().get(after_close) != Some(&b'{') {
            index = close + 1;
            continue;
        }

        push_range(source, "punctuation.bracket", index, index + 1, tokens);
        collect_handler_pattern_text(source, index + 1, close, tokens);
        push_range(source, "punctuation.bracket", close, close + 1, tokens);
        push_range(
            source,
            "punctuation.bracket",
            after_close,
            after_close + 1,
            tokens,
        );
        if let Some(close_brace) = matching_close_brace(source, after_close) {
            push_range(
                source,
                "punctuation.bracket",
                close_brace,
                close_brace + 1,
                tokens,
            );
        }

        index = close + 1;
    }
}

fn collect_handler_pattern_text(
    source: &str,
    start: usize,
    end: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    let mut index = start;
    while index < end {
        index = skip_ascii_whitespace_until(source, index, end);
        if index >= end {
            break;
        }

        if source[index..end].starts_with("_{") {
            let Some(close) = source[index + 2..end]
                .find('}')
                .map(|offset| index + 2 + offset)
            else {
                break;
            };
            push_range(source, "punctuation.delimiter", index, index + 1, tokens);
            push_range(source, "punctuation.bracket", index + 1, index + 2, tokens);
            push_range(source, "capture.block", index + 2, close, tokens);
            push_range(source, "punctuation.bracket", close, close + 1, tokens);
            index = close + 1;
            continue;
        }

        if source[index..end].starts_with("_(") {
            let Some(close) = source[index + 2..end]
                .find(')')
                .map(|offset| index + 2 + offset)
            else {
                break;
            };
            push_range(source, "punctuation.delimiter", index, index + 1, tokens);
            push_range(source, "punctuation.bracket", index + 1, index + 2, tokens);
            collect_predicate_text(source, index + 2, close, tokens);
            push_range(source, "punctuation.bracket", close, close + 1, tokens);
            index = close + 1;
            continue;
        }

        if source.as_bytes()[index] == b'_' {
            let name_start = index + 1;
            let name_end = identifier_end(source, name_start, end);
            if name_end > name_start {
                push_range(source, "punctuation.delimiter", index, index + 1, tokens);
                push_range(source, "capture.variable", name_start, name_end, tokens);
                index = name_end;
                continue;
            }
        }

        let token_end = pattern_token_end(source, index, end);
        collect_handler_atom_text(source, index, token_end, tokens);
        index = token_end;
    }
}

fn collect_predicate_text(
    source: &str,
    start: usize,
    end: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    let mut index = start;
    while index < end {
        index = skip_ascii_whitespace_until(source, index, end);
        if index >= end {
            break;
        }

        let byte = source.as_bytes()[index];
        if byte.is_ascii_digit() {
            let token_end = number_end(source, index, end);
            push_range(source, "predicate.number", index, token_end, tokens);
            index = token_end;
        } else if byte.is_ascii_alphabetic() || byte == b'_' {
            let token_end = identifier_end(source, index, end);
            let capture = match &source[index..token_end] {
                "true" | "false" => "predicate.keyword",
                _ => "predicate.variable",
            };
            push_range(source, capture, index, token_end, tokens);
            index = token_end;
        } else if matches!(
            byte,
            b'>' | b'<' | b'=' | b'!' | b'+' | b'-' | b'*' | b'/' | b'%'
        ) {
            let token_end = operator_end(source, index, end);
            push_range(source, "predicate.operator", index, token_end, tokens);
            index = token_end;
        } else if matches!(byte, b'\'' | b'"') {
            let token_end = quoted_string_end(source, index, end);
            push_range(source, "predicate.string", index, token_end, tokens);
            index = token_end;
        } else {
            index += 1;
        }
    }
}

fn collect_handler_atom_text(
    source: &str,
    start: usize,
    end: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    if start >= end {
        return;
    }

    let token = &source[start..end];
    if token.as_bytes()[0].is_ascii_digit() {
        push_range(source, "number", start, end, tokens);
    } else if matches!(token, "true" | "false") {
        push_range(source, "keyword", start, end, tokens);
    } else if token.starts_with('"') || token.starts_with('\'') {
        push_range(source, "string", start, end, tokens);
    } else {
        push_range(source, "handler", start, end, tokens);
    }
}

fn collect_handler_pattern(
    source: &str,
    pattern: &Spanned<Pattern>,
    tokens: &mut Vec<HighlightToken>,
) {
    match &*pattern.value {
        Pattern::ValueMatch(value) => collect_handler_pattern_value(source, value, tokens),
        Pattern::VariableCapture(name) => {
            push_capture_prefix(source, &name.span, tokens);
            push_capture_name(source, "capture.variable", &name.span, tokens);
        }
        Pattern::BlockCapture(name) => {
            push_block_capture_delimiters(source, &name.span, tokens);
            push_capture_name(source, "capture.block", &name.span, tokens);
        }
        Pattern::PredicateCapture(predicate) => {
            push_predicate_capture_delimiters(source, &predicate.span, tokens);
            collect_predicate(source, predicate, tokens);
        }
        Pattern::List(patterns) => {
            for pattern in patterns {
                collect_handler_pattern(source, pattern, tokens);
            }
        }
    }
}

fn collect_handler_pattern_value(
    source: &str,
    value: &Spanned<Value>,
    tokens: &mut Vec<HighlightToken>,
) {
    match &*value.value {
        Value::Word(_) => push_span(source, "handler", &value.span, tokens),
        Value::Boolean(_) => push_span(source, "keyword", &value.span, tokens),
        Value::String(_) => push_span(source, "string", &value.span, tokens),
        Value::Int(_) | Value::Float(_) => push_span(source, "number", &value.span, tokens),
        _ => collect_value(source, value, tokens),
    }
}

fn collect_predicate(
    source: &str,
    predicate: &Spanned<komrad_core::Predicate>,
    tokens: &mut Vec<HighlightToken>,
) {
    match &*predicate.value {
        komrad_core::Predicate::Value(Value::Boolean(_)) => {
            push_span(source, "predicate.keyword", &predicate.span, tokens);
        }
        komrad_core::Predicate::Value(Value::Int(_) | Value::Float(_)) => {
            push_span(source, "predicate.number", &predicate.span, tokens);
        }
        komrad_core::Predicate::Value(Value::String(_)) => {
            push_span(source, "predicate.string", &predicate.span, tokens);
        }
        komrad_core::Predicate::Value(Value::Word(_)) => {
            push_span(source, "handler", &predicate.span, tokens);
        }
        komrad_core::Predicate::Variable(_) => {
            push_span(source, "predicate.variable", &predicate.span, tokens);
        }
        komrad_core::Predicate::BinaryExpr { lhs, op, rhs } => {
            collect_predicate(source, lhs, tokens);
            push_span(source, "predicate.operator", &op.span, tokens);
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

fn push_handler_delimiters(
    source: &str,
    statement_span: &Span,
    pattern_span: &Span,
    tokens: &mut Vec<HighlightToken>,
) {
    push_char_before(
        source,
        "punctuation.bracket",
        '[',
        pattern_span.start,
        statement_span.start,
        tokens,
    );
    push_char_after(
        source,
        "punctuation.bracket",
        ']',
        pattern_span.end,
        statement_span.end,
        tokens,
    );
}

fn push_block_braces(source: &str, block_span: &Span, tokens: &mut Vec<HighlightToken>) {
    push_char_at_or_after(
        source,
        "punctuation.bracket",
        '{',
        block_span.start,
        block_span.end,
        tokens,
    );
    push_char_before(
        source,
        "punctuation.bracket",
        '}',
        block_span.end,
        block_span.start,
        tokens,
    );
}

fn push_capture_prefix(source: &str, capture_span: &Span, tokens: &mut Vec<HighlightToken>) {
    push_char_at_or_before(
        source,
        "punctuation.delimiter",
        '_',
        capture_span.start,
        tokens,
    );
}

fn push_capture_name(
    source: &str,
    capture: &str,
    capture_span: &Span,
    tokens: &mut Vec<HighlightToken>,
) {
    let mut start = capture_span.start;
    let mut end = capture_span.end;

    if source[start..end].starts_with("_{") {
        start += 2;
    } else if source[start..end].starts_with('_') {
        start += 1;
    }
    if start < end && source[start..end].ends_with('}') {
        end -= 1;
    }

    push_range(source, capture, start, end, tokens);
}

fn push_block_capture_delimiters(
    source: &str,
    capture_span: &Span,
    tokens: &mut Vec<HighlightToken>,
) {
    push_char_before(
        source,
        "punctuation.delimiter",
        '_',
        capture_span.start,
        0,
        tokens,
    );
    push_char_before(
        source,
        "punctuation.bracket",
        '{',
        capture_span.start,
        0,
        tokens,
    );
    push_char_after(
        source,
        "punctuation.bracket",
        '}',
        capture_span.end,
        source.len(),
        tokens,
    );
}

fn push_predicate_capture_delimiters(
    source: &str,
    predicate_span: &Span,
    tokens: &mut Vec<HighlightToken>,
) {
    push_char_before(
        source,
        "punctuation.delimiter",
        '_',
        predicate_span.start,
        0,
        tokens,
    );
    push_char_before(
        source,
        "punctuation.bracket",
        '(',
        predicate_span.start,
        0,
        tokens,
    );
    push_char_after(
        source,
        "punctuation.bracket",
        ')',
        predicate_span.end,
        source.len(),
        tokens,
    );
}

fn push_char_at_or_before(
    source: &str,
    capture: &str,
    needle: char,
    end: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    if source[end..].starts_with(needle) {
        push_range(source, capture, end, end + needle.len_utf8(), tokens);
        return;
    }
    push_char_before(source, capture, needle, end, end.saturating_sub(1), tokens);
}

fn push_char_before(
    source: &str,
    capture: &str,
    needle: char,
    end: usize,
    floor: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    let floor = floor.min(end);
    for (relative_index, ch) in source[floor..end].char_indices().rev() {
        if ch == needle {
            let start = floor + relative_index;
            push_range(source, capture, start, start + ch.len_utf8(), tokens);
            return;
        }
        if !ch.is_whitespace() {
            return;
        }
    }
}

fn push_char_at_or_after(
    source: &str,
    capture: &str,
    needle: char,
    start: usize,
    ceiling: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    push_char_after(source, capture, needle, start, ceiling, tokens);
}

fn push_char_after(
    source: &str,
    capture: &str,
    needle: char,
    start: usize,
    ceiling: usize,
    tokens: &mut Vec<HighlightToken>,
) {
    let ceiling = ceiling.min(source.len());
    for (relative_index, ch) in source[start..ceiling].char_indices() {
        if ch == needle {
            let token_start = start + relative_index;
            push_range(
                source,
                capture,
                token_start,
                token_start + ch.len_utf8(),
                tokens,
            );
            return;
        }
        if !ch.is_whitespace() {
            return;
        }
    }
}

fn push_span(source: &str, capture: &str, span: &Span, tokens: &mut Vec<HighlightToken>) {
    push_range(source, capture, span.start, span.end, tokens);
}

fn skip_ascii_whitespace(source: &str, start: usize) -> usize {
    skip_ascii_whitespace_until(source, start, source.len())
}

fn skip_ascii_whitespace_until(source: &str, start: usize, end: usize) -> usize {
    let mut index = start;
    while index < end && source.as_bytes()[index].is_ascii_whitespace() {
        index += 1;
    }
    index
}

fn identifier_end(source: &str, start: usize, end: usize) -> usize {
    let mut index = start;
    while index < end {
        let byte = source.as_bytes()[index];
        if byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-') {
            index += 1;
        } else {
            break;
        }
    }
    index
}

fn number_end(source: &str, start: usize, end: usize) -> usize {
    let mut index = start;
    while index < end && matches!(source.as_bytes()[index], b'0'..=b'9' | b'.' | b'_') {
        index += 1;
    }
    index
}

fn operator_end(source: &str, start: usize, end: usize) -> usize {
    let mut index = start;
    while index < end
        && matches!(
            source.as_bytes()[index],
            b'>' | b'<' | b'=' | b'!' | b'+' | b'-' | b'*' | b'/' | b'%'
        )
    {
        index += 1;
    }
    index
}

fn quoted_string_end(source: &str, start: usize, end: usize) -> usize {
    let quote = source.as_bytes()[start];
    let mut index = start + 1;
    while index < end {
        if source.as_bytes()[index] == b'\\' {
            index = (index + 2).min(end);
            continue;
        }
        if source.as_bytes()[index] == quote {
            return index + 1;
        }
        index += 1;
    }
    end
}

fn pattern_token_end(source: &str, start: usize, end: usize) -> usize {
    if matches!(source.as_bytes()[start], b'\'' | b'"') {
        return quoted_string_end(source, start, end);
    }

    let mut index = start;
    while index < end && !source.as_bytes()[index].is_ascii_whitespace() {
        index += 1;
    }
    index
}

fn matching_close_brace(source: &str, open: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut index = open;
    while index < source.len() {
        match source.as_bytes()[index] {
            b'{' => depth += 1,
            b'}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(index);
                }
            }
            _ => {}
        }
        index += 1;
    }
    None
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
[greet _name _(count > 3) _{body}] {
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
        assert!(captures.contains(&("handler", "greet")));
        assert!(captures.contains(&("capture.variable", "name")));
        assert!(captures.contains(&("capture.block", "body")));
        assert!(captures.contains(&("predicate.variable", "count")));
        assert!(captures.contains(&("predicate.operator", ">")));
        assert!(captures.contains(&("predicate.number", "3")));
        assert!(captures.contains(&("punctuation.delimiter", "_")));
        assert!(captures.contains(&("punctuation.bracket", "[")));
        assert!(captures.contains(&("punctuation.bracket", "]")));
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

    #[test]
    fn highlights_nested_handler_after_assignment() {
        let source = r#"Actor = {
state = null

[start] {
}
}
"#;

        let tokens = parse_tokens(source).expect("source should parse");
        let captures: Vec<_> = tokens
            .iter()
            .map(|token| (token.capture.as_str(), token.text.as_str()))
            .collect();

        assert!(captures.contains(&("handler", "start")), "{captures:#?}");
    }
}
