// komrad_core/src/codemap.rs
use crate::ast::{Span, Spanned};
use crate::RuntimeError;
use miette::{miette, LabeledSpan, NamedSource};
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

/// Attached context carried through parsing (in LocatedSpan)
#[derive(Debug, Clone)]
pub struct Context {
    pub file_id: usize,
    pub full_source: Arc<String>,
}

/// Used by nom for span tracking
pub type ParserSpan<'a> = LocatedSpan<&'a str, Context>;

impl<'a> From<ParserSpan<'a>> for Span {
    fn from(span: ParserSpan) -> Self {
        let start = span.location_offset();
        let end = start + span.fragment().len();
        Span {
            file_id: span.extra.file_id,
            start,
            end,
        }
    }
}

impl From<Span> for ParserSpan<'_> {
    fn from(span: Span) -> Self {
        LocatedSpan::new_extra(
            "",
            Context {
                file_id: span.file_id,
                full_source: Arc::new("".to_string()),
            },
        )
    }
}

/// Holds one file's source code and metadata
#[derive(Clone, Debug)]
pub struct CodeMap {
    pub file_id: usize,
    pub file_path: Option<PathBuf>,
    pub source: Arc<String>,
}

impl CodeMap {
    pub fn new(file_id: usize, source: Arc<String>, file_path: Option<PathBuf>) -> Self {
        Self {
            file_id,
            source,
            file_path,
        }
    }

    pub fn parser_span(&self) -> ParserSpan<'_> {
        LocatedSpan::new_extra(
            &self.source[..],
            Context {
                file_id: self.file_id,
                full_source: self.source.clone(),
            },
        )
    }

    pub fn span_range(&self, start: usize, end: usize) -> Span {
        Span {
            file_id: self.file_id,
            start,
            end,
        }
    }
}

/// Holds many CodeMaps and provides file‑ID allocation
#[derive(Debug, Default)]
pub struct CodeAtlas {
    files: HashMap<usize, CodeMap>,
    next_file_id: usize,
}

impl CodeAtlas {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            next_file_id: 0,
        }
    }

    /// Adds a file from &str, stores it as Arc<String>, returns initial ParserSpan
    pub fn add_file(&mut self, source: &str, file_path: Option<PathBuf>) -> ParserSpan<'_> {
        let file_id = self.next_file_id;
        self.next_file_id += 1;

        let arc = Arc::new(source.to_string());
        let map = CodeMap::new(file_id, arc.clone(), file_path);

        self.files.insert(file_id, map);

        // Return a span at offset0 so the caller can continue parsing.
        let span = self.files.get(&file_id).unwrap().parser_span();
        span
    }

    pub fn get_codemap(&self, file_id: usize) -> Option<&CodeMap> {
        self.files.get(&file_id)
    }

    pub fn get_source(&self, span: &Span) -> Option<&str> {
        self.files.get(&span.file_id).map(|map| {
            let s = &map.source;
            let start = span.start.min(s.len());
            let end = span.end.min(s.len());
            &s[start..end]
        })
    }

    // ────────────────────────────────────────────────────────────────
    // New helpers: turn a Spanned<RuntimeError> into a pretty miette report
    // ────────────────────────────────────────────────────────────────

    /// Build a [`miette::NamedSource`] from a span.
    pub fn named_source(&self, span: &Span) -> Option<NamedSource<String>> {
        self.get_codemap(span.file_id).map(|cm| {
            let name = cm
                .file_path
                .as_ref()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|| "<repl>".to_owned());
            NamedSource::new(name, cm.source.as_str().to_owned())
        })
    }

    /// Convert any `Spanned<RuntimeError>` into a [`miette::Report`] with labels.
    // komrad_core/src/codemap.rs
    pub fn report_runtime_error(&self, err: &Spanned<RuntimeError>) -> miette::Report {
        let span = &err.span;
        if let Some(src) = self.named_source(span) {
            let label = LabeledSpan::at(span.start..span.end, err.value.to_string());
            miette!(
            labels = vec![label],
            "{}",                      // primary msg
            err.value
        ).with_source_code(
                src,
            )
        } else {
            miette!("{}", err.value)
        }
    }
}
