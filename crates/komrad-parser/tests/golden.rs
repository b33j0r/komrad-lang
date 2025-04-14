use glob;
use komrad_core::{parse_sexpr, CodeMaps, ParseError, SParseError, ToSExpr};
use komrad_parser::parser::parse_snippet_complete;
use miette::NamedSource;
use nom::{
    branch::alt, bytes::complete::{tag, take_until},
    character::complete::{newline, not_line_ending},
    combinator::{complete, rest},
    multi::{many0, separated_list0},
    IResult,
    Parser,
};
use nom_locate::LocatedSpan;
use owo_colors::OwoColorize;
use std::path::Path;
use std::sync::Arc;
use thiserror::Error;
use tracing::info;

/// Represents an individual test case from a `.test` file.
#[derive(Debug, Clone)]
pub struct TestCase {
    pub title: String,
    pub source: String,
    pub expected: String,
}

#[derive(Debug, Clone, Error)]
pub enum ExpectedDidNotMatchError {
    ExpectedDidNotMatch(String, String),
}

impl std::fmt::Display for ExpectedDidNotMatchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedDidNotMatchError::ExpectedDidNotMatch(expected, actual) => {
                write!(
                    f,
                    "Expected: {}\nActual:   {}",
                    expected.green(),
                    actual.red()
                )
            }
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum TestCaseError {
    #[error("{0:?}")]
    ParseExpected(SParseError),

    #[error("{0:?}")]
    ParseSource(ParseError),

    #[error("{0:?}")]
    Comparison(#[from] ExpectedDidNotMatchError),
}

impl TestCase {
    pub(crate) fn run(&self) -> Result<(), TestCaseError> {
        let mut codemaps = CodeMaps::new();

        // 1) Parse the source code as KExpr
        let toplevel = parse_snippet_complete(&mut codemaps, &self.source).map_err(TestCaseError::ParseSource)?;

        // 2) Convert that KExpr to an SExpr
        let actual_sexpr = toplevel.to_sexpr();

        // 3) Parse the expected text as an SExpr
        let expected_sexpr =
            parse_sexpr(&self.expected).map_err(TestCaseError::ParseExpected)?;

        // 4) Compare
        if actual_sexpr == expected_sexpr {
            Ok(())
        } else {
            Err(ExpectedDidNotMatchError::ExpectedDidNotMatch(
                format!("{:#?}", expected_sexpr),
                format!("{:#?}", actual_sexpr),
            )
                .into())
        }
    }
}

/// Represents a test suite: may be a directory (with children) or a single file (with test cases).
#[derive(Debug, Clone)]
pub struct TestSuite {
    /// All test cases within a file.
    pub test_cases: Vec<TestCase>,
    /// Nested subdirectories, each with its own `TestSuite`.
    pub children: Vec<TestSuite>,
}

/// Parser context for better error reporting.
#[derive(Debug, Clone)]
pub struct SuiteContext {
    /// The source file name and contents.
    #[allow(dead_code)]
    named_source: Arc<NamedSource<String>>,
}
pub type SuiteSpan<'input> = LocatedSpan<&'input str, SuiteContext>;
pub type SuiteResult<'input, T> = IResult<SuiteSpan<'input>, T>;

pub fn parse_test_suite<'a>(root_dir: &Path) -> SuiteResult<'a, TestSuite> {
    info!("Root dir: {}", root_dir.display());

    let glob_pattern = format!("{}/**/*.kom", root_dir.display());
    info!("Parsing glob pattern: {}", glob_pattern);

    // We'll accumulate test cases from files, plus child suites for subdirectories.
    let mut suite = TestSuite {
        test_cases: Vec::new(),
        children: Vec::new(),
    };

    for entry in glob::glob(&glob_pattern).expect("Failed to iterate glob pattern") {
        if let Ok(path) = entry {
            let file_contents = std::fs::read_to_string(&path).unwrap();
            let named_source = Arc::new(NamedSource::new(
                path.to_string_lossy().to_string(),
                file_contents.clone(),
            ));

            let span = SuiteSpan::new_extra(file_contents.as_str(), SuiteContext { named_source });

            // Parse the file into multiple test cases
            let (remaining, test_cases) = parse_test_content(span).unwrap();

            assert_eq!(
                remaining.fragment().to_string(),
                "",
                "Did not consume entire test file"
            );

            // Each file is its own suite with test cases
            let file_suite = TestSuite {
                test_cases,
                children: Vec::new(),
            };
            suite.children.push(file_suite);
        }
    }

    // Return a "dummy" span (no actual remaining input) and our accumulated suite
    let dummy_span = SuiteSpan::new_extra(
        "",
        SuiteContext {
            named_source: Arc::new(NamedSource::new("dummy".to_string(), "".into())),
        },
    );
    Ok((dummy_span, suite))
}

/// Parses the entire content of a single test file into a vector of `TestCase`.
fn parse_test_content(input: SuiteSpan) -> SuiteResult<Vec<TestCase>> {
    // Skip leading newlines
    let (input, _) = many0(newline).parse(input)?;
    // Each test case is separated by zero-or-more newlines
    separated_list0(many0(newline), parse_test_case).parse(input)
}

/// A helper that parses the test-case header line ("====" plus title).
fn parse_header(input: SuiteSpan) -> SuiteResult<String> {
    let (input, _) = complete(tag("====")).parse(input)?;
    let (input, title_line) = not_line_ending(input)?;
    let title_str = title_line.fragment().trim().to_string();

    // Consume the newline after the title
    let (input, _) = newline(input)?;
    Ok((input, title_str))
}

/// A helper that discards any blank lines (newlines).
fn parse_blank_lines(input: SuiteSpan) -> SuiteResult<()> {
    let (input, _) = many0(newline).parse(input)?;
    Ok((input, ()))
}

/// A helper that parses the source block, which ends at a line with "----".
fn parse_source_block(input: SuiteSpan) -> SuiteResult<String> {
    // Grab everything until "\n----"
    let (input, src_span) = complete(take_until("\n----")).parse(input)?;
    // Trim for cleanliness
    Ok((input, src_span.fragment().trim().to_string()))
}

/// A helper that parses the "----" delimiter line plus its trailing newline.
fn parse_delimiter(input: SuiteSpan) -> SuiteResult<()> {
    // The delimiter line is always `\\n----` (start-of-line + \"----\")
    let (input, _) = complete(tag("\n----")).parse(input)?;
    let (input, _) = newline(input)?;
    Ok((input, ()))
}

/// A helper that parses the expected block, which goes until the next "====" or end-of-file.
fn parse_expected_block(input: SuiteSpan) -> SuiteResult<String> {
    let (input, exp_span) = alt((
        complete(take_until("\n====")),
        rest, // If there's no further \"====\", parse everything to the end
    ))
        .parse(input)?;
    Ok((input, exp_span.fragment().trim().to_string()))
}

/// Parses a single test case, of the form:
///
/// ```text
/// ==== Title here
/// <source block>
/// ----
/// <expected block>
/// ```
fn parse_test_case(input: SuiteSpan) -> SuiteResult<TestCase> {
    // 1) Parse the "==== Some title" line
    let (input, title) = parse_header(input)?;

    // 2) Eat any blank lines between the title and the source
    let (input, _) = parse_blank_lines(input)?;

    // 3) Source block ends at "----" delimiter
    let (input, source) = parse_source_block(input)?;

    // 4) Parse the "----" line
    let (input, _) = parse_delimiter(input)?;

    // 5) Eat any blank lines between "----" and expected block
    let (input, _) = parse_blank_lines(input)?;

    // 6) Expected block ends at next "====" or end-of-file
    let (input, expected) = parse_expected_block(input)?;

    // Build the test case object
    let test_case = TestCase {
        title,
        source,
        expected,
    };
    Ok((input, test_case))
}


use std::path::PathBuf;

#[test]
fn golden_tests() -> Result<(), Box<dyn std::error::Error>> {
    let path = PathBuf::from("tests");
    let parse_result = parse_test_suite(&path)?;
    assert!(
        parse_result.1.children.len() > 0,
        "Should have at least one test file"
    );

    let mut failures = Vec::new();
    let mut todos = Vec::new();

    for suite in parse_result.1.children {
        for test_case in suite.test_cases {
            if test_case.title.contains("TODO") {
                println!("📝 {}", test_case.title);
                todos.push(test_case);
                continue;
            }
            match test_case.run() {
                Ok(_) => println!("✅ {}", test_case.title.green()),
                Err(e) => {
                    println!("❌ {}", test_case.title.red());
                    failures.push((test_case, e));
                }
            }
        }
    }

    for (failure, error) in &failures {
        println!("==== {}", failure.title.bright_cyan());
        match error {
            TestCaseError::ParseExpected(pe) => {
                println!("Expected: {}", pe.clone())
            }
            TestCaseError::ParseSource(ps) => println!("Source: {}", ps.clone()),
            TestCaseError::Comparison(ce) => println!("{}", ce),
        }
    }

    assert_eq!(failures.len(), 0, "There were test failures");
    Ok(())
}
