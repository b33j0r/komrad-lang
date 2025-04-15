use komrad_core::{ParseError, ParserSpan};
use nom::IResult;

// --------------------------------------------
// PResult type alias.
// --------------------------------------------
pub type PResult<'a, T> = IResult<ParserSpan<'a>, T, ParseError>;