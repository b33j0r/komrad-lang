use komrad_core::ParseError;
use komrad_core::{ParserSpan, Span, Spanned};
use nom::IResult;

// --------------------------------------------
// The spanned(...) helper.
// Wrap a parser returning a T into one returning Spanned<T> using
// the consumed byte offsets.
// --------------------------------------------
pub fn spanned<'a, O, F>(
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
