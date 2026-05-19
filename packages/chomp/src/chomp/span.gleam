//// Spans are so useful in lexing, parsing, and error reporting that Chomp has
//// a separate module dedicated to them. The nice thing about Spans is you don't
//// have to include them in your parse result right away. If you find that you
//// could use some helpful location information, simply take advantage of the
//// fact that [`chomp.token`](../chomp.html#token) returns the location of the
//// token it consumes, and [`chomp.get_pos`](../chomp.html#get_pos) allows you
//// to attain the location of anything else.
////

// TYPES -----------------------------------------------------------------------

/// A source span is a range into the source string that represents the start and
/// end of a lexeme in a human-readable way. That means instead of a straight index
/// into the string you get a row and column for the start and end instead!
///
pub type Span {
  Span(row_start: Int, col_start: Int, row_end: Int, col_end: Int)
}

// UTILITIES -------------------------------------------------------------------

/// Combine two `Span`s into one that represents the combined range.
///
/// ```txt
///    first blah second
/// 1. ~~~~~   2. ~~~~~~
/// 3. ~~~~~~~~~~~~~~~~~
/// ```
///
/// 1. `Span(1, 1, 1, 6)`.
/// 2. `Span(1, 12, 1, 18)`.
/// 3. (combined) `Span(1, 1, 1, 18)`.
///
/// Often when parsing you'll collect a start and end span that you want to combine
/// and include in the AST. That's where this function is most useful!
///
/// ```
/// use start_span <- do(chomp.token(LParen))
/// // ...
/// use end_span <- do(chomp.token(RParen))
/// let location = span.combine(start_span, end_span)
///
/// return(SomeASTNode(.., location))
/// ```
///
/// 🚨 Note that the second `Span` must come after the first location-wise.
///
pub fn combine(first: Span, second: Span) -> Span {
  Span(first.row_start, first.col_start, second.row_end, second.col_end)
}
