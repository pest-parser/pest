/// A `trait` that defines an input for a `Parser`.
pub trait Input {
    /// Returns length of an `Input`.
    fn len(&self) -> usize;

    /// Returns whether an `Input` is empty.
    fn is_empty(&self) -> bool;

    /// Slices an `Input`.
    fn slice(&self, start: usize, end: usize) -> &str;

    /// Returns the line and column of a position for an `Input`.
    fn line_col(&self, pos: usize) -> (usize, usize);

    /// Matches `string` to an `Input` at `pos`, and returns whether it matched.
    fn match_string(&self, string: &str, pos: usize) -> bool;

    /// Matches `string` to an `Input` at `pos` case insensitively, and returns whether it matched.
    fn match_insensitive(&self, string: &str, pos: usize) -> bool;

    /// Matches if `Input`'s `char` at `pos` is between `left` and `right`.
    fn match_range(&self, left: char, right: char, pos: usize) -> bool;
}
