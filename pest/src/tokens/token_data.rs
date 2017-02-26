/// A `struct` that captures the essential data of a `Token` pair.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TokenData<Rule> {
    /// The `Rule` of the `Token` pair
    pub rule:  Rule,
    /// The start position
    pub start: usize,
    /// The end position
    pub end:   usize
}
