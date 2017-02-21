/// A `struct` that captures the essential data of a `Token` pair.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TokenData<'a, Rule> {
    /// The `Rule` of the `Token` pair
    pub rule:  Rule,
    /// The start position
    pub start: usize,
    /// The end position
    pub end:   usize,
    /// The `Input` slice captured by the `Token` pair
    pub slice: &'a str
}
