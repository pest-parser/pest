use super::inputs::Input;
use super::token_stream::TokenStream;

/// A `trait` that defines a `Parser`.
pub trait Parser<Rule> {
    /// Parses `input` starting from `rule` and returns a stream of `Token`s.
    fn parse(rule: Rule, input: &Input) -> TokenStream<Rule>;
}
