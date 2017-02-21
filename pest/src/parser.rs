use super::inputs::Input;
use super::streams::parser_stream::ParserStream;

/// A `trait` that defines a `Parser`.
pub trait Parser<Rule> {
    /// Parses `input` starting from `rule` and returns a stream of `Token`s.
    fn parse(rule: Rule, input: &Input) -> ParserStream<Rule>;
}
