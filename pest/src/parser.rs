use std::fmt::Debug;
use std::sync::Arc;

use super::inputs::Input;
use super::streams_private::parser_stream::ParserStream;

/// A `trait` that defines a `Parser`.
pub trait Parser<Rule: Debug + Eq + 'static> {
    /// Parses `input` starting from `rule` and returns a `ParserStream` of `Token`s.
    fn parse<I: Input>(rule: Rule, input: Arc<I>) -> ParserStream<Rule>;
}
