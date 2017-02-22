use futures::stream::Stream;

use super::error::Error;
use super::inputs::Input;
use super::tokens::Token;
use super::tokens::TokenStream;

/// A `trait` that defines a `Parser`.
pub trait Parser<Rule> {
    /// Parses `input` starting from `rule` and returns a stream of `Token`s.
    fn parse<S>(rule: Rule, input: &Input) -> TokenStream<Rule, S>
        where S: Stream<Item=Result<Token<Rule>, Error<Rule>>, Error=()>;
}
