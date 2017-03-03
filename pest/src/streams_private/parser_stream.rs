use std::fmt::Debug;

use futures::{Async, Poll};
use futures::stream::Stream;
use futures::sync::mpsc::UnboundedReceiver;

use super::token_stream::TokenStream;
use super::super::error::Error;
use super::super::tokens::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is created by the
/// [`state`](../fn.state) function.
pub struct ParserStream<Rule> {
    stream: UnboundedReceiver<Result<Token<Rule>, Error<Rule>>>
}

pub fn new<Rule>(stream: UnboundedReceiver<Result<Token<Rule>, Error<Rule>>>)
    -> ParserStream<Rule> {

    ParserStream {
        stream: stream
    }
}

impl<Rule> Stream for ParserStream<Rule> {
    type Item  = Token<Rule>;
    type Error = Error<Rule>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        match self.stream.poll() {
            Ok(Async::Ready(Some(result))) => {
                match result {
                    Ok(token)  => Ok(Async::Ready(Some(token))),
                    Err(error) => Err(error)
                }
            },
            Ok(Async::Ready(None)) => Ok(Async::Ready(None)),
            Ok(Async::NotReady)    => Ok(Async::NotReady),
            Err(_) => unreachable!()
        }
    }
}
