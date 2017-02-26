use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::Poll;
use futures::stream::Stream;

use super::expandable_stream::ExpandableStream;
use super::token_stream::TokenStream;
use super::super::error::Error;
use super::super::tokens::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is the returned tail stream which is
/// returned in [`TokenStream::expanded`](trait.TokenStream#method.expand).
pub struct TailStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    stream: Rc<RefCell<ExpandableStream<Rule, S>>>
}

pub fn new<Rule, S>(stream: Rc<RefCell<ExpandableStream<Rule, S>>>) -> TailStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    TailStream {
        stream: stream
    }
}

impl<Rule: Copy + Debug + Eq, S> Stream for TailStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    type Item  = Token<Rule>;
    type Error = Error<Rule>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        self.stream.borrow_mut().poll_tail()
    }
}

impl<Rule: Copy + Debug + Eq, S> TokenStream<Rule> for TailStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {}
