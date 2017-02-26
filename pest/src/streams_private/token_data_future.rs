use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::Poll;
use futures::future::Future;
use futures::stream::Stream;

use super::expandable_stream::ExpandableStream;
use super::super::error::Error;
use super::super::tokens::{Token, TokenData};

/// A `struct` which implements `Future` and is the `TokenData`-returning future which is fed to the
/// closure in `TokenStream::expanded`.
pub struct TokenDataFuture<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    stream: Rc<RefCell<ExpandableStream<Rule, S>>>
}

pub fn new<Rule, S>(stream: Rc<RefCell<ExpandableStream<Rule, S>>>) -> TokenDataFuture<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    TokenDataFuture {
        stream: stream
    }
}

impl<Rule: Copy + Debug + Eq, S> Future for TokenDataFuture<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    type Item  = TokenData<Rule>;
    type Error = Error<Rule>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        self.stream.borrow_mut().poll_token_data()
    }
}
