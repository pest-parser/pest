use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::{Async, Poll};
use futures::stream::Stream;

use super::pair_stream as ps;
use super::sliceable_stream::SliceableStream;
use super::super::error::Error;
use super::super::tokens::Token;

/// A `struct` which implements `Stream`, which contains `PairStream`s, and which is returned by
/// [`TokenStream::expanded`](trait.TokenStream#method.sliced).
pub struct SlicedStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    stream: Rc<RefCell<SliceableStream<Rule, S>>>
}

pub fn new<Rule, S>(stream: Rc<RefCell<SliceableStream<Rule, S>>>) -> SlicedStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    SlicedStream {
        stream: stream
    }
}

impl<Rule: Copy + Debug + Eq, S> Stream for SlicedStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    type Item  = ps::PairStream<Rule, S>;
    type Error = Error<Rule>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        match self.stream.borrow_mut().poll_split() {
            Ok(Async::Ready(Some(index))) => {
                Ok(Async::Ready(Some(
                    ps::new(self.stream.clone(), index)
                )))
            },
            Ok(Async::Ready(None)) => Ok(Async::Ready(None)),
            Ok(Async::NotReady)    => Ok(Async::NotReady),
            Err(e)                 => Err(e)
        }
    }
}
