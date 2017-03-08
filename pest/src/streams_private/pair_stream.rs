use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::Poll;
use futures::stream::Stream;

use super::sliceable_stream::SliceableStream;
use super::super::error::Error;
use super::super::tokens::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is contained withing the
/// `SlicedStream` which is returned by [`TokenStream::sliced`](trait.TokenStream#method.sliced).
pub struct PairStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    stream: Rc<RefCell<SliceableStream<Rule, S>>>,
    index:  usize
}

pub fn new<Rule, S>(stream: Rc<RefCell<SliceableStream<Rule, S>>>, index: usize)
                    -> PairStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    PairStream {
        stream: stream,
        index:  index
    }
}

impl<Rule: Copy + Debug + Eq, S> Stream for PairStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    type Item  = Token<Rule>;
    type Error = Error<Rule>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        self.stream.borrow_mut().poll_pair(self.index)
    }
}
