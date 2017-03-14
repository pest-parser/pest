// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::{Async, Poll};
use futures::stream::Stream;

use super::pair_stream as ps;
use super::sliceable_stream::SliceableStream;
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::tokens::Token;

/// A `struct` which implements `Stream`, which contains `PairStream`s, and which is returned by
/// [`TokenStream::sliced`](trait.TokenStream#method.sliced).
pub struct SlicedStream<Rule, I: Input, S>
    where S: Stream<Item=Token<Rule, I>, Error=Error<Rule, I>> {

    stream: Rc<RefCell<SliceableStream<Rule, I, S>>>
}

pub fn new<Rule, I: Input, S>(stream: Rc<RefCell<SliceableStream<Rule, I, S>>>)
    -> SlicedStream<Rule, I, S>
    where S: Stream<Item=Token<Rule, I>, Error=Error<Rule, I>> {

    SlicedStream {
        stream: stream
    }
}

impl<Rule: Copy + Debug + Eq, I: Input + Debug, S> Stream for SlicedStream<Rule, I, S>
    where S: Stream<Item=Token<Rule, I>, Error=Error<Rule, I>> {

    type Item  = ps::PairStream<Rule, I, S>;
    type Error = Error<Rule, I>;

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
