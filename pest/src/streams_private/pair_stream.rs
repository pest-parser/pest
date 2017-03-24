use std::cell::RefCell;
// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Debug;
use std::rc::Rc;

use futures::Poll;
use futures::stream::Stream;

use super::sliceable_stream::SliceableStream;
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::RuleType;
use super::super::tokens::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is contained withing the
/// `SlicedStream` which is returned by [`TokenStream::sliced`](trait.TokenStream#method.sliced).
pub struct PairStream<R, I: Input, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    stream: Rc<RefCell<SliceableStream<R, I, S>>>,
    index:  usize
}

pub fn new<R, I: Input, S>(stream: Rc<RefCell<SliceableStream<R, I, S>>>, index: usize)
                           -> PairStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    PairStream {
        stream: stream,
        index:  index
    }
}

impl<R: RuleType, I: Input, S> Stream for PairStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    type Item  = Token<R, I>;
    type Error = Error<R, I>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        self.stream.borrow_mut().poll_pair(self.index)
    }
}
