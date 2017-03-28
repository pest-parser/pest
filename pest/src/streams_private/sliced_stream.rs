// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::rc::Rc;

use futures::{Async, Poll};
use futures::stream::Stream;

use super::pair_stream as ps;
use super::sliceable_stream::SliceableStream;
use super::token_stream::TokenStream;
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::RuleType;
use super::super::Token;

/// A `struct` which implements `Stream`, which contains `PairStream`s, and which is returned by
/// [`TokenStream::sliced`](trait.TokenStream#method.sliced).
pub struct SlicedStream<R: RuleType, I: Input, S: TokenStream<R, I>> {
    stream: Rc<RefCell<SliceableStream<R, I, S>>>
}

pub fn new<R: RuleType, I: Input, S: TokenStream<R, I>>(
    stream: Rc<RefCell<SliceableStream<R, I, S>>>
) -> SlicedStream<R, I, S> {
    SlicedStream {
        stream: stream
    }
}

impl<R: RuleType, I: Input, S: TokenStream<R, I>> Stream for SlicedStream<R, I, S> {
    type Item  = ps::PairStream<R, I, S>;
    type Error = Error<R, I>;

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
