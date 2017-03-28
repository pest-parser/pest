// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::rc::Rc;

use futures::Poll;
use futures::future::Future;
use futures::stream::Stream;

use super::consumable_stream::ConsumableStream;
use super::super::error::Error;
use super::super::inputs::{Input, Span};
use super::super::RuleType;
use super::super::Token;

pub struct SpanFuture<R, I: Input, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    stream: Rc<RefCell<ConsumableStream<R, I, S>>>
}

pub fn new<R, I: Input, S>(stream: Rc<RefCell<ConsumableStream<R, I, S>>>)
    -> SpanFuture<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    SpanFuture {
        stream: stream
    }
}

impl<R: RuleType, I: Input, S> Future for SpanFuture<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    type Item  = Span<I>;
    type Error = Error<R, I>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        self.stream.borrow_mut().poll_span()
    }
}
