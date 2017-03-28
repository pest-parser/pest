// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::rc::Rc;

use futures::Poll;
use futures::stream::Stream;

use super::consumable_stream::ConsumableStream;
use super::rule_future::{self, RuleFuture};
use super::span_future::{self, SpanFuture};
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::RuleType;
use super::super::tokens::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is the consumed stream which is fed
/// to the closure in [`TokenStream::consume`](trait.TokenStream#method.consume).
pub struct ConsumedStream<R, I: Input, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    stream: Rc<RefCell<ConsumableStream<R, I, S>>>
}

pub fn new<R, I: Input, S>(stream: Rc<RefCell<ConsumableStream<R, I, S>>>)
    -> ConsumedStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    ConsumedStream {
        stream: stream
    }
}

impl<R, I: Input, S> ConsumedStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    #[inline]
    pub fn rule(&self) -> RuleFuture<R, I, S> {
        rule_future::new(self.stream.clone())
    }

    #[inline]
    pub fn span(&self) -> SpanFuture<R, I, S> {
        span_future::new(self.stream.clone())
    }
}


impl<R: RuleType, I: Input, S> Stream for ConsumedStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    type Item  = Token<R, I>;
    type Error = Error<R, I>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        self.stream.borrow_mut().poll_consumed()
    }
}
