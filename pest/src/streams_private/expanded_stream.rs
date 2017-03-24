// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::Poll;
use futures::stream::Stream;

use super::expandable_stream::ExpandableStream;
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::RuleType;
use super::super::tokens::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is the expanded stream which is fed
/// to the closure in [`TokenStream::expanded`](trait.TokenStream#method.expand).
pub struct ExpandedStream<R, I: Input, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    stream: Rc<RefCell<ExpandableStream<R, I, S>>>
}

pub fn new<R, I: Input, S>(stream: Rc<RefCell<ExpandableStream<R, I, S>>>)
    -> ExpandedStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    ExpandedStream {
        stream: stream
    }
}

impl<R: RuleType, I: Input, S> Stream for ExpandedStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    type Item  = Token<R, I>;
    type Error = Error<R, I>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        self.stream.borrow_mut().poll_expanded()
    }
}
