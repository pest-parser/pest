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
use futures::future::Future;
use futures::stream::Stream;

use super::expandable_stream::ExpandableStream;
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::tokens::{Token, TokenData};

/// A `struct` which implements `Future` and is the `TokenData`-returning future which is fed to the
/// closure in [`TokenStream::expanded`](trait.TokenStream#method.expand).
pub struct TokenDataFuture<Rule, I: Input, S>
    where S: Stream<Item=Token<Rule, I>, Error=Error<Rule, I>> {

    stream: Rc<RefCell<ExpandableStream<Rule, I, S>>>
}

pub fn new<Rule, I: Input, S>(stream: Rc<RefCell<ExpandableStream<Rule, I, S>>>)
    -> TokenDataFuture<Rule, I, S>
    where S: Stream<Item=Token<Rule, I>, Error=Error<Rule, I>> {

    TokenDataFuture {
        stream: stream
    }
}

impl<Rule: Copy + Debug + Eq, I: Input + Debug, S> Future for TokenDataFuture<Rule, I, S>
    where S: Stream<Item=Token<Rule, I>, Error=Error<Rule, I>> {

    type Item  = TokenData<Rule, I>;
    type Error = Error<Rule, I>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        self.stream.borrow_mut().poll_token_data()
    }
}
