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

use super::consumable_stream::ConsumableStream;
use super::token_stream::TokenStream;
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::RuleType;

pub struct RuleFuture<R: RuleType, I: Input, S: TokenStream<R, I>> {
    stream: Rc<RefCell<ConsumableStream<R, I, S>>>
}

pub fn new<R: RuleType, I: Input, S: TokenStream<R, I>>(
    stream: Rc<RefCell<ConsumableStream<R, I, S>>>
) -> RuleFuture<R, I, S> {
    RuleFuture {
        stream: stream
    }
}

impl<R: RuleType, I: Input, S: TokenStream<R, I>> Future for RuleFuture<R, I, S> {
    type Item  = R;
    type Error = Error<R, I>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        self.stream.borrow_mut().poll_rule()
    }
}
