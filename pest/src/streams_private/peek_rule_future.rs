// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Debug;

use futures::{Async, Poll};
use futures::future::Future;
use futures::stream::{Peekable, Stream};

use super::super::error::Error;
use super::super::inputs::Input;
use super::super::RuleType;
use super::super::tokens::Token;

/// A `struct` which implements `Future`. It contains a peeked `Rule` and stream, and is returned by
/// [`TokenStream::peek_rule`](trait.TokenStream#method.peek_rule).
pub struct PeekRuleFuture<R, I: Input, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    peekable: Option<Peekable<S>>
}

pub fn new<R, I: Input, S>(stream: S) -> PeekRuleFuture<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    PeekRuleFuture {
        peekable: Some(stream.peekable())
    }
}

impl <R: RuleType, I: Input, S> Future for PeekRuleFuture<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    type Item  = (Option<R>, Peekable<S>);
    type Error = Error<R, I>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        let rule = match self.peekable.as_mut().expect("called poll twice").peek() {
            Ok(Async::Ready(Some(token))) => {
                let rule = match *token {
                    Token::Start { rule, .. } => rule,
                    Token::End   { .. } => {
                        panic!("expected Start {{ .. }}, but found {:?} instead", token)
                    }
                };

                Some(rule)
            },
            Ok(Async::Ready(None)) => None,
            Ok(Async::NotReady)    => return Ok(Async::NotReady),
            Err(e)                 => return Err(e)
        };

        Ok(Async::Ready(
            (rule, self.peekable.take().unwrap())
        ))
    }
}
