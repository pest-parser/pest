// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::rc::Rc;
use std::sync::Arc;

use futures::{Async, Poll};
use futures::stream::Stream;

use super::buffered::{BufferedStream, SendableError, SendableToken};
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::inputs_private::{position, span};
use super::super::tokens::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is created by the
/// [`state`](../fn.state) function.
pub struct ParserStream<Rule, I: Input> {
    stream: BufferedStream<SendableToken<Rule>, SendableError<Rule>>,
    input:  Rc<Arc<I>>
}

pub fn new<Rule, I: Input>(
    stream: BufferedStream<SendableToken<Rule>, SendableError<Rule>>,
    input:  Arc<I>
) -> ParserStream<Rule, I> {

    ParserStream {
        stream: stream,
        input:  Rc::new(input)
    }
}

impl<Rule, I: Input> Stream for ParserStream<Rule, I> {
    type Item  = Token<Rule, I>;
    type Error = Error<Rule, I>;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        match self.stream.poll() {
            Ok(async) => {
                Ok(
                    match async {
                        Async::Ready(option) => {
                            Async::Ready(
                                option.map(|sendable| {
                                    match sendable {
                                        SendableToken::Start { rule, pos } => {
                                            Token::Start {
                                                rule: rule,
                                                pos:  position::new(self.input.clone(), pos)
                                            }
                                        },
                                        SendableToken::End { rule, pos } => {
                                            Token::End {
                                                rule: rule,
                                                pos:  position::new(self.input.clone(), pos)
                                            }
                                        }
                                    }
                                })
                            )
                        },
                        Async::NotReady => Async::NotReady
                    }
                )
            },
            Err(error) => {
                Err(
                    match error {
                        SendableError::ParsingError { negatives, positives, pos } => {
                            Error::ParsingError {
                                positives: positives,
                                negatives: negatives,
                                pos:       position::new(self.input.clone(), pos)
                            }
                        },
                        SendableError::CustomErrorPos { message, pos } => {
                            Error::CustomErrorPos {
                                message: message,
                                pos:     position::new(self.input.clone(), pos)
                            }
                        },
                        SendableError::CustomErrorSpan { message, start, end } => {
                            Error::CustomErrorSpan {
                                message: message,
                                span:    span::new(self.input.clone(), start, end)
                            }
                        }
                    }
                )
            }
        }
    }
}
