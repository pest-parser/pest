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
use super::token_stream::TokenStream;
use super::super::error::Error;
use super::super::inputs::Input;
use super::super::inputs_private::{position, span};
use super::super::RuleType;
use super::super::Token;

/// A `struct` which implements `Stream` and `TokenStream`, and is created by the
/// [`state`](../fn.state) function.
pub struct ParserStream<R: RuleType, I: Input> {
    stream: BufferedStream<SendableToken<R>, SendableError<R>>,
    input:  Rc<Arc<I>>
}

pub fn new<R: RuleType, I: Input>(
    stream: BufferedStream<SendableToken<R>, SendableError<R>>,
    input:  Arc<I>
) -> ParserStream<R, I> {
    ParserStream {
        stream: stream,
        input:  Rc::new(input)
    }
}

impl<R: RuleType, I: Input> Stream for ParserStream<R, I> {
    type Item  = Token<R, I>;
    type Error = Error<R, I>;

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
