// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::VecDeque;

use futures::{Async, Poll};
use futures::stream::Stream;

use super::super::error::Error;
use super::super::inputs::{Input, Position};
use super::super::RuleType;
use super::super::tokens::{Token, TokenData};

pub struct ConsumableStream<R, I: Input, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    stream: S,
    rule:   R,
    depth:  u32,
    queue:  VecDeque<Token<R, I>>,
    start:  Option<Position<I>>,
    end:    Option<Position<I>>,
    error:  Option<Error<R, I>>
}

impl<R: RuleType, I: Input, S> ConsumableStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    #[inline]
    pub fn new(stream: S, rule: R) -> ConsumableStream<R, I, S> {
        ConsumableStream {
            stream: stream,
            rule:   rule,
            depth:  0,
            queue:  VecDeque::new(),
            start:  None,
            end:    None,
            error:  None
        }
    }

    #[inline]
    pub fn poll_token_data(&mut self) -> Poll<TokenData<R, I>, Error<R, I>> {
        if let Some(ref error) = self.error {
            return Err(error.clone());
        }

        if self.start.is_some() && self.end.is_some() {
            let start = self.start.as_ref().unwrap();
            let end = self.end.as_ref().unwrap();

            Ok(Async::Ready(
                TokenData {
                    rule: self.rule,
                    span: start.clone().span(end.clone())
                }
            ))
        } else {
            loop {
                match self.stream.poll() {
                    Ok(Async::Ready(Some(token))) => {
                        if self.start.is_none() {
                            match token {
                                Token::Start { ref rule, ref pos } if *rule == self.rule => {
                                    self.start = Some(pos.clone());
                                },
                                token => panic!("expected Start {{ rule: {:?}, .. }}, \
                                                 but found {:?} instead", self.rule, token)
                            };
                        } else {
                            match token {
                                Token::Start { ref rule, ref pos } if *rule == self.rule => {
                                    self.queue.push_back(
                                        Token::Start { rule: *rule, pos: pos.clone() }
                                    );
                                    self.depth += 1;
                                },
                                Token::End { ref rule, ref pos } if *rule == self.rule => {
                                    if self.depth == 0 {
                                        self.end = Some(pos.clone());

                                        if let Some(ref start) = self.start {
                                            return Ok(Async::Ready(
                                                TokenData {
                                                    rule: self.rule,
                                                    span: start.clone().span(pos.clone())
                                                }
                                            ));
                                        } else {
                                            unreachable!();
                                        }
                                    } else {
                                        self.queue.push_back(
                                            Token::End { rule: *rule, pos: pos.clone() }
                                        );
                                        self.depth -= 1;
                                    }
                                },
                                token => self.queue.push_back(token)
                            };
                        }
                    },
                    Ok(Async::Ready(None)) => {
                        if self.start.is_none() {
                            panic!("expected Start {{ rule: {:?}, .. }}, \
                                    but found nothing", self.rule);
                        } else {
                            panic!("expected End {{ rule: {:?}, .. }}, \
                                    but found nothing", self.rule);
                        }
                    },
                    Ok(Async::NotReady) => return Ok(Async::NotReady),
                    Err(e) => {
                        self.error = Some(e.clone());

                        return Err(e);
                    }
                };
            }
        }
    }

    #[inline]
    pub fn poll_consumed(&mut self) -> Poll<Option<Token<R, I>>, Error<R, I>> {
        if let Some(ref error) = self.error {
            return Err(error.clone());
        }

        if !self.queue.is_empty() {
            Ok(Async::Ready(self.queue.pop_front()))
        } else if self.end.is_some() {
            Ok(Async::Ready(None))
        } else {
            match self.stream.poll() {
                Ok(Async::Ready(Some(token))) => {
                    if self.start.is_none() {
                        match token {
                            Token::Start { ref rule, ref pos } if *rule == self.rule => {
                                self.start = Some(pos.clone());

                                self.poll_consumed()
                            },
                            token => panic!("expected Start {{ rule: {:?}, .. }}, \
                                             but found {:?} instead", self.rule, token)
                        }
                    } else {
                        match token {
                            Token::Start { ref rule, ref pos } if *rule == self.rule => {
                                self.depth += 1;

                                Ok(Async::Ready(Some(
                                    Token::Start { rule: *rule, pos: pos.clone() }
                                )))
                            },
                            Token::End { ref rule, ref pos } if *rule == self.rule => {
                                if self.depth == 0 {
                                    self.end = Some(pos.clone());

                                    Ok(Async::Ready(None))
                                } else {
                                    self.depth -= 1;

                                    Ok(Async::Ready(Some(
                                        Token::End { rule: *rule, pos: pos.clone() }
                                    )))
                                }
                            },
                            token => {
                                Ok(Async::Ready(Some(token)))
                            }
                        }
                    }
                },
                Ok(Async::Ready(None)) => {
                    if self.start.is_none() {
                        panic!("expected Start {{ rule: {:?}, .. }}, \
                                but found nothing", self.rule);
                    } else {
                        panic!("expected End {{ rule: {:?}, .. }}, \
                                but found nothing", self.rule);
                    }
                }
                Ok(Async::NotReady) => Ok(Async::NotReady),
                Err(e) => {
                    self.error = Some(e.clone());

                    Err(e)
                }
            }
        }
    }

    #[inline]
    pub fn poll_tail(&mut self) -> Poll<Option<Token<R, I>>, Error<R, I>> {
        if let Some(ref error) = self.error {
            return Err(error.clone());
        }

        if self.end.is_none() {
            match self.stream.poll() {
                Ok(Async::Ready(Some(token))) => {
                    if self.start.is_none() {
                        match token {
                            Token::Start { ref rule, ref pos } if *rule == self.rule => {
                                self.start = Some(pos.clone());
                            },
                            token => panic!("expected Start {{ rule: {:?}, .. }}, \
                                             but found {:?} instead", self.rule, token)
                        };
                    } else {
                        match token {
                            Token::Start { ref rule, ref pos } if *rule == self.rule => {
                                self.queue.push_back(
                                    Token::Start { rule: *rule, pos: pos.clone() }
                                );
                                self.depth += 1;
                            },
                            Token::End { ref rule, ref pos } if *rule == self.rule => {
                                if self.depth == 0 {
                                    self.end = Some(pos.clone());
                                } else {
                                    self.queue.push_back(
                                        Token::End { rule: *rule, pos: pos.clone() }
                                    );
                                    self.depth -= 1;
                                }
                            },
                            token => self.queue.push_back(token)
                        };
                    }
                },
                Ok(Async::Ready(None)) => {
                    if self.start.is_none() {
                        panic!("expected Start {{ rule: {:?}, .. }}, \
                                but found nothing", self.rule);
                    } else {
                        panic!("expected End {{ rule: {:?}, .. }}, \
                                but found nothing", self.rule);
                    }
                },
                Ok(Async::NotReady) => return Ok(Async::NotReady),
                Err(e) => {
                    self.error = Some(e.clone());

                    return Err(e);
                }
            };

            self.poll_tail()
        } else {
            self.stream.poll()
        }
    }
}
