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
use super::super::inputs::Input;
use super::super::RuleType;
use super::super::tokens::Token;

pub struct SliceableStream<R, I: Input, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    stream: S,
    depth:  usize,
    queues: Vec<VecDeque<Token<R, I>>>,
    rule:   Option<R>,
    error:  Option<Error<R, I>>
}

impl<R: RuleType, I: Input, S> SliceableStream<R, I, S>
    where S: Stream<Item=Token<R, I>, Error=Error<R, I>> {

    pub fn new(stream: S) -> SliceableStream<R, I, S> {
        SliceableStream {
            stream: stream,
            depth:  0,
            queues: vec![],
            rule:   None,
            error:  None
        }
    }

    pub fn poll_split(&mut self) -> Poll<Option<usize>, Error<R, I>> {
        if let Some(ref error) = self.error {
            return Err(error.clone());
        }

        match self.stream.poll() {
            Ok(Async::Ready(Some(token))) => {
                if self.depth == 0 {

                    match token {
                        Token::Start { rule, pos } => {
                            self.depth += 1;
                            self.queues.push(VecDeque::new());
                            self.rule = Some(rule);

                            self.queues.last_mut().unwrap().push_back(
                                Token::Start { rule: rule, pos: pos }
                            );
                        },
                        token => panic!("expected Start {{ .. }}, \
                                         but found {:?} instead", token)
                    };

                    Ok(Async::Ready(Some(self.queues.len() - 1)))
                } else {
                    let current_rule = *self.rule.as_ref().unwrap();

                    match token {
                        Token::Start { ref rule, ref pos } if *rule == current_rule => {
                            self.queues.last_mut().unwrap().push_back(
                                Token::Start { rule: *rule, pos: pos.clone() }
                            );
                            self.depth += 1;
                        },
                        Token::End { ref rule, ref pos } if *rule == current_rule => {
                            self.queues.last_mut().unwrap().push_back(
                                Token::End { rule: *rule, pos: pos.clone() }
                            );
                            self.depth -= 1;
                        },
                        token => self.queues.last_mut().unwrap().push_back(token)
                    };

                    self.poll_split()
                }
            },
            Ok(Async::Ready(None)) => {
                if self.depth == 0 {
                    Ok(Async::Ready(None))
                } else {
                    panic!("expected End {{ rule: {:?}, .. }}, \
                            but found nothing", self.rule.as_ref().unwrap())
                }
            },
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => {
                self.error = Some(e.clone());

                Err(e)
            }
        }
    }

    pub fn poll_pair(&mut self, index: usize) -> Poll<Option<Token<R, I>>, Error<R, I>> {
        if !self.queues[index].is_empty() {
            Ok(Async::Ready(self.queues[index].pop_front()))
        } else if index == self.queues.len() - 1 {
            if let Some(ref error) = self.error {
                return Err(error.clone());
            }

            if self.depth == 0 {
                Ok(Async::Ready(None))
            } else {
                match self.stream.poll() {
                    Ok(Async::Ready(Some(token))) => {
                        let current_rule = *self.rule.as_ref().unwrap();

                        match token {
                            Token::Start { ref rule, ref pos } if *rule == current_rule => {
                                self.queues.last_mut().unwrap().push_back(
                                    Token::Start { rule: *rule, pos: pos.clone() }
                                );
                                self.depth += 1;
                            },
                            Token::End { ref rule, ref pos } if *rule == current_rule => {
                                self.queues.last_mut().unwrap().push_back(
                                    Token::End { rule: *rule, pos: pos.clone() }
                                );
                                self.depth -= 1;
                            },
                            token => self.queues.last_mut().unwrap().push_back(token)
                        };

                        self.poll_pair(index)
                    },
                    Ok(Async::Ready(None)) => {
                        panic!("expected End {{ rule: {:?}, .. }}, \
                                but found nothing", self.rule.as_ref().unwrap())
                    },
                    Ok(Async::NotReady) => Ok(Async::NotReady),
                    Err(e) => {
                        self.error = Some(e.clone());

                        Err(e)
                    }
                }
            }
        } else {
            Ok(Async::Ready(None))
        }
    }
}
