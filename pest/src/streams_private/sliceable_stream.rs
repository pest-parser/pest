use std::collections::VecDeque;
use std::fmt::Debug;

use futures::{Async, Poll};
use futures::stream::Stream;

use super::super::error::Error;
use super::super::tokens::Token;

pub struct SliceableStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    stream: S,
    depth:  usize,
    queues: Vec<VecDeque<Token<Rule>>>,
    rule:   Option<Rule>,
    error:  Option<Error<Rule>>
}

impl<Rule: Copy + Debug + Eq, S> SliceableStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    pub fn new(stream: S) -> SliceableStream<Rule, S> {
        SliceableStream {
            stream: stream,
            depth:  0,
            queues: vec![],
            rule:   None,
            error:  None
        }
    }

    pub fn poll_split(&mut self) -> Poll<Option<usize>, Error<Rule>> {
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
                        Token::Start { rule, pos } if rule == current_rule => {
                            self.queues.last_mut().unwrap().push_back(
                                Token::Start { rule: rule, pos: pos }
                            );
                            self.depth += 1;
                        },
                        Token::End { rule, pos } if rule == current_rule => {
                            self.queues.last_mut().unwrap().push_back(
                                Token::End { rule: rule, pos: pos }
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

    pub fn poll_pair(&mut self, index: usize) -> Poll<Option<Token<Rule>>, Error<Rule>> {
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
                            Token::Start { rule, pos } if rule == current_rule => {
                                self.queues.last_mut().unwrap().push_back(
                                    Token::Start { rule: rule, pos: pos }
                                );
                                self.depth += 1;
                            },
                            Token::End { rule, pos } if rule == current_rule => {
                                self.queues.last_mut().unwrap().push_back(
                                    Token::End { rule: rule, pos: pos }
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
