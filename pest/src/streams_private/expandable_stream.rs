use std::collections::VecDeque;
use std::fmt::Debug;

use futures::{Async, Poll};
use futures::stream::Stream;

use super::super::error::Error;
use super::super::tokens::{Token, TokenData};

pub struct ExpandableStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    stream: S,
    rule:   Rule,
    depth:  u32,
    queue:  VecDeque<Token<Rule>>,
    start:  Option<usize>,
    end:    Option<usize>,
    error:  Option<Error<Rule>>
}

impl<Rule: Copy + Debug + Eq, S> ExpandableStream<Rule, S>
    where S: Stream<Item=Token<Rule>, Error=Error<Rule>> {

    pub fn new(stream: S, rule: Rule) -> ExpandableStream<Rule, S> {
        ExpandableStream {
            stream: stream,
            rule:   rule,
            depth:  0,
            queue:  VecDeque::new(),
            start:  None,
            end:    None,
            error:  None
        }
    }

    pub fn poll_token_data(&mut self) -> Poll<TokenData<Rule>, Error<Rule>> {
        if let Some(ref error) = self.error {
            return Err(error.clone());
        }

        if let (Some(start), Some(end)) = (self.start, self.end) {
            Ok(Async::Ready(
                TokenData {
                    rule:  self.rule,
                    start: start,
                    end:   end
                }
            ))
        } else {
            loop {
                match self.stream.poll() {
                    Ok(Async::Ready(Some(token))) => {
                        if self.start.is_none() {
                            match token {
                                Token::Start { rule, pos } if rule == self.rule => {
                                    self.start = Some(pos);
                                },
                                token => panic!("expected Start {{ rule: {:?}, .. }}, \
                                                 but found {:?} instead", self.rule, token)
                            };
                        } else {
                            match token {
                                Token::Start { rule, pos } if rule == self.rule => {
                                    self.queue.push_back(Token::Start { rule: rule, pos: pos });
                                    self.depth += 1;
                                },
                                Token::End { rule, pos } if rule == self.rule => {
                                    if self.depth == 0 {
                                        self.end = Some(pos);

                                        if let Some(start) = self.start {
                                            return Ok(Async::Ready(
                                                TokenData {
                                                    rule:  self.rule,
                                                    start: start,
                                                    end:   pos
                                                }
                                            ));
                                        } else {
                                            unreachable!();
                                        }
                                    } else {
                                        self.queue.push_back(Token::End { rule: rule, pos: pos });
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

    pub fn poll_expanded(&mut self) -> Poll<Option<Token<Rule>>, Error<Rule>> {
        if let Some(ref error) = self.error {
            return Err(error.clone());
        }

        if !self.queue.is_empty() {
            Ok(Async::Ready(self.queue.pop_front()))
        } else {
            if self.end.is_some() {
                Ok(Async::Ready(None))
            } else {
                match self.stream.poll() {
                    Ok(Async::Ready(Some(token))) => {
                        if self.start.is_none() {
                            match token {
                                Token::Start { rule, pos } if rule == self.rule => {
                                    self.start = Some(pos);

                                    self.poll_expanded()
                                },
                                token => panic!("expected Start {{ rule: {:?}, .. }}, \
                                                 but found {:?} instead", self.rule, token)
                            }
                        } else {
                            match token {
                                Token::Start { rule, pos } if rule == self.rule => {
                                    self.depth += 1;

                                    Ok(Async::Ready(Some(
                                        Token::Start { rule: rule, pos: pos }
                                    )))
                                },
                                Token::End { rule, pos } if rule == self.rule => {
                                    if self.depth == 0 {
                                        self.end = Some(pos);

                                        Ok(Async::Ready(None))
                                    } else {
                                        self.depth -= 1;

                                        Ok(Async::Ready(Some(
                                            Token::End { rule: rule, pos: pos }
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
    }

    pub fn poll_tail(&mut self) -> Poll<Option<Token<Rule>>, Error<Rule>> {
        if let Some(ref error) = self.error {
            return Err(error.clone());
        }

        if self.end.is_none() {
            match self.stream.poll() {
                Ok(Async::Ready(Some(token))) => {
                    if self.start.is_none() {
                        match token {
                            Token::Start { rule, pos } if rule == self.rule => {
                                self.start = Some(pos);
                            },
                            token => panic!("expected Start {{ rule: {:?}, .. }}, \
                                             but found {:?} instead", self.rule, token)
                        };
                    } else {
                        match token {
                            Token::Start { rule, pos } if rule == self.rule => {
                                self.queue.push_back(Token::Start { rule: rule, pos: pos });
                                self.depth += 1;
                            },
                            Token::End { rule, pos } if rule == self.rule => {
                                if self.depth == 0 {
                                    self.end = Some(pos);
                                } else {
                                    self.queue.push_back(Token::End { rule: rule, pos: pos });
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
