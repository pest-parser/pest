// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Debug;
use std::rc::Rc;
use std::sync::Arc;

use super::inputs::{Input, Position};
use super::inputs_private::position;
use super::error::Error;
use super::streams_private::buffered::{buffered, BufferedSender, SendableError, SendableToken};
use super::streams_private::parser_stream;
use super::tokens::Token;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TokenDestination {
    Stream,
    Queue,
    Lookahead
}

/// A `struct` which contains the complete state of a `Parser`.
pub struct ParserState<'a, Rule, I: Input> {
    input:           Rc<Arc<I>>,
    sender:          BufferedSender<SendableToken<Rule>, SendableError<Rule>>,
    queue:           Vec<SendableToken<Rule>>,
    dest:            TokenDestination,
    pos_lookahead:   bool,
    is_atomic:       bool,
    pos_attempts:    Vec<Rule>,
    neg_attempts:    Vec<Rule>,
    attempt_pos:     usize,
    /// Stack of captured strings
    pub stack:       Vec<&'a str>,
    /// End-of-input matched flag
    pub eoi_matched: bool
}

/// Creates a new `(ParserStream, ParserState)` tuple from an `Input`.
///
/// # Examples
///
/// ```
/// # extern crate futures;
/// # extern crate pest;
/// # use pest::inputs::StringInput;
/// # use pest::state;
/// # fn main() {
/// let input = StringInput::new("a");
///
/// let (_, _) = state::<(), _>(input);
/// # }
/// ```
pub fn state<'a, Rule: Copy + Debug + Eq + 'static, I: Input>(input: I)
    -> (ParserState<'a, Rule, I>, parser_stream::ParserStream<Rule, I>) {

    let (sender, stream) = buffered(1024);
    let input = Arc::new(input);

    let state = ParserState {
        input:         Rc::new(input.clone()),
        sender:        sender,
        queue:         vec![],
        dest:          TokenDestination::Stream,
        pos_lookahead: false,
        is_atomic:     false,
        pos_attempts:  vec![],
        neg_attempts:  vec![],
        attempt_pos:   0,
        stack:         vec![],
        eoi_matched:   false
    };

    let stream = parser_stream::new(stream, input);

    (state, stream)
}

impl<'a, Rule: Copy, I: Input> ParserState<'a, Rule, I> {
    /// Creates the initial `Position` of the state's `Input` with the value `0`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # fn main() {
    /// let input = StringInput::new("a");
    ///
    /// let (state, _) = state::<(), _>(input);
    ///
    /// assert_eq!(state.start().pos(), 0);
    /// # }
    /// ```
    #[inline]
    pub fn start(&self) -> Position<I> {
        position::new(self.input.clone(), 0)
    }

    #[inline]
    pub fn rule<F>(&mut self, rule: Rule, pos: Position<I>, must_match: bool, f: F)
        -> Result<Position<I>, Position<I>>
        where F: FnOnce(Position<I>,
                        &mut ParserState<'a, Rule, I>) -> Result<Position<I>, Position<I>> {

        let should_toggle = !must_match && self.dest == TokenDestination::Stream;
        let actual_pos = pos.pos();
        let index = self.queue.len();

        if should_toggle {
            self.dest = TokenDestination::Queue;
        }

        self.send(SendableToken::Start { rule: rule, pos: actual_pos });

        let result = f(pos, self);

        if let Ok(ref pos) = result {
            self.send(SendableToken::End { rule: rule, pos: pos.pos() });
        }

        if self.dest == TokenDestination::Queue && result.is_err() {
            self.queue.truncate(index);
        }

        if should_toggle {
            self.dest = TokenDestination::Stream;

            if result.is_ok() {
                for token in self.queue.drain(..) {
                    self.sender.send(token);
                }
            }
        }

        result
    }

    #[inline]
    fn send(&mut self, token: SendableToken<Rule>) {
        match token {
            SendableToken::Start { rule, pos } => {
                let is_positive = !(self.dest == TokenDestination::Lookahead) || self.pos_lookahead;
                self.track(is_positive, rule, pos);
            },
            _ => ()
        };

        match self.dest {
            TokenDestination::Stream    => self.sender.send(token),
            TokenDestination::Queue     => self.queue.push(token),
            TokenDestination::Lookahead => ()
        };
    }

    /// Consumes the `ParserState` and causes the matching `ParserStream` to fail.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # use pest::Error;
    /// # fn main() {
    /// let input = StringInput::new("");
    /// let (state, _) = state::<(), _>(input);
    /// let pos = state.start();
    ///
    /// state.fail(Error::CustomErrorPos { message: "error".to_owned(), pos: pos });
    /// # }
    /// ```
    #[inline]
    pub fn fail(&self, error: Error<Rule, I>) {
        self.sender.fail(match error {
            Error::ParsingError { positives, negatives, pos } => {
                SendableError::ParsingError {
                    positives: positives,
                    negatives: negatives,
                    pos:       pos.pos()
                }
            },
            Error::CustomErrorPos { message, pos } => {
                SendableError::CustomErrorPos {
                    message: message,
                    pos:     pos.pos()
                }
            },
            Error::CustomErrorSpan { message, span } => {
                SendableError::CustomErrorSpan {
                    message: message,
                    start:   span.start(),
                    end:     span.end()
                }
            }
        });
    }

    #[inline]
    pub fn fail_with_attempts(&mut self) {
        let positives = self.pos_attempts.drain(..).collect();
        let negatives = self.neg_attempts.drain(..).collect();

        self.fail(Error::ParsingError {
            positives: positives,
            negatives: negatives,
            pos:       position::new(self.input.clone(), self.attempt_pos)
        });
    }


    #[inline]
    pub fn lookahead<F>(&mut self, is_positive: bool, f: F) -> Result<Position<I>, Position<I>>
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> Result<Position<I>, Position<I>> {

        let should_toggle = self.dest != TokenDestination::Lookahead;
        let initial_dest = self.dest;

        if should_toggle {
            self.dest = TokenDestination::Lookahead;
            self.pos_lookahead = is_positive;
        }

        let result = f(self);

        if should_toggle {
            self.dest = initial_dest;
        }

        result
    }


    #[inline]
    pub fn atomic<F>(&mut self, is_atomic: bool, f: F) -> Result<Position<I>, Position<I>>
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> Result<Position<I>, Position<I>> {

        let should_toggle = self.is_atomic != is_atomic;

        if should_toggle {
            self.is_atomic = is_atomic;
        }

        let result = f(self);

        if should_toggle {
            self.is_atomic = !is_atomic;
        }

        result
    }

    /// Returns the state's current atomicity.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # fn main() {
    /// let input = StringInput::new("");
    /// let (state, _) = state::<(), _>(input);
    ///
    /// assert!(!state.is_atomic());
    /// # }
    /// ```
    #[inline]
    pub fn is_atomic(&self) -> bool {
        self.is_atomic
    }

    #[inline]
    fn track(&mut self, is_positive: bool, rule: Rule, pos: usize) {
        if self.is_atomic || self.dest == TokenDestination::Lookahead {
            return;
        }

        let mut attempts = if is_positive {
            &mut self.pos_attempts
        } else {
            &mut self.neg_attempts
        };

        if attempts.is_empty() {
            attempts.push(rule);

            self.attempt_pos = pos;
        } else if pos == self.attempt_pos {
            attempts.push(rule);
        } else if pos > self.attempt_pos {
            attempts.clear();
            attempts.clear();
            attempts.push(rule);

            self.attempt_pos = pos;
        }
    }
}
