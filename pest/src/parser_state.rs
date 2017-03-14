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
    Ignore
}

/// A `struct` which contains the complete state of a `Parser`.
pub struct ParserState<'a, Rule, I: Input> {
    input:           Rc<Arc<I>>,
    sender:          BufferedSender<SendableToken<Rule>, SendableError<Rule>>,
    queue:           Vec<SendableToken<Rule>>,
    dest:            TokenDestination,
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
pub fn state<'a, Rule: Copy+ Debug + Eq + 'static, I: Input>(input: I)
    -> (ParserState<'a, Rule, I>, parser_stream::ParserStream<Rule, I>) {

    let (sender, stream) = buffered(1024);
    let input = Arc::new(input);

    let state = ParserState {
        input:        Rc::new(input.clone()),
        sender:       sender,
        queue:        vec![],
        dest:         TokenDestination::Stream,
        is_atomic:    false,
        pos_attempts: vec![],
        neg_attempts: vec![],
        attempt_pos:  0,
        stack:        vec![],
        eoi_matched:  false
    };

    let stream = parser_stream::new(stream, input);

    (state, stream)
}

impl<'a, Rule: Clone + Ord, I: Input> ParserState<'a, Rule, I> {
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

    /// Sends `token` according to the `ParserState`'s destination. The `Token` will get sent to the
    /// `TokenStream`, queued up to be sent later, or ignored.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let input = StringInput::new("");
    /// let (mut state, _) = state::<(), _>(input);
    /// let pos = state.start();
    ///
    /// state.send(Token::Start { rule: (), pos: pos });
    /// # }
    /// ```
    #[inline]
    pub fn send(&mut self, token: Token<Rule, I>) {
        fn to_sendable<Rule, I: Input>(token: Token<Rule, I>) -> SendableToken<Rule> {
            match token {
                Token::Start { rule, pos } => {
                    SendableToken::Start {
                        rule: rule,
                        pos:  pos.pos()
                    }
                },
                Token::End { rule, pos } => {
                    SendableToken::End {
                        rule: rule,
                        pos:  pos.pos()
                    }
                }
            }
        }

        match self.dest {
            TokenDestination::Stream => self.sender.send(to_sendable(token)),
            TokenDestination::Queue  => self.queue.push(to_sendable(token)),
            TokenDestination::Ignore => ()
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
    pub fn fail(self, error: Error<Rule, I>) {
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

    /// Matches the current `rule`, queues up all generated `Token`s, and reverts the state if the
    /// `rule` fails.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # fn main() {
    /// let input = StringInput::new("abacad");
    /// let (mut state, _) = state::<(), _>(input);
    ///
    /// let first = state.queued(|state| {
    ///     state.start().match_string("a").and_then(|p| p.match_string("b"))
    /// });
    /// assert!(first.is_some());
    ///
    /// let second = state.queued(|_| {
    ///     first.and_then(|p| p.match_string("a")).and_then(|p| p.match_string("c"))
    /// });
    /// assert!(second.is_some());
    ///
    /// let third = state.queued(|_| {
    ///     second.and_then(|p| p.match_string("a")).and_then(|p| p.match_string("d"))
    /// });
    /// assert!(third.is_some());
    /// # }
    /// ```
    #[inline]
    pub fn queued<F>(&mut self, rule: F) -> Option<Position<I>>
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> Option<Position<I>> {

        let should_toggle = self.dest == TokenDestination::Stream;

        if should_toggle {
            self.dest = TokenDestination::Queue;
        }

        let result = rule(self);

        if should_toggle {
            self.dest = TokenDestination::Stream;

            if result.is_some() {
                for token in self.queue.drain(..) {
                    self.sender.send(token);
                }
            } else {
                self.queue.clear();
            }
        }

        result
    }

    /// Matches the current `rule`, ignores all `Token`s, and reverts the state.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let input = StringInput::new("a");
    /// let (mut state, _) = state::<(), _>(input);
    ///
    /// assert!(state.ignored(|state| {
    ///     let pos = state.start();
    ///     state.send(Token::Start { rule: (), pos: pos }); // Won't actually be sent.
    ///
    ///     state.start().match_string("b")
    /// }).is_none());
    /// # }
    /// ```
    #[inline]
    pub fn ignored<F>(&mut self, rule: F) -> Option<Position<I>>
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> Option<Position<I>> {

        let should_toggle = self.dest != TokenDestination::Ignore;
        let initial_dest = self.dest;

        if should_toggle {
            self.dest = TokenDestination::Ignore;
        }

        let result = rule(self);

        if should_toggle {
            self.dest = initial_dest;
        }

        result
    }

    /// Matches the current `rule` while toggling atomicity.
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
    /// let (mut state, _) = state::<(), _>(input);
    ///
    /// assert!(!state.is_atomic());
    /// state.atomic(true, |state| {
    ///     assert!(state.is_atomic());
    ///     None
    /// });
    /// assert!(!state.is_atomic());
    /// # }
    /// ```
    #[inline]
    pub fn atomic<F>(&mut self, is_atomic: bool, rule: F) -> Option<Position<I>>
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> Option<Position<I>> {

        let should_toggle = self.is_atomic != is_atomic;

        if should_toggle {
            self.is_atomic = is_atomic;
        }

        let result = rule(self);

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

    /// Keeps track of failed positive rule attempts. It should be called when a `Rule` fails at the
    /// current position.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = StringInput::new("a");
    /// let (mut state, _) = state::<Rule, _>(input);
    /// let pos = state.start();
    ///
    /// state.track_pos(Rule::a, pos);
    /// # }
    /// ```
    #[inline]
    pub fn track_pos(&mut self, rule: Rule, pos: Position<I>) {
        self.track(true, rule, pos)
    }

    /// Keeps track of failed negative rule attempts. It should be called when a `Rule` fails at the
    /// current position.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::inputs::StringInput;
    /// # use pest::state;
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = StringInput::new("a");
    /// let (mut state, _) = state::<Rule, _>(input);
    /// let pos = state.start();
    ///
    /// state.track_neg(Rule::a, pos);
    /// # }
    /// ```
    #[inline]
    pub fn track_neg(&mut self, rule: Rule, pos: Position<I>) {
        self.track(false, rule, pos)
    }

    #[inline]
    fn track(&mut self, positive: bool, rule: Rule, pos: Position<I>) {
        if self.is_atomic || self.dest == TokenDestination::Ignore {
            return
        }

        let mut attempts = if positive {
            &mut self.pos_attempts
        } else {
            &mut self.neg_attempts
        };

        if attempts.is_empty() {
            attempts.push(rule);

            self.attempt_pos = pos.pos();
        } else if pos.pos() == self.attempt_pos {
            attempts.push(rule);
        } else if pos.pos() > self.attempt_pos {
            attempts.clear();
            attempts.clear();
            attempts.push(rule);

            self.attempt_pos = pos.pos();
        }
    }
}
