// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Debug;

use super::inputs::Input;
use super::error::Error;
use super::streams_private::buffered::{buffered, BufferedSender};
use super::streams_private::parser_stream;
use super::tokens::Token;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TokenDestination {
    Stream,
    Queue,
    Ignore
}

/// A `struct` which contains the complete state of a `Parser`.
pub struct ParserState<'a, Rule, I: 'a + Input> {
    input:           &'a I,
    pos:             usize,
    sender:          BufferedSender<Token<Rule>, Error<Rule>>,
    queue:           Vec<Token<Rule>>,
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
/// # use pest::{state, StringInput};
/// # fn main() {
/// let input = StringInput::new("a");
///
/// let (_, _) = state::<(), _>(&input);
/// # }
/// ```
pub fn state<Rule: Copy+ Debug + Eq + 'static, I: Input>(input: &I)
    -> (parser_stream::ParserStream<Rule>, ParserState<Rule, I>) {

    let (sender, stream) = buffered(1024);

    let state = ParserState {
        input:        input,
        pos:          0,
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

    let stream = parser_stream::new(stream);

    (stream, state)
}

impl<'a, Rule: Clone + Ord, I: Input> ParserState<'a, Rule, I> {
    /// Sends `token` according to the `ParserState`'s destination. The `Token` will get sent to the
    /// `TokenStream`, queued up to be sent later, or ignored.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let input = StringInput::new("");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// state.send(Token::Start { rule: (), pos: 0 });
    /// # }
    /// ```
    #[inline]
    pub fn send(&mut self, token: Token<Rule>) {
        match self.dest {
            TokenDestination::Stream => self.sender.send(token),
            TokenDestination::Queue  => self.queue.push(token),
            TokenDestination::Ignore => ()
        };
    }

    /// Consumes the `ParserState` and causes the matching `ParserStream` to fail.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # use pest::Error;
    /// # fn main() {
    /// let input = StringInput::new("");
    /// let (_, state) = state::<(), _>(&input);
    ///
    /// state.fail(Error::CustomErrorPos("error".to_owned(), 0));
    /// # }
    /// ```
    #[inline]
    pub fn fail(self, error: Error<Rule>) {
        self.sender.fail(error);
    }

    /// Returns whether the position is at the start of the `Input`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("ab");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(state.at_start());
    /// state.match_string("ab");
    /// assert!(!state.at_start());
    /// # }
    /// ```
    #[inline]
    pub fn at_start(&self) -> bool {
        self.pos == 0
    }

    /// Returns whether the position is at the end of the `Input`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("ab");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(!state.at_end());
    /// state.match_string("ab");
    /// assert!(state.at_end());
    /// # }
    /// ```
    #[inline]
    pub fn at_end(&self) -> bool {
        self.pos == self.input.len()
    }

    /// Matches `string`, returns whether it matched, and advances the position with `string.len()`
    /// in case it did.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("abcd");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(state.match_string("ab"));
    /// assert!(state.match_string("cd"));
    /// # }
    /// ```
    #[inline]
    pub fn match_string(&mut self, string: &str) -> bool {
        let result = self.input.match_string(string, self.pos);

        if result {
            self.pos += string.len();
        }

        result
    }

    /// Matches `string` case insensitively, returns whether it matched, and advances the position
    /// with `string.len()` in case it did.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("AbcD");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(state.match_insensitive("ab"));
    /// assert!(state.match_insensitive("cd"));
    /// # }
    /// ```
    #[inline]
    pub fn match_insensitive(&mut self, string: &str) -> bool {
        let result = self.input.match_insensitive(string, self.pos);

        if result {
            self.pos += string.len();
        }

        result
    }

    /// Matches if the current `char` is between `left` and `right`, and advances the position with
    /// one `char` length in case it did.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("Cd");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(state.match_range('A', 'Z'));
    /// assert!(state.match_range('a', 'z'));
    /// # }
    /// ```
    #[inline]
    pub fn match_range(&mut self, left: char, right: char) -> bool {
        let result = self.input.match_range(left, right, self.pos);

        if result {
            self.pos += left.len_utf8();
        }

        result
    }

    /// Matches the current `rule`, queues up all generated `Token`s, and reverts the state if the
    /// `rule` fails.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("abacad");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(state.queued(|state| {
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(state.queued(|state| {
    ///     state.match_string("a") && state.match_string("c")
    /// }));
    /// assert!(!state.queued(|state| {
    ///     state.match_string("a") && state.match_string("c")
    /// }));
    /// # }
    /// ```
    #[inline]
    pub fn queued<F>(&mut self, rule: F) -> bool
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> bool {

        let initial_pos = self.pos;
        let should_toggle = self.dest == TokenDestination::Stream;

        if should_toggle {
            self.dest = TokenDestination::Queue;
        }

        let result = rule(self);

        if should_toggle {
            self.dest = TokenDestination::Stream;

            if result {
                for token in self.queue.drain(..) {
                    self.sender.send(token);
                }
            } else {
                self.queue.clear();
            }
        }

        if !result {
            self.pos = initial_pos;
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
    /// # use pest::{state, StringInput};
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// let input = StringInput::new("ab");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(state.ignored(|state| {
    ///     state.send(Token::Start { rule: (), pos: 0 });
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(state.ignored(|state| {
    ///     state.send(Token::Start { rule: (), pos: 0 });
    ///     state.match_string("a") && state.match_string("b")
    /// }));
    /// assert!(!state.ignored(|state| {
    ///     state.send(Token::Start { rule: (), pos: 0 });
    ///     state.match_string("a") && state.match_string("c")
    /// }));
    /// # }
    /// ```
    #[inline]
    pub fn ignored<F>(&mut self, rule: F) -> bool
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> bool {

        let initial_pos = self.pos;
        let should_toggle = self.dest != TokenDestination::Ignore;
        let initial_dest = self.dest;

        if should_toggle {
            self.dest = TokenDestination::Ignore;
        }

        let result = rule(self);

        if should_toggle {
            self.dest = initial_dest;
        }

        self.pos = initial_pos;

        result
    }

    /// Matches the current `rule` while toggling atomicity.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("");
    /// let (_, mut state) = state::<(), _>(&input);
    ///
    /// assert!(!state.is_atomic());
    /// state.atomic(true, |state| {
    ///     assert!(state.is_atomic());
    ///     true
    /// });
    /// assert!(!state.is_atomic());
    /// # }
    /// ```
    #[inline]
    pub fn atomic<F>(&mut self, is_atomic: bool, rule: F) -> bool
        where F: FnOnce(&mut ParserState<'a, Rule, I>) -> bool {

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
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// let input = StringInput::new("");
    /// let (_, state) = state::<(), _>(&input);
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
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = StringInput::new("a");
    /// let (_, mut state) = state::<Rule, _>(&input);
    ///
    /// state.track_pos(Rule::a);
    /// # }
    /// ```
    #[inline]
    pub fn track_pos(&mut self, rule: Rule) {
        if self.is_atomic || self.dest == TokenDestination::Ignore {
            return
        }

        if self.pos_attempts.is_empty() {
            self.pos_attempts.push(rule);

            self.attempt_pos = self.pos;
        } else if self.pos == self.attempt_pos {
            self.pos_attempts.push(rule);
        } else if self.pos > self.attempt_pos {
            self.pos_attempts.clear();
            self.neg_attempts.clear();
            self.pos_attempts.push(rule);

            self.attempt_pos = self.pos;
        }
    }

    /// Keeps track of failed negative rule attempts. It should be called when a `Rule` fails at the
    /// current position.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use pest::{state, StringInput};
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = StringInput::new("a");
    /// let (_, mut state) = state::<Rule, _>(&input);
    ///
    /// state.track_neg(Rule::a);
    /// # }
    /// ```
    #[inline]
    pub fn track_neg(&mut self, rule: Rule) {
        if self.is_atomic || self.dest == TokenDestination::Ignore {
            return
        }

        if self.neg_attempts.is_empty() {
            self.neg_attempts.push(rule);

            self.attempt_pos = self.pos;
        } else if self.pos == self.attempt_pos {
            self.neg_attempts.push(rule);
        } else if self.pos > self.attempt_pos {
            self.pos_attempts.clear();
            self.neg_attempts.clear();
            self.neg_attempts.push(rule);

            self.attempt_pos = self.pos;
        }
    }
}
