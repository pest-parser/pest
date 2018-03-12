// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::rc::Rc;
use std::ops::Range;

use RuleType;
use error::Error;
use iterators::{pairs, QueueableToken};
use position::{self, Position};
use span::Span;
use stack::Stack;

/// An `enum` specifying the current lookahead status of a `ParserState`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Lookahead {
    Positive,
    Negative,
    None
}

/// An `enum` specifying the current atomicity of a `ParserState`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Atomicity {
    Atomic,
    CompoundAtomic,
    NonAtomic
}

/// A `struct` which contains the complete state of a `Parser`.
#[derive(Debug)]
pub struct ParserState<'i, R: RuleType> {
    position: Position<'i>,
    queue: Vec<QueueableToken<R>>,
    lookahead: Lookahead,
    pos_attempts: Vec<R>,
    neg_attempts: Vec<R>,
    attempt_pos: usize,
    /// Specifies current atomicity
    pub atomicity: Atomicity,
    /// Stack of `Span`s
    pub stack: Stack<Span<'i>>
}

pub type ParseResult<'i, R> = Result<ParserState<'i, R>, ParserState<'i, R>>;

/// Creates a `ParserState` from a `&str`, supplying it to a closure `f`.
///
/// # Examples
///
/// ```
/// # use std::rc::Rc;
/// # use pest;
///
/// let input = "";
/// pest::state::<(), _>(input, |s| {
///     Ok(s)
/// }).unwrap();
/// ```
pub fn state<'i, R: RuleType, F>(input: &'i str, f: F) -> Result<pairs::Pairs<'i, R>, Error<'i, R>>
where
    F: FnOnce(ParserState<'i, R>) -> ParseResult<R>
{
    let state = ParserState::new(input);

    match f(state) {
        Ok(s) => {
            let len = s.queue.len();
            Ok(pairs::new(Rc::new(s.queue), input.as_bytes(), 0, len))
        }
        Err(mut s) => {
            s.pos_attempts.sort();
            s.pos_attempts.dedup();
            s.neg_attempts.sort();
            s.neg_attempts.dedup();

            Err(Error::ParsingError {
                positives: s.pos_attempts,
                negatives: s.neg_attempts,
                // All attempted positions were legal.
                pos: unsafe { position::new(input.as_bytes(), s.attempt_pos) }
            })
        }
    }
}

impl<'i, R: RuleType> ParserState<'i, R> {

    ///
    ///
    pub fn new(input: &'i str) -> Self {
        ParserState {
            position: Position::from_start(input),
            queue: vec![],
            lookahead: Lookahead::None,
            pos_attempts: vec![],
            neg_attempts: vec![],
            attempt_pos: 0,
            atomicity: Atomicity::NonAtomic,
            stack: Stack::new()
        }
    }

    ///
    ///
    pub fn get_position(&self) -> &Position<'i> {
        &self.position
    }

    ///
    ///
    pub fn clone_position(&self) -> Position<'i> {
        self.position.clone()
    }

    pub fn with_updated_position(mut self, position: Position<'i>) -> Self {
        self.position = position;
        self
    }

    /// Wrapper needed to generate tokens.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a";
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.rule(Rule::a, |s| Ok(s))
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 1);
    /// ```
    #[inline]
    pub fn rule<F>(
        mut self,
        rule: R,
        f: F
    ) -> ParseResult<'i, R>
    where
        F: FnOnce(ParserState<'i, R>) -> ParseResult<'i, R>
    {
        let actual_pos = self.position.pos();
        let index = self.queue.len();

        let (pos_attempts_index, neg_attempts_index) = if actual_pos == self.attempt_pos {
            (self.pos_attempts.len(), self.neg_attempts.len())
        } else {
            // Attempts have not been cleared yet since the attempt_pos is older.
            (0, 0)
        };

        if self.lookahead == Lookahead::None && self.atomicity != Atomicity::Atomic {
            // Pair's position will only be known after running the closure.
            self.queue.push(QueueableToken::Start {
                pair: 0,
                pos: actual_pos
            });
        }

        let attempts = self.pos_attempts.len() + self.neg_attempts.len();

        let result = f(self);

        match result {
            Ok(mut new_state) => {
                if new_state.lookahead == Lookahead::Negative {
                    new_state.track(
                        rule,
                        actual_pos,
                        pos_attempts_index,
                        neg_attempts_index,
                        attempts
                    );
                }

                if new_state.lookahead == Lookahead::None && new_state.atomicity != Atomicity::Atomic {
                    // Storing the pair's index in the first token that was added before the closure was
                    // run.
                    let new_index = new_state.queue.len();
                    match new_state.queue[index] {
                        QueueableToken::Start { ref mut pair, .. } => *pair = new_index,
                        _ => unreachable!()
                    };

                    new_state.queue.push(QueueableToken::End {
                        rule,
                        pos: new_state.position.pos()
                    });
                }

                Ok(new_state)
            }
            Err(mut new_state) => {
                if new_state.lookahead != Lookahead::Negative {
                    new_state.track(
                        rule,
                        actual_pos,
                        pos_attempts_index,
                        neg_attempts_index,
                        attempts
                    );
                }

                if new_state.lookahead == Lookahead::None && new_state.atomicity != Atomicity::Atomic {
                    new_state.queue.truncate(index);
                }

                Err(new_state)
            }
        }
    }

    fn track(
        &mut self,
        rule: R,
        pos: usize,
        pos_attempts_index: usize,
        neg_attempts_index: usize,
        prev_attempts: usize
    ) {
        if self.atomicity == Atomicity::Atomic {
            return;
        }

        // If nested rules made no progress, there is no use to report them; it's only useful to
        // track the current rule, the exception being when only one attempt has been made during
        // the children rules.
        let curr_attempts = self.pos_attempts.len() + self.neg_attempts.len();
        if curr_attempts > prev_attempts && curr_attempts - prev_attempts == 1 {
            return;
        }

        if pos == self.attempt_pos {
            self.pos_attempts.truncate(pos_attempts_index);
            self.neg_attempts.truncate(neg_attempts_index);
        }

        if pos > self.attempt_pos {
            self.pos_attempts.clear();
            self.neg_attempts.clear();
            self.attempt_pos = pos;
        }

        let attempts = if self.lookahead != Lookahead::Negative {
            &mut self.pos_attempts
        } else {
            &mut self.neg_attempts
        };

        if pos == self.attempt_pos {
            attempts.push(rule);
        }
    }

    /// Wrapper which removes `Tokens` in case of a sequence's failure.
    ///
    /// Usually used in conjunction with
    /// [`Position::sequence`](struct.Position.html#method.sequence).
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a";
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.sequence(|s| {
    ///         s.rule(Rule::a, |s| Ok(s)).and_then(|s| {
    ///             s.match_string("b")
    ///         })
    ///     }).or_else(|s| {
    ///         Ok(s)
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn sequence<F>(self, f: F) -> ParseResult<'i, R>
    where
        F: FnOnce(ParserState<'i, R>) -> ParseResult<'i, R>
    {
        let token_index = self.queue.len();
        let initial_pos = self.clone_position();

        let result = f(self);

        match result {
            Ok(new_state) => Ok(new_state),
            Err(mut new_state) => {
                // Restore the initial position and truncate the token queue.
                new_state.position = initial_pos;
                new_state.queue.truncate(token_index);
                Err(new_state)
            }
        }
    }

    #[inline]
    pub fn repeat<F>(self, mut f: F) -> ParseResult<'i, R>
    where
        F: FnMut(ParserState<'i, R>) -> ParseResult<'i, R>
    {
        let mut result = f(self);

        loop {
            match result {
                Ok(state) => result = f(state),
                Err(state) => return Ok(state)
            };
        }
    }

    #[inline]
    pub fn optional<F>(self, f: F) -> ParseResult<'i, R>
    where
        F: FnOnce(ParserState<'i, R>) -> ParseResult<'i, R>
    {
        let result = f(self);

        match result {
            Ok(state) | Err(state) => Ok(state)
        }
    }

    #[inline]
    pub fn match_string(mut self, string: &str) -> ParseResult<'i, R> {
        // TODO: Set up a macro for this...
        match self.position.match_string(string) {
            Ok(pos) => {
                self.position = pos;
                Ok(self)
            },
            Err(pos) => {
                self.position = pos;
                Err(self)
            }
        }
    }

    #[inline]
    pub fn match_insensitive(mut self, string: &str) -> ParseResult<'i, R> {
        // TODO: Set up a macro for this...
        match self.position.match_insensitive(string) {
            Ok(pos) => {
                self.position = pos;
                Ok(self)
            },
            Err(pos) => {
                self.position = pos;
                Err(self)
            }
        }
    }

    #[inline]
    pub fn match_range(mut self, range: Range<char>) -> ParseResult<'i, R> {
        // TODO: Set up a macro for this...
        match self.position.match_range(range) {
            Ok(pos) => {
                self.position = pos;
                Ok(self)
            },
            Err(pos) => {
                self.position = pos;
                Err(self)
            }
        }
    }

    #[inline]
    pub fn skip(mut self, n: usize) -> ParseResult<'i, R> {
        match self.position.skip(n) {
            Ok(pos) => {
                self.position = pos;
                Ok(self)
            },
            Err(pos) => {
                self.position = pos;
                Err(self)
            }
        }
    }

    #[inline]
    pub fn skip_until(mut self, string: &str) -> ParseResult<'i, R> {
        match self.position.skip_until(string) {
            Ok(pos) => {
                self.position = pos;
                Ok(self)
            },
            Err(pos) => {
                self.position = pos;
                Err(self)
            }
        }
    }

    #[inline]
    pub fn start_of_input(mut self) -> ParseResult<'i, R> {
        match self.position.at_start() {
            Ok(pos) => {
                self.position = pos;
                Ok(self)
            },
            Err(pos) => {
                self.position = pos;
                Err(self)
            }
        }
    }

    #[inline]
    pub fn end_of_input(mut self) -> ParseResult<'i, R> {
        match self.position.at_end() {
            Ok(pos) => {
                self.position = pos;
                Ok(self)
            },
            Err(pos) => {
                self.position = pos;
                Err(self)
            }
        }
    }

    /// Wrapper which stops `Token`s from being generated.
    ///
    /// Usually used in conjunction with
    /// [`Position::lookahead`](struct.Position.html#method.lookahead).
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a";
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.lookahead(true, |state| {
    ///         state.rule(Rule::a, |s| Ok(s))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn lookahead<F>(mut self, is_positive: bool, f: F) -> ParseResult<'i, R>
    where
        F: FnOnce(ParserState<'i, R>) -> ParseResult<'i, R>
    {
        let initial_lookahead = self.lookahead;

        self.lookahead = if is_positive {
            match initial_lookahead {
                Lookahead::None | Lookahead::Positive => Lookahead::Positive,
                Lookahead::Negative => Lookahead::Negative
            }
        } else {
            match initial_lookahead {
                Lookahead::None | Lookahead::Positive => Lookahead::Negative,
                Lookahead::Negative => Lookahead::Positive
            }
        };

        let initial_pos = self.clone_position();

        let result = f(self);

        let return_state = match result {
            Ok(mut new_state) => {
                new_state.position = initial_pos;
                new_state.lookahead = initial_lookahead;
                Ok(new_state)
            }
            Err(mut new_state) => {
                new_state.position = initial_pos;
                new_state.lookahead = initial_lookahead;
                Err(new_state)
            }
        };

        if is_positive {
            return_state
        } else {
            match return_state {
                Ok(s) => Err(s),
                Err(s) => Ok(s)
            }
        }

    }

    /// Wrapper which stops `Token`s from being generated according to `is_atomic`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::{self, Atomicity};
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a";
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.atomic(Atomicity::Atomic, |s| {
    ///         s.rule(Rule::a, |s| Ok(s))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn atomic<F>(mut self, atomicity: Atomicity, f: F) -> ParseResult<'i, R>
    where
        F: FnOnce(ParserState<'i, R>) -> ParseResult<'i, R>
    {
        let initial_atomicity = self.atomicity;
        let should_toggle = self.atomicity != atomicity;

        if should_toggle {
            self.atomicity = atomicity;
        }

        let result = f(self);

        match result {
            Ok(mut new_state) => {
                if should_toggle {
                    new_state.atomicity = initial_atomicity;
                }
                Ok(new_state)
            }
            Err(mut new_state) => {
                if should_toggle {
                    new_state.atomicity = initial_atomicity;
                }
                Err(new_state)
            }
        }
    }
}
