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

/// Type alias to simplify specifying the return value of the chained closures.
pub type ParseResult<S> = Result<S, S>;

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

/// Creates a `ParserState` from a `&str`, supplying it to a closure `f`.
///
/// # Examples
///
/// ```
/// # use pest;
///
/// let input = "";
/// pest::state::<(), _>(input, |s| {
///     Ok(s)
/// }).unwrap();
/// ```
pub fn state<'i, R: RuleType, F>(input: &'i str, f: F) -> Result<pairs::Pairs<'i, R>, Error<'i, R>>
where
    F: FnOnce(Box<ParserState<'i, R>>) -> ParseResult<Box<ParserState<'i, R>>>
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
                positives: s.pos_attempts.clone(),
                negatives: s.neg_attempts.clone(),
                // All attempted positions were legal.
                pos: unsafe { position::new(input.as_bytes(), s.attempt_pos) }
            })
        }
    }
}

impl<'i, R: RuleType> ParserState<'i, R> {

    /// Allocates a fresh `ParserState` object to the heap and returns the owned `Box`. This `Box`
    /// will be passed from closure to closure based on the needs of the specified `Parser`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    ///
    /// let input = "";
    /// let state: Box<pest::ParserState<&str>> = pest::ParserState::new(input);
    /// ```
    pub fn new(input: &'i str) -> Box<Self> {
        Box::new(ParserState {
            position: Position::from_start(input),
            queue: vec![],
            lookahead: Lookahead::None,
            pos_attempts: vec![],
            neg_attempts: vec![],
            attempt_pos: 0,
            atomicity: Atomicity::NonAtomic,
            stack: Stack::new()
        })
    }

    /// Returns a reference to the current `Position` of the `ParserState`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let position = state.get_position();
    /// assert_eq!(position.pos(), 0);
    /// ```
    pub fn get_position(&self) -> &Position<'i> {
        &self.position
    }

    /// Returns a clone of the current `Position` of the `ParserState`. Ideally, this will likely
    /// not be needed as manually manipulating the `Position` can lead to a future inconsistent
    /// state.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let position = state.clone_position();
    /// assert_eq!(position.pos(), 0);
    /// ```
    pub fn clone_position(&self) -> Position<'i> {
        self.position.clone()
    }

    /// Given a `Position`, replace the current `Position` of the `ParserState` with the new one.
    ///
    /// # Examples
    ///
    /// ```
    ///
    /// ```
    pub fn with_updated_position(mut self: Box<Self>, position: Position<'i>) -> Box<Self> {
        self.position = position;
        self
    }

    /// Wrapper needed to generate tokens. This will associate the `R` type rule to the closure
    /// meant to match the rule.
    ///
    /// # Examples
    ///
    /// ```
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
        mut self: Box<Self>,
        rule: R,
        f: F
    ) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
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

                    let new_pos= new_state.position.pos();

                    new_state.queue.push(QueueableToken::End {
                        rule,
                        pos: new_pos
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

    /// Starts a sequence of transformations provided by `f` from the `Box<ParserState>`. It returns the
    /// same `Result` returned by `f` in the case of an `Ok` or `Err` with the current `Box<ParserState>`
    /// otherwise.
    ///
    /// This method is useful to parse sequences that only match together which usually come in the
    /// form of chained `Result`s with
    /// [`Result::and_then`](https://doc.rust-lang.org/std/result/enum.Result.html#method.and_then).
    ///
    ///
    /// # Examples
    ///
    /// ```
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
    pub fn sequence<F>(self: Box<Self>, f: F) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
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

    /// Repeatedly applies the transformation provided by `f` from the `Box<ParserState>`. It
    /// returns `Ok` with the updated `Box<ParserState>` returned by `f` wrapped up in an `Err`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "aab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.repeat(|s| {
    ///     s.match_string("a")
    /// });
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().get_position().pos(), 2);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.repeat(|s| {
    ///     s.match_string("b")
    /// });
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().get_position().pos(), 0);
    /// ```
    #[inline]
    pub fn repeat<F>(self: Box<Self>, mut f: F) -> ParseResult<Box<Self>>
    where
        F: FnMut(Box<Self>) -> ParseResult<Box<Self>>
    {
        let mut result = f(self);

        loop {
            match result {
                Ok(state) => result = f(state),
                Err(state) => return Ok(state)
            };
        }
    }

    /// Optionally applies the transformation provided by `f` from the `Box<ParserState>`. It returns `Ok`
    /// with the updated `Box<ParserState>` returned by `f` regardless of the `Result`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let result = state.optional(|s| {
    ///     s.match_string("ab")
    /// });
    /// assert!(result.is_ok());
    ///
    /// state = pest::ParserState::new(input);
    /// let result = state.optional(|s| {
    ///     s.match_string("ac")
    /// });
    /// assert!(result.is_ok());
    ///
    /// ```
    #[inline]
    pub fn optional<F>(self: Box<Self>, f: F) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
    {
        let result = f(self);

        match result {
            Ok(state) | Err(state) => Ok(state)
        }
    }

    /// Asks the `ParserState` to match the given `string`. If the match is successful, the
    /// `position` will be updated and an `Ok` `Result` is returned. If failed, an `Err` is
    /// returned.
    ///
    /// # Examples
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_string("ab");
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().get_position().pos(), 2);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.match_string("ac");
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().get_position().pos(), 0);
    /// ```
    #[inline]
    pub fn match_string(mut self: Box<Self>, string: &str) -> ParseResult<Box<Self>> {
        if self.position.match_string(string) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to case-insensitively match the given `string`. If the match is
    /// successful, the `position` will be updated and an `Ok` `Result` is returned. If failed, an
    /// `Err` is returned.
    ///
    /// # Examples
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_insensitive("AB");
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().get_position().pos(), 2);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.match_insensitive("AC");
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().get_position().pos(), 0);
    /// ```
    #[inline]
    pub fn match_insensitive(mut self: Box<Self>, string: &str) -> ParseResult<Box<Self>> {
        if self.position.match_insensitive(string) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match a `char` `range` from the given `string`. If the match is
    /// successful, the `position` will be updated and an `Ok` `Result` is returned. If failed, an
    /// `Err` is returned.
    ///
    /// # Examples
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_range('a'..'z');
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().get_position().pos(), 1);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.match_range('A'..'Z');
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().get_position().pos(), 0);
    /// ```
    #[inline]
    pub fn match_range(mut self: Box<Self>, range: Range<char>) -> ParseResult<Box<Self>> {
        if self.position.match_range(range) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to skip `n` `char`s. If the match is successful, the `position` will
    /// be updated and an `Ok` `Result` is returned. If failed, an `Err` is returned.
    ///
    /// # Examples
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.skip(1);
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().get_position().pos(), 1);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.skip(3);
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().get_position().pos(), 0);
    /// ```
    #[inline]
    pub fn skip(mut self: Box<Self>, n: usize) -> ParseResult<Box<Self>> {
        if self.position.skip(n) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to skip until the first occurrence of `string`. If the match is
    /// successful, the `position` will be updated and an `Ok` `Result` is returned. If failed, an
    /// `Err` is returned.
    ///
    /// # Examples
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.skip_until("b");
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().get_position().pos(), 1);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.skip_until("c");
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().get_position().pos(), 0);
    /// ```
    #[inline]
    pub fn skip_until(mut self: Box<Self>, string: &str) -> ParseResult<Box<Self>> {
        if self.position.skip_until(string) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match the start of the input. If the match is successful, an
    /// `Ok` `Result` is returned. If failed, an `Err` is returned.
    ///
    /// # Examples
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.start_of_input();
    /// assert!(result.is_ok());
    ///
    /// state = pest::ParserState::new(input);
    /// state = state.match_string("ab").unwrap();
    /// result = state.start_of_input();
    /// assert!(result.is_err());
    /// ```
    #[inline]
    pub fn start_of_input(self: Box<Self>) -> ParseResult<Box<Self>> {
        if self.position.at_start() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match the end of the input. If the match is successful, an
    /// `Ok` `Result` is returned. If failed, an `Err` is returned.
    ///
    /// # Examples
    /// ```
    /// # use pest;
    ///
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.end_of_input();
    /// assert!(result.is_err());
    ///
    /// state = pest::ParserState::new(input);
    /// state = state.match_string("ab").unwrap();
    /// result = state.end_of_input();
    /// assert!(result.is_ok());
    /// ```
    #[inline]
    pub fn end_of_input(self: Box<Self>) -> ParseResult<Box<Self>> {
        if self.position.at_end() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Starts a lookahead transformation provided by `f` from the `Box<ParserState>`. It returns
    /// `Ok` with the current position if `f` also returns an `Ok ` or `Err` with the current
    /// `Box<ParserState>` otherwise. If `is_positive` is `false`, it swaps the `Ok` and `Err`
    /// together, negating the `Result`.
    ///
    /// # Examples
    ///
    /// ```
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
    pub fn lookahead<F>(mut self: Box<Self>, is_positive: bool, f: F) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
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

        let result_state = match result {
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
            result_state
        } else {
            match result_state {
                Ok(s) => Err(s),
                Err(s) => Ok(s)
            }
        }

    }

    /// Transformation which stops `Token`s from being generated according to `is_atomic`.
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
    pub fn atomic<F>(mut self: Box<Self>, atomicity: Atomicity, f: F) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
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
