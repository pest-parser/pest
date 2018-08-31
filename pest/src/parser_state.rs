// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::ops::Range;
use std::rc::Rc;

use RuleType;
use error::{Error, ErrorVariant};
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
    atomicity: Atomicity,
    stack: Stack<Span<'i>>
}

/// Creates a `ParserState` from a `&str`, supplying it to a closure `f`.
///
/// # Examples
///
/// ```
/// # use pest;
/// let input = "";
/// pest::state::<(), _>(input, |s| Ok(s)).unwrap();
/// ```
pub fn state<'i, R: RuleType, F>(input: &'i str, f: F) -> Result<pairs::Pairs<'i, R>, Error<R>>
where
    F: FnOnce(Box<ParserState<'i, R>>) -> ParseResult<Box<ParserState<'i, R>>>
{
    let state = ParserState::new(input);

    match f(state) {
        Ok(state) => {
            let len = state.queue.len();
            Ok(pairs::new(Rc::new(state.queue), input.as_bytes(), 0, len))
        }
        Err(mut state) => {
            state.pos_attempts.sort();
            state.pos_attempts.dedup();
            state.neg_attempts.sort();
            state.neg_attempts.dedup();

            Err(Error::new_from_pos(
                ErrorVariant::ParsingError {
                    positives: state.pos_attempts.clone(),
                    negatives: state.neg_attempts.clone()
                },
                unsafe { position::new(input.as_bytes(), state.attempt_pos) }
            ))
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
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let position = state.position();
    /// assert_eq!(position.pos(), 0);
    /// ```
    pub fn position(&self) -> &Position<'i> {
        &self.position
    }

    /// Returns the current atomicity of the `ParserState`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # use pest::Atomicity;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let atomicity = state.atomicity();
    /// assert_eq!(atomicity, Atomicity::NonAtomic);
    /// ```
    pub fn atomicity(&self) -> Atomicity {
        self.atomicity
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
    pub fn rule<F>(mut self: Box<Self>, rule: R, f: F) -> ParseResult<Box<Self>>
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
                end_token_index: 0,
                input_pos: actual_pos
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

                if new_state.lookahead == Lookahead::None
                    && new_state.atomicity != Atomicity::Atomic
                {
                    // Storing the pair's index in the first token that was added before the closure was
                    // run.
                    let new_index = new_state.queue.len();
                    match new_state.queue[index] {
                        QueueableToken::Start {
                            ref mut end_token_index,
                            ..
                        } => *end_token_index = new_index,
                        _ => unreachable!()
                    };

                    let new_pos = new_state.position.pos();

                    new_state.queue.push(QueueableToken::End {
                        start_token_index: index,
                        rule,
                        input_pos: new_pos
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

                if new_state.lookahead == Lookahead::None
                    && new_state.atomicity != Atomicity::Atomic
                {
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
        let initial_pos = self.position.clone();

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
    /// assert_eq!(result.unwrap().position().pos(), 2);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.repeat(|s| {
    ///     s.match_string("b")
    /// });
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 0);
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
    /// ```
    #[inline]
    pub fn optional<F>(self: Box<Self>, f: F) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
    {
        match f(self) {
            Ok(state) | Err(state) => Ok(state)
        }
    }

    /// Asks the `ParserState` to match a single character based on a filter function.
    /// If the match is successful, this will return an `Ok` with the updated `Box<ParserState>`.
    /// If failed, an `Err` with the updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let result = state.match_char_by(|c| c.is_ascii());
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 1);
    ///
    /// let input = "❤";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let result = state.match_char_by(|c| c.is_ascii());
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().position().pos(), 0);
    /// ```
    #[inline]
    pub fn match_char_by<F>(mut self: Box<Self>, f: F) -> ParseResult<Box<Self>>
    where F: FnOnce(char) -> bool
    {
        if self.position.match_char_by(f) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match the given `string`. If the match is successful, this will
    /// return an `Ok` with the updated `Box<ParserState>`. If failed, an `Err` with the updated
    /// `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_string("ab");
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.match_string("ac");
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().position().pos(), 0);
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
    /// successful, this will return an `Ok` with the updated `Box<ParserState>`. If failed, an
    /// `Err` with the updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_insensitive("AB");
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.match_insensitive("AC");
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().position().pos(), 0);
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
    /// successful, this will return an `Ok` with the updated `Box<ParserState>`. If failed, an
    /// `Err` with the updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_range('a'..'z');
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 1);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.match_range('A'..'Z');
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().position().pos(), 0);
    /// ```
    #[inline]
    pub fn match_range(mut self: Box<Self>, range: Range<char>) -> ParseResult<Box<Self>> {
        if self.position.match_range(range) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to skip `n` `char`s. If the match is successful, this will return an
    /// `Ok` with the updated `Box<ParserState>`. If failed, an `Err` with the updated
    /// `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.skip(1);
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 1);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.skip(3);
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().position().pos(), 0);
    /// ```
    #[inline]
    pub fn skip(mut self: Box<Self>, n: usize) -> ParseResult<Box<Self>> {
        if self.position.skip(n) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to continue to skip until one of the given `strings` is found. If
    /// the match is successful, this will return an `Ok` with the updated `Box<ParserState>`. If
    /// failed, an `Err` with the updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "abcd";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.skip_until(&["c", "d"]);
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    /// ```
    #[inline]
    pub fn skip_until(mut self: Box<Self>, strings: &[&str]) -> ParseResult<Box<Self>> {
        if self.position.skip_until(strings) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to continue to skip until one of the given `bytes` is found. If
    /// the match is successful, this will return an `Ok` with the updated `Box<ParserState>`. If
    /// failed, an `Err` with the updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "abcd";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.skip_until_bytes(&[99, 100]); // c: 99, d: 100
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    /// ```
    #[cfg(not(feature = "no_std"))]
    #[inline]
    pub fn skip_until_bytes(mut self: Box<Self>, bytes: &[u8]) -> ParseResult<Box<Self>> {
        if self.position.skip_until_bytes(bytes) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match the start of the input. If the match is successful, this
    /// will return an `Ok` with the updated `Box<ParserState>`. If failed, an `Err` with the
    /// updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
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

    /// Asks the `ParserState` to match the end of the input. If the match is successful, this will
    /// return an `Ok` with the updated `Box<ParserState>`. If failed, an `Err` with the updated
    /// `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
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
    /// `Ok` with the current `Box<ParserState>` if `f` also returns an `Ok` or `Err` with the current
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

        let initial_pos = self.position.clone();

        let result = f(self.checkpoint());

        let result_state = match result {
            Ok(mut new_state) => {
                new_state.position = initial_pos;
                new_state.lookahead = initial_lookahead;
                Ok(new_state.restore())
            }
            Err(mut new_state) => {
                new_state.position = initial_pos;
                new_state.lookahead = initial_lookahead;
                Err(new_state.restore())
            }
        };

        if is_positive {
            result_state
        } else {
            match result_state {
                Ok(state) => Err(state),
                Err(state) => Ok(state)
            }
        }
    }

    /// Transformation which stops `Token`s from being generated according to `is_atomic`.
    ///
    /// # Examples
    ///
    /// ```
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

    /// Evaluates the result of closure `f` and pushes the span of the input consumed from before
    /// `f` is called to after `f` is called to the stack. Returns an `Ok(Box<ParserState>)` if
    /// `f` is called successfully, `Err(Box<ParserState>)` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.stack_push(|state| state.match_string("a"));
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 1);
    /// ```
    #[inline]
    pub fn stack_push<F>(self: Box<Self>, f: F) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
    {
        let start = self.position.clone();

        let result = f(self);

        match result {
            Ok(mut state) => {
                let end = state.position.clone();
                state.stack.push(start.span(&end));
                Ok(state)
            }
            Err(state) => Err(state)
        }
    }

    /// Peeks the top of the stack and attempts to match the string. Returns an
    /// `Ok(Box<ParserState>)` if the string is matched successfully, `Err(Box<ParserState>)`
    /// otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "aa";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.stack_push(|state| state.match_string("a")).and_then(
    ///     |state| state.stack_peek()
    /// );
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    /// ```
    #[inline]
    pub fn stack_peek(self: Box<Self>) -> ParseResult<Box<Self>> {
        let string = self.stack
            .peek()
            .expect("peek was called on empty stack")
            .as_str();
        self.match_string(string)
    }

    /// Pops the top of the stack and attempts to match the string. Returns an
    /// `Ok(Box<ParserState>)` if the string is matched successfully, `Err(Box<ParserState>)`
    /// otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "aa";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.stack_push(|state| state.match_string("a")).and_then(
    ///     |state| state.stack_pop()
    /// );
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    /// ```
    #[inline]
    pub fn stack_pop(mut self: Box<Self>) -> ParseResult<Box<Self>> {
        let string = self.stack
            .pop()
            .expect("pop was called on empty stack")
            .as_str();
        self.match_string(string)
    }

    /// Matches the full state of the stack.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "aaaa";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.stack_push(|state| state.match_string("a")).and_then(|state| {
    ///     state.stack_push(|state| state.match_string("a"))
    /// }).and_then(|state| state.stack_match_peek());
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 4);
    /// ```
    #[inline]
    pub fn stack_match_peek(mut self: Box<Self>) -> ParseResult<Box<Self>> {
        let mut position = self.position.clone();
        let result = self.stack.iter().all(|span| {
            position.match_string(span.as_str())
        });

        if result {
            self.position = position;
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches the full state of the stack. This method will clear the stack as it evaluates.
    ///
    /// # Examples
    ///
    /// ```
    /// /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "aaaa";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.stack_push(|state| state.match_string("a")).and_then(|state| {
    ///     state.stack_push(|state| state.match_string("a"))
    /// }).and_then(|state| state.stack_match_peek());
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 4);
    /// ```
    #[inline]
    pub fn stack_match_pop(mut self: Box<Self>) -> ParseResult<Box<Self>> {
        let mut position = self.position.clone();
        let mut result = true;
        while self.stack.peek().is_some() {
            let span = self.stack.pop().unwrap();
            result = position.match_string(span.as_str());
            if !result {
                break;
            }
        }

        if result {
            self.position = position;
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Drops the top of the stack and returns `Ok(Box<ParserState>)` if there was a value to
    /// drop. Otherwise, it returns `Err(Box<ParserState>)`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "aa";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.stack_push(|state| state.match_string("a")).and_then(
    ///     |state| state.stack_drop()
    /// );
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 1);
    /// ```
    #[inline]
    pub fn stack_drop(mut self: Box<Self>) -> ParseResult<Box<Self>> {
        match self.stack.pop() {
            Some(_) => Ok(self),
            None => Err(self)
        }
    }

    /// Restores the original state of the `ParserState` when `f` returns an `Err`. Currently,
    /// this method only restores the stack.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.restore_on_err(|state| state.stack_push(|state|
    ///     state.match_string("a")).and_then(|state| state.match_string("a"))
    /// );
    ///
    /// assert!(result.is_err());
    ///
    /// // Since the the rule doesn't match, the "a" pushed to the stack will be removed.
    /// let catch_panic = std::panic::catch_unwind(|| result.unwrap_err().stack_pop());
    /// assert!(catch_panic.is_err());
    /// ```
    #[inline]
    pub fn restore_on_err<F>(self: Box<Self>, f: F) -> ParseResult<Box<Self>>
    where
        F: FnOnce(Box<Self>) -> ParseResult<Box<Self>>
    {
        match f(self.checkpoint()) {
            Ok(state) => Ok(state),
            Err(state) => Err(state.restore())
        }
    }

    // Mark the current state as a checkpoint and return the `Box`.
    #[inline]
    pub(crate) fn checkpoint(mut self: Box<Self>) -> Box<Self> {
        self.stack.snapshot();
        self
    }

    // Restore the current state to the most recent checkpoint.
    #[inline]
    pub(crate) fn restore(mut self: Box<Self>) -> Box<Self> {
        self.stack.restore();
        self
    }
}
