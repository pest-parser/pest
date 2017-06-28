// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::rc::Rc;

use super::error::Error;
use super::inputs::{Input, Position, Span};
use super::inputs::position;
use super::iterators::{pairs, QueueableToken};
use super::RuleType;

/// An `enum` specifying the current lookahead status of a `ParserState`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Lookahead {
    Positive,
    Negative,
    None
}

/// A `struct` which contains the complete state of a `Parser`.
#[derive(Debug)]
pub struct ParserState<R: RuleType, I: Input> {
    queue: Vec<QueueableToken<R>>,
    lookahead: Lookahead,
    pos_attempts: Vec<R>,
    neg_attempts: Vec<R>,
    attempt_pos: usize,
    /// Specifies whether the current state is atomic
    pub is_atomic: bool,
    /// Stack of `Span`s
    pub stack: Vec<Span<I>>
}

/// Creates a `ParserState` from an `Input`, supplying it to a closure `f`.
///
/// # Examples
///
/// ```
/// # use std::rc::Rc;
/// # use pest;
/// # use pest::inputs::StringInput;
///
/// let input = Rc::new(StringInput::new("".to_owned()));
/// pest::state::<(), _, _>(input, |_, pos| {
///     Ok(pos)
/// }).unwrap();
/// ```
pub fn state<R: RuleType, I: Input, F>(
    input: Rc<I>,
    f: F
) -> Result<pairs::Pairs<R, I>, Error<R, I>>
where
    F: FnOnce(&mut ParserState<R, I>, Position<I>) -> Result<Position<I>, Position<I>>
{
    let mut state = ParserState {
        queue: vec![],
        lookahead: Lookahead::None,
        is_atomic: false,
        pos_attempts: vec![],
        neg_attempts: vec![],
        attempt_pos: 0,
        stack: vec![]
    };

    if f(&mut state, Position::from_start(input.clone())).is_ok() {
        let len = state.queue.len();
        Ok(pairs::new(Rc::new(state.queue), input, 0, len))
    } else {
        state.pos_attempts.sort();
        state.pos_attempts.dedup();
        state.neg_attempts.sort();
        state.neg_attempts.dedup();

        Err(Error::ParsingError {
            positives: state.pos_attempts,
            negatives: state.neg_attempts,
            pos: position::new(input, state.attempt_pos)
        })
    }
}

impl<R: RuleType, I: Input> ParserState<R, I> {
    /// Wrapper needed to generate tokens.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("a".to_owned()));
    /// let pairs: Vec<_> = pest::state(input, |state, pos| {
    ///     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 1);
    /// ```
    #[inline]
    pub fn rule<F>(&mut self, rule: R, pos: Position<I>, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(&mut ParserState<R, I>, Position<I>) -> Result<Position<I>, Position<I>>
    {
        let actual_pos = pos.pos();
        let index = self.queue.len();

        let (pos_attempts_index, neg_attempts_index) = if actual_pos == self.attempt_pos {
            (self.pos_attempts.len(), self.neg_attempts.len())
        } else { // Attempts have not been cleared yet since the attempt_pos is older.
            (0, 0)
        };

        if self.lookahead == Lookahead::None && !self.is_atomic {
            // Pair's position will only be known after running the closure.
            self.queue.push(QueueableToken::Start { pair: 0, pos: actual_pos });
        }

        let result = f(self, pos);

        if result.is_err() {
            self.track(rule, actual_pos, pos_attempts_index, neg_attempts_index);
        }

        if self.lookahead == Lookahead::None && !self.is_atomic {
            if let Ok(ref pos) = result {
                // Storing the pair's index in the first token that was added before the closure was
                // run.
                let new_index = self.queue.len();
                match self.queue[index] {
                    QueueableToken::Start { ref mut pair, .. } => *pair = new_index,
                    _ => unreachable!()
                };

                self.queue.push(QueueableToken::End { rule: rule, pos: pos.pos() });
            } else {
                self.queue.truncate(index);
            }
        }

        result
    }

    fn track(&mut self, rule: R, pos: usize, pos_attempts_index: usize, neg_attempts_index: usize) {
        if self.is_atomic {
            return;
        }

        // If nested rules made no progress, there is no use to report them; it's only useful to
        // track the current rule.
        if pos == self.attempt_pos {
            self.pos_attempts.truncate(pos_attempts_index);
            self.neg_attempts.truncate(neg_attempts_index);
        }

        let mut attempts = if self.lookahead != Lookahead::Negative {
            &mut self.pos_attempts
        } else {
            &mut self.neg_attempts
        };

        if pos > self.attempt_pos {
            attempts.clear();
            self.attempt_pos = pos;
        }

        if pos == self.attempt_pos {
            attempts.push(rule);
        }
    }

    /// Wrapper which removes `Tokens` in case of a sequence's failure.
    ///
    /// Usually used in conjunction with
    /// [`Position::sequence`](../inputs/struct.Position#method.sequence).
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("a".to_owned()));
    /// let pairs: Vec<_> = pest::state(input, |state, pos| {
    ///     state.sequence(move |state| {
    ///         pos.sequence(|p| {
    ///             state.rule(Rule::a, p, |_, p| Ok(p)).and_then(|p| {
    ///                 p.match_string("b")
    ///             })
    ///         })
    ///     }).or_else(|p| {
    ///         Ok(p)
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn sequence<F>(&mut self, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(&mut ParserState<R, I>) -> Result<Position<I>, Position<I>>
    {
        let index = self.queue.len();

        let result = f(self);

        if result.is_err() {
            self.queue.truncate(index);
        }

        result
    }


    /// Wrapper which stops `Token`s from being generated.
    ///
    /// Usually used in conjunction with
    /// [`Position::lookahead`](../inputs/struct.Position#method.lookahead).
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("a".to_owned()));
    /// let pairs: Vec<_> = pest::state(input, |state, pos| {
    ///     state.lookahead(true, move |state| {
    ///         state.rule(Rule::a, pos, |_, p| Ok(p))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn lookahead<F>(&mut self, is_positive: bool, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(&mut ParserState<R, I>) -> Result<Position<I>, Position<I>>
    {
        let initial_lookahead = self.lookahead;

        self.lookahead = if is_positive {
            Lookahead::Positive
        } else {
            Lookahead::Negative
        };

        let result = f(self);

        self.lookahead = initial_lookahead;

        result
    }

    /// Wrapper which stops `Token`s from being generated according to `is_atomic`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("a".to_owned()));
    /// let pairs: Vec<_> = pest::state(input, |state, pos| {
    ///     state.atomic(true, move |state| {
    ///         state.rule(Rule::a, pos, |_, p| Ok(p))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn atomic<F>(&mut self, is_atomic: bool, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(&mut ParserState<R, I>) -> Result<Position<I>, Position<I>>
    {
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

    /// Returns whether the `ParserState` is in an atomic state.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("a".to_owned()));
    /// let pairs: Vec<_> = pest::state::<(), _, _>(input, |state, pos| {
    ///     assert!(!state.is_atomic());
    ///
    ///     state.atomic(true, move |state| {
    ///         assert!(state.is_atomic());
    ///
    ///         Ok(pos)
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn is_atomic(&self) -> bool {
        self.is_atomic
    }
}
