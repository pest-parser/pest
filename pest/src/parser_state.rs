// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::rc::Rc;

use RuleType;
use error::Error;
use iterators::{pairs, QueueableToken};
use position::{self, Position};
use span::Span;

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
    queue: Vec<QueueableToken<R>>,
    lookahead: Lookahead,
    pos_attempts: Vec<R>,
    neg_attempts: Vec<R>,
    attempt_pos: usize,
    /// Specifies current atomicity
    pub atomicity: Atomicity,
    /// Stack of `Span`s
    pub stack: Vec<Span<'i>>
}

/// Creates a `ParserState` from a `&str`, supplying it to a closure `f`.
///
/// # Examples
///
/// ```
/// # use std::rc::Rc;
/// # use pest;
///
/// let input = "";
/// pest::state::<(), _>(input, |_, pos| {
///     Ok(pos)
/// }).unwrap();
/// ```
pub fn state<'i, R: RuleType, F>(input: &'i str, f: F) -> Result<pairs::Pairs<'i, R>, Error<'i, R>>
where
    F: FnOnce(&mut ParserState<'i, R>, Position<'i>) -> Result<Position<'i>, Position<'i>>
{
    let mut state = ParserState {
        queue: vec![],
        lookahead: Lookahead::None,
        pos_attempts: vec![],
        neg_attempts: vec![],
        attempt_pos: 0,
        atomicity: Atomicity::NonAtomic,
        stack: vec![]
    };

    if f(&mut state, Position::from_start(input)).is_ok() {
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
            // All attempted positions were legal.
            pos: unsafe { position::new(input, state.attempt_pos) }
        })
    }
}

impl<'i, R: RuleType> ParserState<'i, R> {
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
    /// let pairs: Vec<_> = pest::state(input, |state, pos| {
    ///     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 1);
    /// ```
    #[inline]
    pub fn rule<F>(
        &mut self,
        rule: R,
        pos: Position<'i>,
        f: F
    ) -> Result<Position<'i>, Position<'i>>
    where
        F: FnOnce(&mut ParserState<'i, R>, Position<'i>) -> Result<Position<'i>, Position<'i>>
    {
        let actual_pos = pos.pos();
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

        let result = f(self, pos);

        if result.is_err() ^ (self.lookahead == Lookahead::Negative) {
            self.track(
                rule,
                actual_pos,
                pos_attempts_index,
                neg_attempts_index,
                attempts
            );
        }

        if self.lookahead == Lookahead::None && self.atomicity != Atomicity::Atomic {
            if let Ok(ref pos) = result {
                // Storing the pair's index in the first token that was added before the closure was
                // run.
                let new_index = self.queue.len();
                match self.queue[index] {
                    QueueableToken::Start { ref mut pair, .. } => *pair = new_index,
                    _ => unreachable!()
                };

                self.queue.push(QueueableToken::End {
                    rule,
                    pos: pos.pos()
                });
            } else {
                self.queue.truncate(index);
            }
        }

        result
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
    pub fn sequence<F>(&mut self, f: F) -> Result<Position<'i>, Position<'i>>
    where
        F: FnOnce(&mut ParserState<'i, R>) -> Result<Position<'i>, Position<'i>>
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
    /// let pairs: Vec<_> = pest::state(input, |state, pos| {
    ///     state.lookahead(true, move |state| {
    ///         state.rule(Rule::a, pos, |_, p| Ok(p))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn lookahead<F>(&mut self, is_positive: bool, f: F) -> Result<Position<'i>, Position<'i>>
    where
        F: FnOnce(&mut ParserState<'i, R>) -> Result<Position<'i>, Position<'i>>
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
    /// # use pest::{self, Atomicity};
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a";
    /// let pairs: Vec<_> = pest::state(input, |state, pos| {
    ///     state.atomic(Atomicity::Atomic, move |state| {
    ///         state.rule(Rule::a, pos, |_, p| Ok(p))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn atomic<F>(&mut self, atomicity: Atomicity, f: F) -> Result<Position<'i>, Position<'i>>
    where
        F: FnOnce(&mut ParserState<'i, R>) -> Result<Position<'i>, Position<'i>>
    {
        let initial_atomicity = self.atomicity;
        let should_toggle = self.atomicity != atomicity;

        if should_toggle {
            self.atomicity = atomicity;
        }

        let result = f(self);

        if should_toggle {
            self.atomicity = initial_atomicity;
        }

        result
    }
}
