// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::rc::Rc;

use super::error::Error;
use super::inputs::{Input, Position, Span};
use super::inputs_private::position;
use super::iterators_private::{pairs, QueueableToken};
use super::RuleType;

#[derive(Clone, Copy, Eq, PartialEq)]
enum Lookahead {
    Positive,
    Negative,
    None
}

/// A `struct` which contains the complete state of a `Parser`.
pub struct ParserState<R: RuleType, I: Input> {
    queue: Vec<QueueableToken<R>>,
    lookahead: Lookahead,
    is_atomic: bool,
    pos_attempts: Vec<R>,
    neg_attempts: Vec<R>,
    attempt_pos: usize,
    /// Stack of `Span`s
    pub stack: Vec<Span<I>>
}


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
        Err(Error::ParsingError {
            positives: state.pos_attempts,
            negatives: state.neg_attempts,
            pos: position::new(input, state.attempt_pos)
        })
    }
}

impl<R: RuleType, I: Input> ParserState<R, I> {
    #[inline]
    pub fn rule<F>(&mut self, rule: R, pos: Position<I>, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(&mut ParserState<R, I>, Position<I>) -> Result<Position<I>, Position<I>>
    {
        let actual_pos = pos.pos();
        let index = self.queue.len();

        self.track(rule, actual_pos);

        if self.lookahead == Lookahead::None && !self.is_atomic {
            // Pair's position will only be known after running the closure.
            self.queue.push(QueueableToken::Start { pair: 0, pos: actual_pos });
        }

        let result = f(self, pos);

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

    #[inline]
    pub fn is_atomic(&self) -> bool {
        self.is_atomic
    }

    fn track(&mut self, rule: R, pos: usize) {
        if self.is_atomic {
            return;
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

        attempts.push(rule);
    }
}
