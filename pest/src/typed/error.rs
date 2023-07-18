// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Types for Parsing failures.

use crate::{
    error::{Error, ErrorVariant},
    Position, RuleType,
};
use alloc::{borrow::ToOwned, format, vec, vec::Vec};

/// Error tracker.
pub enum Tracker<'i, R: RuleType> {
    /// Positive attempts, negative attempts, position.
    Attempts(Vec<R>, Vec<R>, Position<'i>),
    /// Peek slice out of bound.
    SliceOutOfBound(i32, Option<i32>, Position<'i>),
    /// Repeat too many times.
    RepeatTooManyTimes(Position<'i>),
    /// Accessing elements in empty stack, such as Drop or Pop.
    EmptyStack(Position<'i>),
}
impl<'i, R: RuleType> Tracker<'i, R> {
    /// Create an empty tracker for attempts.
    pub fn new(pos: Position<'i>) -> Self {
        Self::Attempts(vec![], vec![], pos)
    }
    /// Create a tracker with positive attempts.
    pub fn new_positive(positives: R, pos: Position<'i>) -> Self {
        Self::Attempts(vec![positives], vec![], pos)
    }
    /// Create a tracker with negative attempts.
    pub fn new_negative(negatives: R, pos: Position<'i>) -> Self {
        Self::Attempts(vec![], vec![negatives], pos)
    }
    /// Get position.
    pub fn position(self) -> Position<'i> {
        match self {
            Tracker::Attempts(_, _, pos) => pos,
            Tracker::SliceOutOfBound(_, _, pos) => pos,
            Tracker::RepeatTooManyTimes(pos) => pos,
            Tracker::EmptyStack(pos) => pos,
        }
    }
    /// Get position.
    pub fn ref_position(&self) -> &Position<'i> {
        match self {
            Tracker::Attempts(_, _, pos) => pos,
            Tracker::SliceOutOfBound(_, _, pos) => pos,
            Tracker::RepeatTooManyTimes(pos) => pos,
            Tracker::EmptyStack(pos) => pos,
        }
    }
    /// Handle attempts in nested rules.
    /// If the nested rule don't make progress, it will be ignored.
    /// See [`ParserState`](crate::parser_state::ParserState)
    pub fn nest(self, rule: R, pos: Position<'i>) -> Self {
        if self.ref_position() == &pos {
            Tracker::new_positive(rule, pos)
        } else {
            self
        }
    }
    /// Merge attempts in two tracker in different choices.
    /// Only further tracker will be stored if they don't point to the same position.
    /// `other` has higher priority if we encoutered unexpected errors.
    pub fn merge(mut self, mut other: Self) -> Self {
        match &mut other {
            Tracker::Attempts(_positive, _negative, _pos) => match &mut self {
                Tracker::Attempts(positive, negative, pos) => {
                    if pos == _pos {
                        _positive.append(positive);
                        _negative.append(negative);
                        other
                    } else if pos < _pos {
                        other
                    } else {
                        self
                    }
                }
                _ => self,
            },
            _ => other,
        }
    }
    /// Invert attempts.
    pub fn to_negative(self) -> Self {
        match self {
            Self::Attempts(p, n, pos) => Self::Attempts(n, p, pos),
            _ => self,
        }
    }
    /// Collect attempts to `pest::error::Error<R>`
    pub fn collect(self) -> Error<R> {
        match self {
            Self::Attempts(mut positives, mut negatives, pos) => {
                positives.sort();
                positives.dedup();
                negatives.sort();
                negatives.dedup();
                Error::new_from_pos(
                    ErrorVariant::ParsingError {
                        positives,
                        negatives,
                    },
                    pos,
                )
            }
            Self::SliceOutOfBound(start, end, pos) => Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: match end {
                        Some(end) => format!("Peek slice {}..{} out of bound.", start, end),
                        None => format!("Peek slice {}.. out of bound.", start),
                    },
                },
                pos,
            ),
            Self::RepeatTooManyTimes(pos) => Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Repeated too many times.".to_owned(),
                },
                pos,
            ),
            Self::EmptyStack(pos) => Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Nothing to pop or drop.".to_owned(),
                },
                pos,
            ),
        }
    }
}
