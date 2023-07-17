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
    SliceOutOfBound(i32, Option<i32>, Position<'i>),
    RepeatTooManyTimes(Position<'i>),
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
    /// Merge attempts in two tracker if points to the same position.
    /// `other` has higher priority.
    pub fn merge(self, mut other: Self) -> Self {
        match &mut other {
            Tracker::Attempts(_positive, _negative, _pos) => match self {
                Tracker::Attempts(positive, negative, pos) => {
                    if pos == *_pos {
                        _positive.extend(positive.into_iter());
                        _negative.extend(negative.into_iter());
                    }
                    other
                }
                _ => other,
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
