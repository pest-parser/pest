// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use super::input::Input;
use super::span;

/// A `struct` containing a position that is tied to an `Input` which provides useful methods to
/// manually parse it. This leads to an API largely based on the standard `Result`.
pub struct Position<I: Input> {
    input: Rc<I>,
    pos: usize
}

pub fn new<I: Input>(input: Rc<I>, pos: usize) -> Position<I> {
    Position {
        input: input,
        pos: pos
    }
}

impl<I: Input> Position<I> {
    /// Creates starting `Position` from an `Rc<Input>`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("".to_owned()));
    ///
    /// Position::from_start(input);
    /// ```
    #[inline]
    pub fn from_start(input: Rc<I>) -> Position<I> {
        Position {
            input: input,
            pos: 0
        }
    }

    /// Returns the current position as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.pos(), 0);
    /// assert_eq!(start.match_string("ab").unwrap().pos(), 2);
    /// ```
    #[inline]
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Creates a `Span` from two `Position`s.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.span(end);
    ///
    /// assert_eq!(span.start(), 0);
    /// assert_eq!(span.end(), 2);
    /// ```
    #[inline]
    pub fn span(self, other: Position<I>) -> span::Span<I> {
        if &*self.input as *const I == &*other.input as *const I {
            span::new(self.input, self.pos, other.pos)
        } else {
            panic!("Span created from positions from different inputs")
        }
    }

    /// Returns the line - and column number pair of the current `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("\na".to_owned()));
    /// let start = Position::from_start(input);
    /// let pos = start.match_string("\na").unwrap();
    ///
    /// assert_eq!(pos.line_col(), (2, 2));
    /// ```
    #[inline]
    pub fn line_col(&self) -> (usize, usize) {
        unsafe { self.input.line_col(self.pos) }
    }

    /// Returns the actual line of the current `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("\na".to_owned()));
    /// let start = Position::from_start(input);
    /// let pos = start.match_string("\na").unwrap();
    ///
    /// assert_eq!(pos.line_of(), "a");
    /// ```
    #[inline]
    pub fn line_of(&self) -> &str {
        unsafe { self.input.line_of(self.pos) }
    }

    /// Returns `Ok` with the current `Position` if it is at the start of its `Input` or `Err` of
    /// the same `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    ///
    /// assert_eq!(start.clone().at_start(), Ok(start));
    /// assert_eq!(end.clone().at_start(), Err(end));
    /// ```
    #[inline]
    pub fn at_start(self) -> Result<Position<I>, Position<I>> {
        if self.pos == 0 {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Returns `Ok` with the current `Position` if it is at the end of its `Input` or `Err` of the
    /// same `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    ///
    /// assert_eq!(start.clone().at_end(), Err(start));
    /// assert_eq!(end.clone().at_end(), Ok(end));
    /// ```
    #[inline]
    pub fn at_end(self) -> Result<Position<I>, Position<I>> {
        if self.pos == self.input.len() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Skips `n` `char`s from the `Position` and returns `Ok` with the new `Position` if the skip
    /// was possible or `Err` with the current `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().skip(2).unwrap().pos(), 2);
    /// assert_eq!(start.clone().skip(3), Err(start));
    /// ```
    #[inline]
    pub fn skip(self, n: usize) -> Result<Position<I>, Position<I>> {
        let skipped = unsafe { self.input.skip(n, self.pos) };

        match skipped {
            Some(len) => Ok(new(self.input, self.pos + len)),
            None => Err(self)
        }
    }

    /// Matches `string` from the `Position` and returns `Ok` with the new `Position` if a match was
    /// made or `Err` with the current `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_string("ab").unwrap().pos(), 2);
    /// assert_eq!(start.clone().match_string("ac"), Err(start));
    /// ```
    #[inline]
    pub fn match_string(self, string: &str) -> Result<Position<I>, Position<I>> {
        // Matching is safe since, even if the string does not fall on UTF-8 borders, that
        // particular slice is only used for comparison which will be handled correctly.
        if unsafe { self.input.match_string(string, self.pos) } {
            Ok(new(self.input, self.pos + string.len()))
        } else {
            Err(self)
        }
    }

    /// Case-insensitively matches `string` from the `Position` and returns `Ok` with the new
    /// `Position` if a match was made or `Err` with the current `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_insensitive("AB").unwrap().pos(), 2);
    /// assert_eq!(start.clone().match_insensitive("AC"), Err(start));
    /// ```
    #[inline]
    pub fn match_insensitive(self, string: &str) -> Result<Position<I>, Position<I>> {
        // Matching is safe since, even if the string does not fall on UTF-8 borders, that
        // particular slice is only used for comparison which will be handled correctly.
        if unsafe { self.input.match_insensitive(string, self.pos) } {
            Ok(new(self.input, self.pos + string.len()))
        } else {
            Err(self)
        }
    }

    /// Matches `char` `range` from the `Position` and returns `Ok` with the new `Position` if a
    /// match was made or `Err` with the current `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_range('a'..'z').unwrap().pos(), 1);
    /// assert_eq!(start.clone().match_range('A'..'Z'), Err(start));
    /// ```
    #[inline]
    pub fn match_range(self, range: Range<char>) -> Result<Position<I>, Position<I>> {
        let len = unsafe { self.input.match_range(range, self.pos) };

        match len {
            Some(len) => Ok(new(self.input, self.pos + len)),
            None => Err(self)
        }
    }

    /// Starts a sequence of transformations provided by `f` from the `Position`. It returns the
    /// same `Result` returned by `f` in the case of an `Ok` or `Err` with the current `Position`
    /// otherwise.
    ///
    /// This method is useful to parse sequences that only match together which usually come in the
    /// form of chained `Result`s with
    /// [`Result::and_then`](https://doc.rust-lang.org/std/result/enum.Result.html#method.and_then).
    /// Such chains should always be wrapped up in
    /// [`ParserState::sequence`](../struct.ParserState.html#method.sequence) if they can create
    /// `Token`s before being wrapped in `Position::sequence`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(
    ///     start.clone().sequence(|p| {
    ///         p.match_string("a").and_then(|p| {
    ///             p.match_string("b")
    ///         })
    ///     }).unwrap().pos(),
    ///     2
    /// );
    /// assert_eq!(
    ///     start.clone().sequence(|p| {
    ///         p.match_string("a").and_then(|p| {
    ///             p.match_string("c")
    ///         })
    ///     }),
    ///     Err(start)
    /// );
    /// ```
    #[inline]
    pub fn sequence<F>(self, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(Position<I>) -> Result<Position<I>, Position<I>>
    {
        let initial_pos = self.pos;
        let result = f(self);

        match result {
            Ok(pos) => Ok(pos),
            Err(mut pos) => {
                pos.pos = initial_pos;
                Err(pos)
            }
        }
    }

    /// Starts a lookahead transformation provided by `f` from the `Position`. It returns `Ok` with
    /// the current position if `f` also returns an `Ok ` or `Err` with the current `Position`
    /// otherwise.
    ///
    /// If `is_positive` is `false`, it swaps the `Ok` and `Err` together, negating the `Result`. It
    /// should always be wrapped up in
    /// [`ParserState::lookahead`](../struct.ParserState.html#method.lookahead) if it can create
    /// `Token`s before being wrapped in `Position::lookahead`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(
    ///     start.clone().lookahead(true, |p| {
    ///         p.match_string("ab")
    ///     }),
    ///     Ok(start.clone())
    /// );
    /// assert_eq!(
    ///     start.clone().lookahead(true, |p| {
    ///         p.match_string("ac")
    ///     }),
    ///     Err(start.clone())
    /// );
    /// assert_eq!(
    ///     start.clone().lookahead(false, |p| {
    ///         p.match_string("ac")
    ///     }),
    ///     Ok(start)
    /// );
    /// ```
    #[inline]
    pub fn lookahead<F>(self, is_positive: bool, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(Position<I>) -> Result<Position<I>, Position<I>>
    {
        let initial_pos = self.pos;
        let result = f(self);

        let result = match result {
            Ok(mut pos) => {
                pos.pos = initial_pos;
                Ok(pos)
            }
            Err(mut pos) => {
                pos.pos = initial_pos;
                Err(pos)
            }
        };

        if is_positive {
            result
        } else {
            match result {
                Ok(pos) => Err(pos),
                Err(pos) => Ok(pos)
            }
        }
    }

    /// Optionally applies the transformation provided by `f` from the `Position`. It returns `Ok`
    /// with the `Position` returned by `f` regardless of the `Result`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(
    ///     start.clone().optional(|p| {
    ///         p.match_string("a").and_then(|p| {
    ///             p.match_string("b")
    ///         })
    ///     }).unwrap().pos(),
    ///     2
    /// );
    /// assert_eq!(
    ///     start.clone().sequence(|p| {
    ///         p.match_string("a").and_then(|p| {
    ///             p.match_string("c")
    ///         })
    ///     }),
    ///     Err(start)
    /// );
    /// ```
    #[inline]
    pub fn optional<F>(self, f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnOnce(Position<I>) -> Result<Position<I>, Position<I>>
    {
        let result = f(self);

        match result {
            Ok(pos) => Ok(pos),
            Err(pos) => Ok(pos)
        }
    }

    /// Repeatedly applies the transformation provided by `f` from the `Position`. It returns `Ok`
    /// with the first `Position` returned by `f` which is wrapped up in an `Err`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(
    ///     start.clone().repeat(|p| {
    ///         p.match_string("a")
    ///     }).unwrap().pos(),
    ///     1
    /// );
    /// assert_eq!(
    ///     start.repeat(|p| {
    ///         p.match_string("b")
    ///     }).unwrap().pos(),
    ///     0
    /// );
    /// ```
    #[inline]
    pub fn repeat<F>(self, mut f: F) -> Result<Position<I>, Position<I>>
    where
        F: FnMut(Position<I>) -> Result<Position<I>, Position<I>>
    {
        let mut result = f(self);

        while let Ok(pos) = result {
            result = f(pos);
        }

        match result {
            Err(pos) => Ok(pos),
            _ => unreachable!()
        }
    }
}

// We don't want to enforce derivable traits on the Input which forces to implement them manually.

impl<I: Input> fmt::Debug for Position<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Position {{ pos: {} }}", self.pos)
    }
}

impl<I: Input> Clone for Position<I> {
    fn clone(&self) -> Position<I> {
        new(self.input.clone(), self.pos)
    }
}

impl<I: Input> PartialEq for Position<I> {
    fn eq(&self, other: &Position<I>) -> bool {
        self.input.ptr_eq(other.input) && self.pos == other.pos
    }
}

impl<I: Input> Eq for Position<I> {}

impl<I: Input> PartialOrd for Position<I> {
    fn partial_cmp(&self, other: &Position<I>) -> Option<Ordering> {
        if self.input.ptr_eq(other.input) {
            self.pos.partial_cmp(&other.pos)
        } else {
            None
        }
    }
}

impl<I: Input> Ord for Position<I> {
    fn cmp(&self, other: &Position<I>) -> Ordering {
        self.partial_cmp(other).expect("cannot compare positions from different inputs")
    }
}

impl<I: Input> Hash for Position<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.input as *const I).hash(state);
        self.pos.hash(state);
    }
}
