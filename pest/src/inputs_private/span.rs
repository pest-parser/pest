// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use super::input::Input;
use super::position;

/// A `struct` of a span over an `Input`. It is created from either
/// [two `Position`s](struct.Position.html#method.span) or from a
/// [`Pair`](../iterators/struct.Pair.html#method.span).
pub struct Span<I: Input> {
    input: Rc<I>,
    start: usize,
    end: usize
}

#[inline]
pub fn new<I: Input>(input: Rc<I>, start: usize, end: usize) -> Span<I> {
    Span {
        input: input,
        start: start,
        end: end
    }
}

impl<I: Input> Span<I> {
    /// Returns the `Span`'s start position.
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
    /// ```
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the `Span`'s end position.
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
    /// assert_eq!(span.end(), 2);
    /// ```
    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }

    /// Splits the `Span` into a pair of `Position`s.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.clone().span(end.clone());
    ///
    /// assert_eq!(span.split(), (start, end));
    /// ```
    #[inline]
    pub fn split(self) -> (position::Position<I>, position::Position<I>) {
        let pos1 = position::new(self.input.clone(), self.start);
        let pos2 = position::new(self.input, self.end);

        (pos1, pos2)
    }

    /// Captures a `&str` slice from the `Input` defined by the `Span`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("abc".to_owned()));
    /// let start = Position::from_start(input).skip(1).unwrap();
    /// let end = start.clone().match_string("b").unwrap();
    /// let span = start.span(end);
    ///
    /// assert_eq!(span.capture(), "b");
    /// ```
    #[inline]
    pub fn capture(&self) -> &str {
        unsafe { self.input.slice(self.start, self.end) }
    }
}

// We don't want to enforce derivable traits on the Input which forces to implement them manually.

impl<I: Input> fmt::Debug for Span<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span {{ start: {}, end: {} }}", self.start, self.end)
    }
}

impl<I: Input> Clone for Span<I> {
    fn clone(&self) -> Span<I> {
        new(self.input.clone(), self.start, self.end)
    }
}

impl<I: Input> PartialEq for Span<I> {
    fn eq(&self, other: &Span<I>) -> bool {
        self.input.ptr_eq(other.input) &&
        self.start == other.start &&
        self.end == other.end
    }
}

impl<I: Input> Eq for Span<I> {}

impl<'a, I: Input> Hash for Span<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.input as *const I).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}
