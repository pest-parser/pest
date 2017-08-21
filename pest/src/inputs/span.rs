// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cmp::Ordering;
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
        input,
        start,
        end
    }
}

impl<I: Input> Span<I> {
    /// Returns the `Span`'s start position as a `usize`.
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

    /// Returns the `Span`'s end position as a `usize`.
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

    /// Returns the `Span`'s start `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.clone().span(end);
    ///
    /// assert_eq!(span.start_pos(), start);
    /// ```
    #[inline]
    pub fn start_pos(&self) -> position::Position<I> {
        // Span start position is a UTF-8 border and is safe.
        unsafe { position::new(self.input.clone(), self.start) }
    }

    /// Returns the `Span`'s end `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StringInput};
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.span(end.clone());
    ///
    /// assert_eq!(span.end_pos(), end);
    /// ```
    #[inline]
    pub fn end_pos(&self) -> position::Position<I> {
        // Span end position is a UTF-8 border and is safe.
        unsafe { position::new(self.input.clone(), self.end) }
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
        // Span start and end positions are UTF-8 borders and safe.
        let pos1 = unsafe { position::new(self.input.clone(), self.start) };
        let pos2 = unsafe { position::new(self.input, self.end) };

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
    /// assert_eq!(span.as_str(), "b");
    /// ```
    #[inline]
    pub fn as_str(&self) -> &str {
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
        Rc::ptr_eq(&self.input, &other.input) &&
        self.start == other.start &&
        self.end == other.end
    }
}

impl<I: Input> Eq for Span<I> {}

impl<I: Input> PartialOrd for Span<I> {
    fn partial_cmp(&self, other: &Span<I>) -> Option<Ordering> {
        if Rc::ptr_eq(&self.input, &other.input) {
            match self.start.partial_cmp(&other.start) {
                Some(Ordering::Equal) => self.end.partial_cmp(&other.end),
                ordering => ordering
            }
        } else {
            None
        }
    }
}

impl<I: Input> Ord for Span<I> {
    fn cmp(&self, other: &Span<I>) -> Ordering {
        self.partial_cmp(other).expect("cannot compare spans from different inputs")
    }
}

impl<'a, I: Input> Hash for Span<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.input as *const I).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}
