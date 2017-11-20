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

use super::str_input::StrInput;
use super::position;

/// A `struct` of a span over an `Input`. It is created from either
/// [two `Position`s](struct.Position.html#method.span) or from a
/// [`Pair`](../iterators/struct.Pair.html#method.span).
pub struct Span<'i> {
    input: Rc<StrInput<'i>>,
    start: usize,
    end: usize,
    __phantom: ::std::marker::PhantomData<&'i str>,
}

#[inline]
pub fn new<'i>(input: Rc<StrInput<'i>>, start: usize, end: usize) -> Span<'i> {
    Span { input, start, end, __phantom: ::std::marker::PhantomData }
}

impl<'i> Span<'i> {
    /// Returns the `Span`'s start byte position as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StrInput};
    /// let input = Rc::new(StrInput::new("ab"));
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

    /// Returns the `Span`'s end byte position as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StrInput};
    /// let input = Rc::new(StrInput::new("ab"));
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
    /// # use pest::inputs::{Position, StrInput};
    /// let input = Rc::new(StrInput::new("ab"));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.clone().span(end);
    ///
    /// assert_eq!(span.start_pos(), start);
    /// ```
    #[inline]
    pub fn start_pos(&self) -> position::Position<'i> {
        // Span start position is a UTF-8 border and is safe.
        unsafe { position::new(self.input.clone(), self.start) }
    }

    /// Returns the `Span`'s end `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StrInput};
    /// let input = Rc::new(StrInput::new("ab"));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.span(end.clone());
    ///
    /// assert_eq!(span.end_pos(), end);
    /// ```
    #[inline]
    pub fn end_pos(&self) -> position::Position<'i> {
        // Span end position is a UTF-8 border and is safe.
        unsafe { position::new(self.input.clone(), self.end) }
    }

    /// Splits the `Span` into a pair of `Position`s.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::inputs::{Position, StrInput};
    /// let input = Rc::new(StrInput::new("ab"));
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.clone().span(end.clone());
    ///
    /// assert_eq!(span.split(), (start, end));
    /// ```
    #[inline]
    pub fn split(self) -> (position::Position<'i>, position::Position<'i>) {
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
    /// # use pest::inputs::{Position, StrInput};
    /// let input = Rc::new(StrInput::new("abc"));
    /// let start = Position::from_start(input).skip(1).unwrap();
    /// let end = start.clone().match_string("b").unwrap();
    /// let span = start.span(end);
    ///
    /// assert_eq!(span.as_str(), "b");
    /// ```
    #[inline]
    pub fn as_str(&self) -> &'i str {
        unsafe { self.input.slice(self.start, self.end) }
    }
}

// We don't want to enforce derivable traits on the Input which forces to implement them manually.

impl<'i> fmt::Debug for Span<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span {{ start: {}, end: {} }}", self.start, self.end)
    }
}

impl<'i> Clone for Span<'i> {
    fn clone(&self) -> Span<'i> {
        new(self.input.clone(), self.start, self.end)
    }
}

impl<'i> PartialEq for Span<'i> {
    fn eq(&self, other: &Span<'i>) -> bool {
        Rc::ptr_eq(&self.input, &other.input) && self.start == other.start && self.end == other.end
    }
}

impl<'i> Eq for Span<'i> {}

impl<'i> PartialOrd for Span<'i> {
    fn partial_cmp(&self, other: &Span<'i>) -> Option<Ordering> {
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

impl<'i> Ord for Span<'i> {
    fn cmp(&self, other: &Span<'i>) -> Ordering {
        self.partial_cmp(other).expect(
            "cannot compare spans from \
             different inputs"
        )
    }
}

impl<'i> Hash for Span<'i> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.input as *const StrInput<'i>).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}
