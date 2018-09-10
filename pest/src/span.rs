// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ptr;
use std::str;

use position;

/// A `struct` of a span over a `&str`. It is created from either
/// [two `Position`s](struct.Position.html#method.span) or from a
/// [`Pair`](../iterators/struct.Pair.html#method.span).
pub struct Span<'i> {
    input: &'i [u8],
    start: usize,
    end: usize
}

#[inline]
pub unsafe fn new(input: &[u8], start: usize, end: usize) -> Span {
    Span { input, start, end }
}

impl<'i> Span<'i> {
    /// Returns the `Span`'s start byte position as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone();
    /// let span = start.span(&end);
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
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone();
    /// let span = start.span(&end);
    ///
    /// assert_eq!(span.end(), 0);
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
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone();
    /// let span = start.clone().span(&end);
    ///
    /// assert_eq!(span.start_pos(), start);
    /// ```
    #[inline]
    pub fn start_pos(&self) -> position::Position<'i> {
        // Span's start position is always a UTF-8 border.
        unsafe { position::new(self.input, self.start) }
    }

    /// Returns the `Span`'s end `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone();
    /// let span = start.span(&end);
    ///
    /// assert_eq!(span.end_pos(), end);
    /// ```
    #[inline]
    pub fn end_pos(&self) -> position::Position<'i> {
        // Span's end position is always a UTF-8 border.
        unsafe { position::new(self.input, self.end) }
    }

    /// Splits the `Span` into a pair of `Position`s.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone();
    /// let span = start.clone().span(&end);
    ///
    /// assert_eq!(span.split(), (start, end));
    /// ```
    #[inline]
    pub fn split(self) -> (position::Position<'i>, position::Position<'i>) {
        // Span's start and end positions are always a UTF-8 borders.
        let pos1 = unsafe { position::new(self.input, self.start) };
        let pos2 = unsafe { position::new(self.input, self.end) };

        (pos1, pos2)
    }

    /// Captures a slice from the `&str` defined by the `Span`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "abc";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input).skip(1).unwrap();
    /// let start_pos = state.position().clone();
    /// state = state.match_string("b").unwrap();
    /// let span = start_pos.span(&state.position().clone());
    /// assert_eq!(span.as_str(), "b");
    /// ```
    #[inline]
    pub fn as_str(&self) -> &'i str {
        // Span's start and end positions are always a UTF-8 borders.
        unsafe { str::from_utf8_unchecked(&self.input[self.start..self.end]) }
    }
}

impl<'i> fmt::Debug for Span<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Span")
            .field("str", &self.as_str())
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

impl<'i> Clone for Span<'i> {
    fn clone(&self) -> Span<'i> {
        unsafe { new(self.input, self.start, self.end) }
    }
}

impl<'i> PartialEq for Span<'i> {
    fn eq(&self, other: &Span<'i>) -> bool {
        ptr::eq(self.input, other.input) && self.start == other.start && self.end == other.end
    }
}

impl<'i> Eq for Span<'i> {}

impl<'i> PartialOrd for Span<'i> {
    fn partial_cmp(&self, other: &Span<'i>) -> Option<Ordering> {
        if ptr::eq(self.input, other.input) {
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
        (self.input as *const [u8]).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split() {
        let input = "a";
        let start = position::Position::from_start(input);
        let mut end = start.clone();

        assert!(end.skip(1));

        let span = start.clone().span(&end.clone());

        assert_eq!(span.split(), (start, end));
    }
}
