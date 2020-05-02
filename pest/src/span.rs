// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::ptr;
use std::str;

use position;

/// A span over a `&str`. It is created from either [two `Position`s] or from a [`Pair`].
///
/// [two `Position`s]: struct.Position.html#method.span
/// [`Pair`]: ../iterators/struct.Pair.html#method.span
#[derive(Clone)]
pub struct Span<'i> {
    input: &'i str,
    /// # Safety
    ///
    /// Must be a valid character boundary index into `input`.
    start: usize,
    /// # Safety
    ///
    /// Must be a valid character boundary index into `input`.
    end: usize,
}

impl<'i> Span<'i> {
    /// Create a new `Span` without checking invariants. (Checked with `debug_assertions`.)
    ///
    /// # Safety
    ///
    /// `input[start..end]` must be a valid subslice; that is, said indexing should not panic.
    pub(crate) unsafe fn new_unchecked(input: &str, start: usize, end: usize) -> Span {
        debug_assert!(input.get(start..end).is_some());
        Span { input, start, end }
    }

    /// Attempts to create a new span. Will return `None` if `input[start..end]` is an invalid index
    /// into `input`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Span;
    /// let input = "Hello!";
    /// assert_eq!(None, Span::new(input, 100, 0));
    /// assert!(Span::new(input, 0, input.len()).is_some());
    /// ```
    #[allow(clippy::new_ret_no_self)]
    pub fn new(input: &str, start: usize, end: usize) -> Option<Span> {
        if input.get(start..end).is_some() {
            Some(Span { input, start, end })
        } else {
            None
        }
    }

    /// Attempts to create a new span based on a sub-range.
    ///
    /// TODO better docs
    pub fn sub_span(&self, range: impl std::ops::RangeBounds<usize>) -> Option<Span<'i>> {
        let start = match range.start_bound() {
            std::ops::Bound::Included(&offset) => offset,
            std::ops::Bound::Excluded(&offset) => offset + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            std::ops::Bound::Included(&offset) => Some(offset + 1),
            std::ops::Bound::Excluded(&offset) => Some(offset),
            std::ops::Bound::Unbounded => None,
        };
        let s = self.as_str();
        if s.get(start..end.unwrap_or(s.len())).is_some() {
            Span::new(
                self.input,
                self.start + start,
                end.map(|n| self.start + n).unwrap_or(self.end),
            )
        } else {
            None
        }
    }

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
        unsafe { position::Position::new_unchecked(self.input, self.start) }
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
        unsafe { position::Position::new_unchecked(self.input, self.end) }
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
        let pos1 = unsafe { position::Position::new_unchecked(self.input, self.start) };
        let pos2 = unsafe { position::Position::new_unchecked(self.input, self.end) };

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
        &self.input[self.start..self.end]
    }

    /// Iterates over all lines (partially) covered by this span.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "a\nb\nc";
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input).skip(2).unwrap();
    /// let start_pos = state.position().clone();
    /// state = state.match_string("b\nc").unwrap();
    /// let span = start_pos.span(&state.position().clone());
    /// assert_eq!(span.lines().collect::<Vec<_>>(), vec!["b\n", "c"]);
    /// ```
    #[inline]
    pub fn lines(&self) -> Lines<'i> {
        Lines {
            inner: self.lines_span(),
        }
    }

    /// TODO better docs
    #[inline]
    pub fn lines_span(&self) -> LinesSpan<'i> {
        LinesSpan {
            span: self.clone(),
            pos: self.start,
        }
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

impl<'i> PartialEq for Span<'i> {
    fn eq(&self, other: &Span<'i>) -> bool {
        ptr::eq(self.input, other.input) && self.start == other.start && self.end == other.end
    }
}

impl<'i> Eq for Span<'i> {}

impl<'i> Hash for Span<'i> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.input as *const str).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}

/// Line iterator for Spans, created by [`Span::lines()`].
///
/// Iterates all lines that are at least partially covered by the span.
///
/// [`Span::lines()`]: struct.Span.html#method.lines
pub struct Lines<'i> {
    inner: LinesSpan<'i>,
}

impl<'i> Iterator for Lines<'i> {
    type Item = &'i str;
    fn next(&mut self) -> Option<&'i str> {
        self.inner.next().map(|span| span.start_pos().line_of())
    }
}

/// Like `Lines`, but returns `Span`s to preserve source mapping information. `Lines` simply calls this and `span.start_pos().line_of()`.
///
/// TODO better docs
pub struct LinesSpan<'i> {
    span: Span<'i>,
    pos: usize,
}

impl<'i> Iterator for LinesSpan<'i> {
    type Item = Span<'i>;
    fn next(&mut self) -> Option<Span<'i>> {
        if self.pos > self.span.end {
            return None;
        }
        let pos = position::Position::new(self.span.input, self.pos)?;
        if pos.at_end() {
            return None;
        }
        let line_start = self.pos;
        self.pos = pos.find_line_end();
        Some(unsafe { Span::new_unchecked(self.span.input, line_start, self.pos) })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sub_span() {
        let input = "abcde";
        let span = Span::new(input, 0, input.len()).unwrap();
        assert_eq!(span.sub_span(..).unwrap().as_str(), "abcde");
        assert_eq!(span.sub_span(..3).unwrap().as_str(), "abc");
        assert_eq!(span.sub_span(..=3).unwrap().as_str(), "abcd");
        assert_eq!(span.sub_span(1..).unwrap().as_str(), "bcde");
        assert_eq!(span.sub_span(1..3).unwrap().as_str(), "bc");
        assert_eq!(span.sub_span(1..=3).unwrap().as_str(), "bcd");
    }

    #[test]
    fn sub_span_out_of_range() {
        let input = "abc";
        let span = Span::new(input, 0, 1).unwrap();
        assert_eq!(span.sub_span(..).unwrap().as_str(), "a");

        // Even though `input` has this character, a sub span cannot be created.
        assert_eq!(span.sub_span(..2), None);
    }

    #[test]
    fn sub_span_preserve_input() {
        let input = "abc";
        let span = Span::new(input, 1, input.len()).unwrap();

        // The user can no longer access `span.input` to see the leading `a`.
        assert_eq!(span.as_str(), "bc");

        // If the user wants to process a portion of `span`, they can do so preserving the original `span.input`.
        let sub_span = span.sub_span(1..2).unwrap();
        assert_eq!(sub_span.as_str(), "c");
        assert_eq!(sub_span.start_pos().line_of(), "abc");
    }

    #[test]
    fn split() {
        let input = "a";
        let start = position::Position::from_start(input);
        let mut end = start.clone();

        assert!(end.skip(1));

        let span = start.clone().span(&end.clone());

        assert_eq!(span.split(), (start, end));
    }

    #[test]
    fn lines_span_mid() {
        let input = "abc\ndef\nghi";
        let span = Span::new(input, 1, 7).unwrap();
        let lines: Vec<_> = span.lines_span().collect();
        println!("{:?}", lines);
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].as_str(), "bc\n".to_owned()); // `Span::lines_span` preserves the "partial" coverage of the span.
        assert_eq!(lines[1].as_str(), "def\n".to_owned());
    }

    #[test]
    fn lines_mid() {
        let input = "abc\ndef\nghi";
        let span = Span::new(input, 1, 7).unwrap();
        let lines: Vec<_> = span.lines().collect();
        println!("{:?}", lines);
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "abc\n".to_owned());
        assert_eq!(lines[1], "def\n".to_owned());
    }

    #[test]
    fn lines_eof() {
        let input = "abc\ndef\nghi";
        let span = Span::new(input, 5, 11).unwrap();
        assert!(span.end_pos().at_end());
        let lines: Vec<_> = span.lines().collect();
        println!("{:?}", lines);
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "def\n".to_owned());
        assert_eq!(lines[1], "ghi".to_owned());
    }
}
