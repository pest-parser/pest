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
use std::ops::Range;
use std::ptr;

use span;

/// A `struct` containing a position that is tied to a `&str` which provides useful methods to
/// manually parse it. This leads to an API largely based on the standard `Result`.
pub struct Position<'i> {
    input: &'i str,
    pos: usize
}

pub unsafe fn new(input: &str, pos: usize) -> Position {
    Position { input, pos }
}

impl<'i> Position<'i> {
    /// Creates starting `Position` from an `&str`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    ///
    /// Position::from_start("");
    /// ```
    #[inline]
    pub fn from_start(input: &'i str) -> Position<'i> {
        // Position 0 is always safe because it's always a valid UTF-8 border.
        unsafe { new(input, 0) }
    }

    /// Returns the current byte position as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
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
    /// # Panics
    ///
    /// Panics when the positions come from different inputs.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    /// let span = start.span(&end);
    ///
    /// assert_eq!(span.start(), 0);
    /// assert_eq!(span.end(), 2);
    /// ```
    #[inline]
    pub fn span(&self, other: &Position<'i>) -> span::Span<'i> {
        if ptr::eq(self.input, other.input) {
            span::new(self.input, self.pos, other.pos)
        } else {
            panic!("span created from positions from different inputs")
        }
    }

    /// Returns the line - and column number pair of the current `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "\na";
    /// let start = Position::from_start(input);
    /// let pos = start.match_string("\na").unwrap();
    ///
    /// assert_eq!(pos.line_col(), (2, 2));
    /// ```
    #[inline]
    pub fn line_col(&self) -> (usize, usize) {
        if self.pos > self.input.len() {
            panic!("position out of bounds");
        }

        let mut pos = self.pos;
        let slice = &self.input[..pos];
        let mut chars = slice.chars().peekable();

        let mut line_col = (1, 1);

        while pos != 0 {
            match chars.next() {
                Some('\r') => {
                    if let Some(&'\n') = chars.peek() {
                        chars.next();

                        if pos == 1 {
                            pos -= 1;
                        } else {
                            pos -= 2;
                        }

                        line_col = (line_col.0 + 1, 1);
                    } else {
                        pos -= 1;
                        line_col = (line_col.0, line_col.1 + 1);
                    }
                }
                Some('\n') => {
                    pos -= 1;
                    line_col = (line_col.0 + 1, 1);
                }
                Some(c) => {
                    pos -= c.len_utf8();
                    line_col = (line_col.0, line_col.1 + 1);
                }
                None => unreachable!()
            }
        }

        line_col
    }

    /// Returns the actual line of the current `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "\na";
    /// let start = Position::from_start(input);
    /// let pos = start.match_string("\na").unwrap();
    ///
    /// assert_eq!(pos.line_of(), "a");
    /// ```
    #[inline]
    pub fn line_of(&self) -> &str {
        if self.pos > self.input.len() {
            panic!("position out of bounds");
        }

        unsafe {
            let start = if self.pos == 0 {
                0
            } else {
                let start = self.input
                    .char_indices()
                    .rev()
                    .skip_while(|&(i, _)| i >= self.pos)
                    .find(|&(_, c)| c == '\n');
                match start {
                    Some((i, _)) => i + 1,
                    None => 0
                }
            };

            let end = if self.input.is_empty() {
                0
            } else if self.pos == self.input.len() - 1 {
                let mut end = self.input.len();

                if end > 0 && self.input.slice_unchecked(end - 1, end) == "\n" {
                    end -= 1;
                }
                if end > 0 && self.input.slice_unchecked(end - 1, end) == "\r" {
                    end -= 1;
                }

                end
            } else {
                let end = self.input
                    .char_indices()
                    .skip_while(|&(i, _)| i < self.pos)
                    .find(|&(_, c)| c == '\n');
                let mut end = match end {
                    Some((i, _)) => i,
                    None => self.input.len()
                };

                if end > 0 && self.input.slice_unchecked(end - 1, end) == "\r" {
                    end -= 1;
                }

                end
            };

            self.input.slice_unchecked(start, end)
        }
    }

    /// Returns `Ok` with the current `Position` if it is at the start of its `&str` or `Err` of
    /// the same `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    ///
    /// assert_eq!(start.clone().at_start(), Ok(start));
    /// assert_eq!(end.clone().at_start(), Err(end));
    /// ```
    #[inline]
    pub fn at_start(self) -> Result<Position<'i>, Position<'i>> {
        if self.pos == 0 {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Returns `Ok` with the current `Position` if it is at the end of its `&str` or `Err` of the
    /// same `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let end = start.clone().match_string("ab").unwrap();
    ///
    /// assert_eq!(start.clone().at_end(), Err(start));
    /// assert_eq!(end.clone().at_end(), Ok(end));
    /// ```
    #[inline]
    pub fn at_end(self) -> Result<Position<'i>, Position<'i>> {
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
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().skip(2).unwrap().pos(), 2);
    /// assert_eq!(start.clone().skip(3), Err(start));
    /// ```
    #[inline]
    pub fn skip(mut self, n: usize) -> Result<Position<'i>, Position<'i>> {
        let skipped = unsafe {
            let mut len = 0;
            let mut chars = self.input
                .slice_unchecked(self.pos, self.input.len())
                .chars();

            for _ in 0..n {
                if let Some(c) = chars.next() {
                    len += c.len_utf8();
                } else {
                    return Err(self);
                }
            }

            len
        };

        self.pos += skipped;
        Ok(self)
    }

    /// Matches `string` from the `Position` and returns `Ok` with the new `Position` if a match was
    /// made or `Err` with the current `Position` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_string("ab").unwrap().pos(), 2);
    /// assert_eq!(start.clone().match_string("ac"), Err(start));
    /// ```
    #[inline]
    pub fn match_string(mut self, string: &'i str) -> Result<Position<'i>, Position<'i>> {
        // Matching is safe since, even if the string does not fall on UTF-8 borders, that
        // particular slice is only used for comparison which will be handled correctly.
        let matched = unsafe {
            let to = self.pos + string.len();

            if to <= self.input.len() {
                let slice = self.input.slice_unchecked(self.pos, to);
                slice == string
            } else {
                false
            }
        };

        if matched {
            self.pos += string.len();
            Ok(self)
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
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_insensitive("AB").unwrap().pos(), 2);
    /// assert_eq!(start.clone().match_insensitive("AC"), Err(start));
    /// ```
    #[inline]
    pub fn match_insensitive(mut self, string: &'i str) -> Result<Position<'i>, Position<'i>> {
        // Matching is safe since, even if the string does not fall on UTF-8 borders, that
        // particular slice is only used for comparison which will be handled correctly.

        let matched = unsafe {
            let slice = self.input.slice_unchecked(self.pos, self.input.len());

            if slice.is_char_boundary(string.len()) {
                let slice = slice.slice_unchecked(0, string.len());
                slice.eq_ignore_ascii_case(string)
            } else {
                false
            }
        };

        if matched {
            self.pos += string.len();
            Ok(self)
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
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_range('a'..'z').unwrap().pos(), 1);
    /// assert_eq!(start.clone().match_range('A'..'Z'), Err(start));
    /// ```
    #[inline]
    pub fn match_range(mut self, range: Range<char>) -> Result<Position<'i>, Position<'i>> {
        // Cannot actually cause undefined behavior.
        let len = unsafe {
            let slice = self.input.slice_unchecked(self.pos, self.input.len());

            if let Some(char) = slice.chars().next() {
                if range.start <= char && char <= range.end {
                    Some(char.len_utf8())
                } else {
                    None
                }
            } else {
                None
            }
        };

        match len {
            Some(len) => {
                self.pos += len;
                Ok(self)
            }
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
    /// # use pest::Position;
    /// let input = "ab";
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
    pub fn sequence<F>(self, f: F) -> Result<Position<'i>, Position<'i>>
    where
        F: FnOnce(Position<'i>) -> Result<Position<'i>, Position<'i>>
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
    /// # use pest::Position;
    /// let input = "ab";
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
    pub fn lookahead<F>(self, is_positive: bool, f: F) -> Result<Position<'i>, Position<'i>>
    where
        F: FnOnce(Position<'i>) -> Result<Position<'i>, Position<'i>>
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
    /// # use pest::Position;
    /// let input = "ab";
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
    pub fn optional<F>(self, f: F) -> Result<Position<'i>, Position<'i>>
    where
        F: FnOnce(Position<'i>) -> Result<Position<'i>, Position<'i>>
    {
        let result = f(self);

        match result {
            Ok(pos) | Err(pos) => Ok(pos)
        }
    }

    /// Repeatedly applies the transformation provided by `f` from the `Position`. It returns `Ok`
    /// with the first `Position` returned by `f` which is wrapped up in an `Err`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
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
    pub fn repeat<F>(self, mut f: F) -> Result<Position<'i>, Position<'i>>
    where
        F: FnMut(Position<'i>) -> Result<Position<'i>, Position<'i>>
    {
        let mut result = f(self);

        loop {
            match result {
                Ok(pos) => result = f(pos),
                Err(pos) => return Ok(pos)
            };
        }
    }
}

impl<'i> fmt::Debug for Position<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Position {{ pos: {} }}", self.pos)
    }
}

impl<'i> Clone for Position<'i> {
    fn clone(&self) -> Position<'i> {
        // Cloning a safe position is safe.
        unsafe { new(self.input, self.pos) }
    }
}

impl<'i> PartialEq for Position<'i> {
    fn eq(&self, other: &Position<'i>) -> bool {
        ptr::eq(self.input, other.input) && self.pos == other.pos
    }
}

impl<'i> Eq for Position<'i> {}

impl<'i> PartialOrd for Position<'i> {
    fn partial_cmp(&self, other: &Position<'i>) -> Option<Ordering> {
        if ptr::eq(self.input, other.input) {
            self.pos.partial_cmp(&other.pos)
        } else {
            None
        }
    }
}

impl<'i> Ord for Position<'i> {
    fn cmp(&self, other: &Position<'i>) -> Ordering {
        self.partial_cmp(other)
            .expect("cannot compare positions from different strs")
    }
}

impl<'i> Hash for Position<'i> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.input as *const str).hash(state);
        self.pos.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let input = "";
        assert!(unsafe { new(input, 0) }.match_string("").is_ok());
        assert!(!unsafe { new(input, 0) }.match_string("a").is_ok());
    }

    #[test]
    fn parts() {
        let input = "asdasdf";

        assert!(unsafe { new(input, 0) }.match_string("asd").is_ok());
        assert!(unsafe { new(input, 3) }.match_string("asdf").is_ok());
    }

    #[test]
    fn line_col() {
        let input = "a\rb\nc\r\nd嗨";

        assert_eq!(unsafe { new(input, 0) }.line_col(), (1, 1));
        assert_eq!(unsafe { new(input, 1) }.line_col(), (1, 2));
        assert_eq!(unsafe { new(input, 2) }.line_col(), (1, 3));
        assert_eq!(unsafe { new(input, 3) }.line_col(), (1, 4));
        assert_eq!(unsafe { new(input, 4) }.line_col(), (2, 1));
        assert_eq!(unsafe { new(input, 5) }.line_col(), (2, 2));
        assert_eq!(unsafe { new(input, 6) }.line_col(), (2, 3));
        assert_eq!(unsafe { new(input, 7) }.line_col(), (3, 1));
        assert_eq!(unsafe { new(input, 8) }.line_col(), (3, 2));
        assert_eq!(unsafe { new(input, 11) }.line_col(), (3, 3));
    }

    #[test]
    fn line_of() {
        let input = "a\rb\nc\r\nd嗨";

        assert_eq!(unsafe { new(input, 0) }.line_of(), "a\rb");
        assert_eq!(unsafe { new(input, 1) }.line_of(), "a\rb");
        assert_eq!(unsafe { new(input, 2) }.line_of(), "a\rb");
        assert_eq!(unsafe { new(input, 3) }.line_of(), "a\rb");
        assert_eq!(unsafe { new(input, 4) }.line_of(), "c");
        assert_eq!(unsafe { new(input, 5) }.line_of(), "c");
        assert_eq!(unsafe { new(input, 6) }.line_of(), "c");
        assert_eq!(unsafe { new(input, 7) }.line_of(), "d嗨");
        assert_eq!(unsafe { new(input, 8) }.line_of(), "d嗨");
        assert_eq!(unsafe { new(input, 11) }.line_of(), "d嗨");
    }

    #[test]
    fn line_of_empty() {
        let input = "";

        assert_eq!(unsafe { new(input, 0) }.line_of(), "");
    }

    #[test]
    fn line_of_new_line() {
        let input = "\n";

        assert_eq!(unsafe { new(input, 0) }.line_of(), "");
    }

    #[test]
    fn line_of_between_new_line() {
        let input = "\n\n";

        assert_eq!(unsafe { new(input, 1) }.line_of(), "");
    }

    fn measure_skip<'i>(input: &'i str, pos: usize, n: usize) -> Option<usize> {
        let p = unsafe { new(input, pos) };
        if let Ok(p) = p.skip(n) {
            Some(p.pos - pos)
        } else {
            None
        }
    }

    #[test]
    fn skip_empty() {
        let input = "";

        assert_eq!(measure_skip(input, 0, 0), Some(0));
        assert_eq!(measure_skip(input, 0, 1), None);
    }

    #[test]
    fn skip() {
        let input = "d嗨";

        assert_eq!(measure_skip(input, 0, 0), Some(0));
        assert_eq!(measure_skip(input, 0, 1), Some(1));
        assert_eq!(measure_skip(input, 1, 1), Some(3));
    }

    #[test]
    fn match_range() {
        let input = "b";

        assert!(unsafe { new(input, 0) }.match_range('a'..'c').is_ok());
        assert!(unsafe { new(input, 0) }.match_range('b'..'b').is_ok());
        assert!(!unsafe { new(input, 0) }.match_range('a'..'a').is_ok());
        assert!(!unsafe { new(input, 0) }.match_range('c'..'c').is_ok());
        assert!(unsafe { new(input, 0) }.match_range('a'..'嗨').is_ok());
    }

    #[test]
    fn match_insensitive() {
        let input = "AsdASdF";

        assert!(unsafe { new(input, 0) }.match_insensitive("asd").is_ok());
        assert!(unsafe { new(input, 3) }.match_insensitive("asdf").is_ok());
    }
}
