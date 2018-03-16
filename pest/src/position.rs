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
use std::str;

use memchr;

use span;

/// A `struct` containing a position that is tied to a `&str` which provides useful methods to
/// manually parse it. This leads to an API largely based on the standard `Result`.
pub struct Position<'i> {
    input: &'i [u8],
    pos: usize
}

pub unsafe fn new(input: &[u8], pos: usize) -> Position {
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
        unsafe { new(input.as_bytes(), 0) }
    }

    /// Returns the current byte position as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let mut start = Position::from_start(input);
    ///
    /// assert_eq!(start.pos(), 0);
    ///
    /// start.match_string("ab");
    /// assert_eq!(start.pos(), 2);
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
    /// let mut end = start.clone();
    /// end.match_string("ab");
    /// let span = start.span(&end);
    ///
    /// assert_eq!(span.start(), 0);
    /// assert_eq!(span.end(), 2);
    /// ```
    #[inline]
    pub fn span(&self, other: &Position<'i>) -> span::Span<'i> {
        if ptr::eq(self.input, other.input) {
            // Position's pos is always a UTF-8 border.
            unsafe { span::new(self.input, self.pos, other.pos) }
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
    /// let mut pos = Position::from_start(input);
    /// pos.match_string("\na");
    ///
    /// assert_eq!(pos.line_col(), (2, 2));
    /// ```
    #[inline]
    pub fn line_col(&self) -> (usize, usize) {
        if self.pos > self.input.len() {
            panic!("position out of bounds");
        }

        let mut pos = self.pos;
        // Position's pos is always a UTF-8 border.
        let slice = unsafe { str::from_utf8_unchecked(&self.input[..pos]) };
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

    /// Returns the actual line of the input represented by the current `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "\na";
    /// let mut pos = Position::from_start(input);
    /// pos.match_string("\na");
    ///
    /// assert_eq!(pos.line_of(), "a");
    /// ```
    #[inline]
    pub fn line_of(&self) -> &str {
        if self.pos > self.input.len() {
            panic!("position out of bounds");
        }

        let start = if self.pos == 0 {
            0
        } else {
            // Position's pos is always a UTF-8 border.
            let start = unsafe { str::from_utf8_unchecked(self.input) }
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

            if end > 0 && self.input[end - 1] == b'\n' {
                end -= 1;
            }
            if end > 0 && self.input[end - 1] == b'\r' {
                end -= 1;
            }

            end
        } else {
            // Position's pos is always a UTF-8 border.
            let end = unsafe { str::from_utf8_unchecked(self.input) }
                .char_indices()
                .skip_while(|&(i, _)| i < self.pos)
                .find(|&(_, c)| c == '\n');
            let mut end = match end {
                Some((i, _)) => i,
                None => self.input.len()
            };

            if end > 0 && self.input[end - 1] == b'\r' {
                end -= 1;
            }

            end
        };

        // Safe since start and end can only be valid UTF-8 borders.
        unsafe { str::from_utf8_unchecked(&self.input[start..end]) }
    }

    /// Returns `true` when the `Position` is at the start of the input `&str`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let mut end = start.clone();
    /// end.match_string("ab");
    ///
    /// assert_eq!(start.clone().at_start(), true);
    /// assert_eq!(end.clone().at_start(), false);
    /// ```
    #[inline]
    pub fn at_start(&self) -> bool {
        self.pos == 0
    }

    /// Returns `true` when the `Position` is at the end of the input `&str`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    /// let mut end = start.clone();
    /// end.match_string("ab");
    ///
    /// assert_eq!(start.clone().at_end(), false);
    /// assert_eq!(end.clone().at_end(), true);
    /// ```
    #[inline]
    pub fn at_end(&self) -> bool {
        self.pos == self.input.len()
    }

    /// Skips `n` `char`s from the `Position` and returns `true` if the skip was possible or `false`
    /// otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().skip(2), true);
    /// assert_eq!(start.clone().skip(3), false);
    /// ```
    #[inline]
    pub fn skip(&mut self, n: usize) -> bool {
        let skipped = {
            let mut len = 0;
            // Position's pos is always a UTF-8 border.
            let mut chars = unsafe { str::from_utf8_unchecked(&self.input[self.pos..]) }.chars();

            for _ in 0..n {
                if let Some(c) = chars.next() {
                    len += c.len_utf8();
                } else {
                    return false;
                }
            }

            len
        };

        self.pos += skipped;
        true
    }

    /// Skips until the first occurrence of `string`. If the `string` can not be found, this
    /// function will return false.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().skip_until("a"), true);
    /// assert_eq!(start.clone().skip_until("b"), true);
    /// assert_eq!(start.clone().skip_until("c"), false);
    /// ```
    #[inline]
    pub fn skip_until(&mut self, string: &str) -> bool {
        let mut current = self.pos;
        let slice = string.as_bytes();

        while let Some(pos) = memchr::memchr(slice[0], &self.input[current..]) {
            if slice.len() == 1 {
                self.pos = current + pos;
                return true;
            } else {
                if slice == &self.input[current + pos..current + pos + slice.len()] {
                    self.pos = current + pos;
                    return true;
                } else {
                    current += pos + 1;
                }
            }
        }

        false
    }

    /// Matches `string` from the `Position` and returns `true` if a match was made or `false`
    /// otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_string("ab"), true);
    /// assert_eq!(start.clone().match_string("ac"), false);
    /// ```
    #[inline]
    pub fn match_string(&mut self, string: &str) -> bool {
        let matched = {
            let to = self.pos + string.len();

            if to <= self.input.len() {
                string.as_bytes() == &self.input[self.pos..to]
            } else {
                false
            }
        };

        if matched {
            self.pos += string.len();
            true
        } else {
            false
        }
    }

    /// Case-insensitively matches `string` from the `Position` and returns `true` if a match was
    /// made or `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_insensitive("AB"), true);
    /// assert_eq!(start.clone().match_insensitive("AC"), false);
    /// ```
    #[inline]
    pub fn match_insensitive(&mut self, string: &str) -> bool {
        let matched = {
            // Matching is safe since, even if the string does not fall on UTF-8 borders, that
            // particular slice is only used for comparison which will be handled correctly.
            let slice = unsafe { str::from_utf8_unchecked(&self.input[self.pos..]) };

            if slice.is_char_boundary(string.len()) {
                let slice = unsafe { slice.slice_unchecked(0, string.len()) };
                slice.eq_ignore_ascii_case(string)
            } else {
                false
            }
        };

        if matched {
            self.pos += string.len();
            true
        } else {
            false
        }
    }

    /// Matches `char` `range` from the `Position` and returns `true` if a match was made or `false`
    /// otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab";
    /// let start = Position::from_start(input);
    ///
    /// assert_eq!(start.clone().match_range('a'..'z'), true);
    /// assert_eq!(start.clone().match_range('A'..'Z'), false);
    /// ```
    #[inline]
    pub fn match_range(&mut self, range: Range<char>) -> bool {
        let len = {
            // Cannot actually cause undefined behavior.
            let slice = unsafe { str::from_utf8_unchecked(&self.input[self.pos..]) };

            if let Some(c) = slice.chars().next() {
                if range.start <= c && c <= range.end {
                    Some(c.len_utf8())
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
                true
            }
            None => false
        }
    }

//    / Starts a sequence of transformations provided by `f` from the `Position`. It returns the
//    / same `Result` returned by `f` in the case of an `Ok` or `Err` with the current `Position`
//    / otherwise.
//    /
//    / This method is useful to parse sequences that only match together which usually come in the
//    / form of chained `Result`s with
//    / [`Result::and_then`](https://doc.rust-lang.org/std/result/enum.Result.html#method.and_then).
//    / Such chains should always be wrapped up in
//    / [`ParserState::sequence`](../struct.ParserState.html#method.sequence) if they can create
//    / `Token`s before being wrapped in `Position::sequence`.
//    /
//    / # Examples
//    /
//    / ```
//    / # use pest::Position;
//    / let input = "ab";
//    / let start = Position::from_start(input);
//    /
//    / assert_eq!(
//    /     start.clone().sequence(|p| {
//    /         p.match_string("a").and_then(|p| {
//    /             p.match_string("b")
//    /         })
//    /     }).unwrap().pos(),
//    /     2
//    / );
//    / assert_eq!(
//    /     start.clone().sequence(|p| {
//    /         p.match_string("a").and_then(|p| {
//    /             p.match_string("c")
//    /         })
//    /     }),
//    /     Err(start)
//    / );
//    / ```
//    #[inline]
//    pub fn sequence<F>(self, f: F) -> Result<Position<'i>, Position<'i>>
//    where
//        F: FnOnce(Position<'i>) -> Result<Position<'i>, Position<'i>>
//    {
//        let initial_pos = self.pos;
//        let result = f(self);
//
//        match result {
//            Ok(pos) => Ok(pos),
//            Err(mut pos) => {
//                pos.pos = initial_pos;
//                Err(pos)
//            }
//        }
//    }

//    /// Starts a lookahead transformation provided by `f` from the `Position`. It returns `Ok` with
//    /// the current position if `f` also returns an `Ok ` or `Err` with the current `Position`
//    /// otherwise.
//    ///
//    /// If `is_positive` is `false`, it swaps the `Ok` and `Err` together, negating the `Result`. It
//    /// should always be wrapped up in
//    /// [`ParserState::lookahead`](../struct.ParserState.html#method.lookahead) if it can create
//    /// `Token`s before being wrapped in `Position::lookahead`.
//    ///
//    /// # Examples
//    ///
//    /// ```
//    /// # use pest::Position;
//    /// let input = "ab";
//    /// let start = Position::from_start(input);
//    ///
//    /// assert_eq!(
//    ///     start.clone().lookahead(true, |p| {
//    ///         p.match_string("ab")
//    ///     }),
//    ///     Ok(start.clone())
//    /// );
//    /// assert_eq!(
//    ///     start.clone().lookahead(true, |p| {
//    ///         p.match_string("ac")
//    ///     }),
//    ///     Err(start.clone())
//    /// );
//    /// assert_eq!(
//    ///     start.clone().lookahead(false, |p| {
//    ///         p.match_string("ac")
//    ///     }),
//    ///     Ok(start)
//    /// );
//    /// ```
//    #[inline]
//    pub fn lookahead<F>(self, is_positive: bool, f: F) -> Result<Position<'i>, Position<'i>>
//    where
//        F: FnOnce(Position<'i>) -> Result<Position<'i>, Position<'i>>
//    {
//        let initial_pos = self.pos;
//        let result = f(self);
//
//        let result = match result {
//            Ok(mut pos) => {
//                pos.pos = initial_pos;
//                Ok(pos)
//            }
//            Err(mut pos) => {
//                pos.pos = initial_pos;
//                Err(pos)
//            }
//        };
//
//        if is_positive {
//            result
//        } else {
//            match result {
//                Ok(pos) => Err(pos),
//                Err(pos) => Ok(pos)
//            }
//        }
//    }

//    /// Optionally applies the transformation provided by `f` from the `Position`. It returns `Ok`
//    /// with the `Position` returned by `f` regardless of the `Result`.
//    ///
//    /// # Examples
//    ///
//    /// ```
//    /// # use pest::Position;
//    /// let input = "ab";
//    /// let start = Position::from_start(input);
//    ///
//    /// assert_eq!(
//    ///     start.clone().optional(|p| {
//    ///         p.match_string("a").and_then(|p| {
//    ///             p.match_string("b")
//    ///         })
//    ///     }).unwrap().pos(),
//    ///     2
//    /// );
//    /// assert_eq!(
//    ///     start.clone().sequence(|p| {
//    ///         p.match_string("a").and_then(|p| {
//    ///             p.match_string("c")
//    ///         })
//    ///     }),
//    ///     Err(start)
//    /// );
//    /// ```
//    #[inline]
//    pub fn optional<F>(self, f: F) -> Result<Position<'i>, Position<'i>>
//    where
//        F: FnOnce(Position<'i>) -> Result<Position<'i>, Position<'i>>
//    {
//        let result = f(self);
//
//        match result {
//            Ok(pos) | Err(pos) => Ok(pos)
//        }
//    }

//    /// Repeatedly applies the transformation provided by `f` from the `Position`. It returns `Ok`
//    /// with the first `Position` returned by `f` which is wrapped up in an `Err`.
//    ///
//    /// # Examples
//    ///
//    /// ```
//    /// # use pest::Position;
//    /// let input = "ab";
//    /// let start = Position::from_start(input);
//    ///
//    /// assert_eq!(
//    ///     start.clone().repeat(|p| {
//    ///         p.match_string("a")
//    ///     }).unwrap().pos(),
//    ///     1
//    /// );
//    /// assert_eq!(
//    ///     start.repeat(|p| {
//    ///         p.match_string("b")
//    ///     }).unwrap().pos(),
//    ///     0
//    /// );
//    /// ```
//    #[inline]
//    pub fn repeat<F>(self, mut f: F) -> Result<Position<'i>, Position<'i>>
//    where
//        F: FnMut(Position<'i>) -> Result<Position<'i>, Position<'i>>
//    {
//        let mut result = f(self);
//
//        loop {
//            match result {
//                Ok(pos) => result = f(pos),
//                Err(pos) => return Ok(pos)
//            };
//        }
//    }
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
        (self.input as *const [u8]).hash(state);
        self.pos.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let input = b"";
        assert_eq!(unsafe { new(input, 0) }.match_string(""), true);
        assert_eq!(!unsafe { new(input, 0) }.match_string("a"), true);
    }

    #[test]
    fn parts() {
        let input = b"asdasdf";

        assert_eq!(unsafe { new(input, 0) }.match_string("asd"), true);
        assert_eq!(unsafe { new(input, 3) }.match_string("asdf"), true);
    }

    #[test]
    fn line_col() {
        let input = "a\rb\nc\r\nd嗨".as_bytes();

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
        let input = "a\rb\nc\r\nd嗨".as_bytes();

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
        let input = b"";

        assert_eq!(unsafe { new(input, 0) }.line_of(), "");
    }

    #[test]
    fn line_of_new_line() {
        let input = b"\n";

        assert_eq!(unsafe { new(input, 0) }.line_of(), "");
    }

    #[test]
    fn line_of_between_new_line() {
        let input = b"\n\n";

        assert_eq!(unsafe { new(input, 1) }.line_of(), "");
    }

    fn measure_skip(input: &[u8], pos: usize, n: usize) -> Option<usize> {
        let mut p = unsafe { new(input, pos) };
        if p.skip(n) {
            Some(p.pos - pos)
        } else {
            None
        }
    }

    #[test]
    fn skip_empty() {
        let input = b"";

        assert_eq!(measure_skip(input, 0, 0), Some(0));
        assert_eq!(measure_skip(input, 0, 1), None);
    }

    #[test]
    fn skip() {
        let input = "d嗨".as_bytes();

        assert_eq!(measure_skip(input, 0, 0), Some(0));
        assert_eq!(measure_skip(input, 0, 1), Some(1));
        assert_eq!(measure_skip(input, 1, 1), Some(3));
    }

    #[test]
    fn skip_until() {
        let input = "ab ac";
        let pos = Position::from_start(input);

        let mut test_pos = pos.clone();
        test_pos.skip_until("a");
        assert_eq!(test_pos.pos(), 0);

        test_pos = pos.clone();
        test_pos.skip_until("b");
        assert_eq!(test_pos.pos(), 1);

        test_pos = pos.clone();
        test_pos.skip_until("ab");
        assert_eq!(test_pos.pos(), 0);

        test_pos = pos.clone();
        test_pos.skip_until("ac");
        assert_eq!(test_pos.pos(), 3);
    }

    #[test]
    fn match_range() {
        let input = b"b";

        assert_eq!(unsafe { new(input, 0) }.match_range('a'..'c'), true);
        assert_eq!(unsafe { new(input, 0) }.match_range('b'..'b'), true);
        assert_eq!(!unsafe { new(input, 0) }.match_range('a'..'a'), true);
        assert_eq!(!unsafe { new(input, 0) }.match_range('c'..'c'), true);
        assert_eq!(unsafe { new(input, 0) }.match_range('a'..'嗨'), true);
    }

    #[test]
    fn match_insensitive() {
        let input = b"AsdASdF";

        assert_eq!(unsafe { new(input, 0) }.match_insensitive("asd"), true);
        assert_eq!(unsafe { new(input, 3) }.match_insensitive("asdf"), true);
    }
}
