// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::ffi::OsString;
use std::ops::Range;
use std::str;

use super::Input;

/// A `struct` useful for matching borrowed `str`s.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StrInput<'a> {
    str_ref: &'a str
}

impl<'a> StrInput<'a> {
    /// Creates a new `StrInput` from a `&str`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::inputs::{Input, StrInput};
    /// let input = StrInput::new("asd");
    ///
    /// assert_eq!(input.len(), 3);
    /// ```
    pub fn new(source: &'a str) -> StrInput<'a> {
        StrInput { str_ref: source }
    }
}

#[inline]
unsafe fn line_col(source: &str, pos: usize) -> (usize, usize) {
    if pos > source.len() {
        panic!("position out of bounds");
    }

    let mut pos = pos;
    let slice = &source[..pos];
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

#[inline]
unsafe fn line_of(source: &str, pos: usize) -> &str {
    if pos > source.len() {
        panic!("position out of bounds");
    }

    let start = if pos == 0 {
        0
    } else {
        let start = source.char_indices()
                          .rev()
                          .skip_while(|&(i, _)| i >= pos)
                          .find(|&(_, c)| c == '\n');
        match start {
            Some((i, _)) => i + 1,
            None => 0
        }
    };

    let end = if source.len() == 0 {
        0
    } else if pos == source.len() - 1 {
        let mut end = source.len();

        if end > 0 && source.slice_unchecked(end - 1, end) == "\n" {
            end -= 1;
        }
        if end > 0 && source.slice_unchecked(end - 1, end) == "\r" {
            end -= 1;
        }

        end
    } else {
        let end = source.char_indices()
                        .skip_while(|&(i, _)| i < pos)
                        .find(|&(_, c)| c == '\n');
        let mut end = match end {
            Some((i, _)) => i,
            None => source.len()
        };

        if end > 0 && source.slice_unchecked(end - 1, end) == "\r" {
            end -= 1;
        }

        end
    };

    source.slice_unchecked(start, end)
}

#[inline]
unsafe fn skip(source: &str, n: usize, pos: usize) -> Option<usize> {
    let mut len = 0;
    let mut chars = source.slice_unchecked(pos, source.len()).chars();

    for _ in 0..n {
        if let Some(c) = chars.next() {
            len += c.len_utf8();
        } else {
            return None;
        }
    }

    Some(len)
}

#[inline]
unsafe fn match_string(source: &str, string: &str, pos: usize) -> bool {
    let to = pos + string.len();

    if to <= source.len() {
        let slice = source.slice_unchecked(pos, to);
        slice == string
    } else {
        false
    }
}

#[inline]
unsafe fn match_insensitive(source: &str, string: &str, pos: usize) -> bool {
    let slice = source.slice_unchecked(pos, source.len());

    if slice.is_char_boundary(string.len()) {
        let slice = slice.slice_unchecked(0, string.len());
        slice.eq_ignore_ascii_case(string)
    } else {
        false
    }
}

#[inline]
unsafe fn match_range(source: &str, range: Range<char>, pos: usize) -> Option<usize> {
    let slice = source.slice_unchecked(pos, source.len());

    if let Some(char) = slice.chars().next() {
        if range.start <= char && char <= range.end {
            Some(char.len_utf8())
        } else {
            None
        }
    } else {
        None
    }
}

impl<'a> Input<'a> for StrInput<'a> {
    #[inline]
    fn len(&self) -> usize {
        self.str_ref.len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.str_ref.is_empty()
    }

    #[inline]
    fn file_name(&self) -> Option<OsString> {
        None
    }

    #[inline]
    unsafe fn slice(&self, start: usize, end: usize) -> &'a str {
        self.str_ref.slice_unchecked(start, end)
    }

    unsafe fn line_col(&self, pos: usize) -> (usize, usize) {
        line_col(self.str_ref, pos)
    }

    unsafe fn line_of(&self, pos: usize) -> &'a str {
        line_of(self.str_ref, pos)
    }

    #[inline]
    unsafe fn skip(&self, n: usize, pos: usize) -> Option<usize> {
        skip(self.str_ref, n, pos)
    }

    #[inline]
    unsafe fn match_string(&self, string: &str, pos: usize) -> bool {
        match_string(self.str_ref, string, pos)
    }

    #[inline]
    unsafe fn match_insensitive(&self, string: &str, pos: usize) -> bool {
        match_insensitive(self.str_ref, string, pos)
    }

    #[inline]
    unsafe fn match_range(&self, range: Range<char>, pos: usize) -> Option<usize> {
        match_range(self.str_ref, range, pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let input2 = StrInput::new("");

        unsafe {
            assert!(input2.is_empty());
            assert!(input2.match_string("", 0));
            assert!(!input2.match_string("a", 0));
        }
    }

    #[test]
    fn parts() {
        let input2 = StrInput::new("asdasdf");

        unsafe {
            assert!(!input2.is_empty());
            assert!(input2.match_string("asd", 0));
            assert!(input2.match_string("asdf", 3));
        }
    }

    #[test]
    fn len() {
        assert_eq!(StrInput::new("asdasdf").len(), 7);
    }

    #[test]
    fn slice() {
        let input2 = StrInput::new("asdasdf");

        unsafe {
            assert_eq!(input2.slice(1, 3), "sd");
        }
    }

    #[test]
    fn line_col() {
        let input2 = StrInput::new("a\rb\nc\r\nd嗨");

        unsafe {
            assert_eq!(input2.line_col(0), (1, 1));
            assert_eq!(input2.line_col(1), (1, 2));
            assert_eq!(input2.line_col(2), (1, 3));
            assert_eq!(input2.line_col(3), (1, 4));
            assert_eq!(input2.line_col(4), (2, 1));
            assert_eq!(input2.line_col(5), (2, 2));
            assert_eq!(input2.line_col(6), (2, 3));
            assert_eq!(input2.line_col(7), (3, 1));
            assert_eq!(input2.line_col(8), (3, 2));
            assert_eq!(input2.line_col(11), (3, 3));
        }
    }

    #[test]
    fn line_of() {
        let input2 = StrInput::new("a\rb\nc\r\nd嗨");

        unsafe {
            assert_eq!(input2.line_of(0), "a\rb");
            assert_eq!(input2.line_of(1), "a\rb");
            assert_eq!(input2.line_of(2), "a\rb");
            assert_eq!(input2.line_of(3), "a\rb");
            assert_eq!(input2.line_of(4), "c");
            assert_eq!(input2.line_of(5), "c");
            assert_eq!(input2.line_of(6), "c");
            assert_eq!(input2.line_of(7), "d嗨");
            assert_eq!(input2.line_of(8), "d嗨");
            assert_eq!(input2.line_of(11), "d嗨");
        }
    }

    #[test]
    fn line_of_empty() {
        let input2 = StrInput::new("");

        unsafe {
            assert_eq!(input2.line_of(0), "");
        }
    }

    #[test]
    fn line_of_new_line() {
        let input2 = StrInput::new("\n");

        unsafe {
            assert_eq!(input2.line_of(0), "");
        }
    }

    #[test]
    fn line_of_between_new_line() {
        let input2 = StrInput::new("\n\n");

        unsafe {
            assert_eq!(input2.line_of(1), "");
        }
    }

    #[test]
    fn skip_empty() {
        let input2 = StrInput::new("");

        unsafe {
            assert_eq!(input2.skip(0, 0), Some(0));
            assert_eq!(input2.skip(1, 0), None);
        }
    }

    #[test]
    fn skip() {
        let input2 = StrInput::new("d嗨");

        unsafe {
            assert_eq!(input2.skip(0, 0), Some(0));
            assert_eq!(input2.skip(1, 0), Some(1));
            assert_eq!(input2.skip(1, 1), Some(3));
        }
    }

    #[test]
    fn match_range() {
        let input2 = StrInput::new("b");

        unsafe {
            assert!(input2.match_range('a'..'c', 0).is_some());
            assert!(input2.match_range('b'..'b', 0).is_some());
            assert!(input2.match_range('a'..'a', 0).is_none());
            assert!(input2.match_range('c'..'c', 0).is_none());
            assert!(input2.match_range('a'..'嗨', 0).is_some());
        }
    }

    #[test]
    fn match_insensitive() {
        let input2 = StrInput::new("AsdASdF");

        unsafe {
            assert!(input2.match_insensitive("asd", 0));
            assert!(input2.match_insensitive("asdf", 3));
        }
    }
}
