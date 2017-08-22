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

/// A `struct` useful for matching heap-allocated `String`s.
#[derive(Debug)]
pub struct StringInput {
    string: String
}

impl StringInput {
    /// Creates a new `StringInput` from a `String`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::inputs::{Input, StringInput};
    /// let input = StringInput::new("asd".to_owned());
    ///
    /// assert_eq!(input.len(), 3);
    /// ```
    pub fn new(string: String) -> StringInput {
        StringInput {
            string
        }
    }
}

impl Input for StringInput {
    #[inline]
    fn len(&self) -> usize {
        self.string.len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.string.is_empty()
    }

    #[inline]
    fn file_name(&self) -> Option<OsString> {
        None
    }

    #[inline]
    unsafe fn slice(&self, start: usize, end: usize) -> &str {
        self.string.slice_unchecked(start, end)
    }

    unsafe fn line_col(&self, pos: usize) -> (usize, usize) {
        if pos > self.string.len() {
            panic!("position out of bounds");
        }

        let mut pos = pos;
        let slice = &self.string[..pos];
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
                None => unreachable!(),
            }
        }

        line_col
    }

    unsafe fn line_of(&self, mut pos: usize) -> &str {
        if pos > self.string.len() {
            panic!("position out of bounds");
        }

        let start = if pos == 0 {
            0
        } else {
            if self.string.slice_unchecked(pos, pos + 1) == "\n" {
                pos -= 1;
            }

            let start = self.string.char_indices()
                                   .rev()
                                   .skip_while(|&(i, _)| i > pos)
                                   .find(|&(_, c)| c == '\n');
            match start {
                Some((i, _)) => i + 1,
                None => 0
            }
        };

        let end = if self.string.len() == 0 {
            0
        } else if pos == self.string.len() - 1 {
            let mut end = self.string.len();

            if end > 0 && self.string.slice_unchecked(end - 1, end) == "\n" {
                end -= 1;
            }
            if end > 0 && self.string.slice_unchecked(end - 1, end) == "\r" {
                end -= 1;
            }

            end
        } else {
            let end = self.string.char_indices()
                .skip_while(|&(i, _)| i < pos)
                .find(|&(_, c)| c == '\n');
            let mut end = match end {
                Some((i, _)) => i,
                None => self.string.len()
            };

            if end > 0 && self.string.slice_unchecked(end - 1, end) == "\r" {
                end -= 1;
            }

            end
        };

        self.string.slice_unchecked(start, end)
    }

    #[inline]
    unsafe fn skip(&self, n: usize, pos: usize) -> Option<usize> {
        let mut len = 0;
        let mut chars = self.string.slice_unchecked(pos, self.string.len()).chars();

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
    unsafe fn match_string(&self, string: &str, pos: usize) -> bool {
        let to = pos + string.len();

        if to <= self.string.len() {
            let slice = self.string.slice_unchecked(pos, to);
            slice == string
        } else {
            false
        }
    }

    #[inline]
    unsafe fn match_insensitive(&self, string: &str, pos: usize) -> bool {
        let slice = self.string.slice_unchecked(pos, self.string.len());

        if slice.is_char_boundary(string.len()) {
            let slice = slice.slice_unchecked(0, string.len());
            slice.eq_ignore_ascii_case(string)
        } else {
            false
        }
    }

    #[inline]
    unsafe fn match_range(&self, range: Range<char>, pos: usize) -> Option<usize> {
        let slice = self.string.slice_unchecked(pos, self.string.len());

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let input = StringInput::new("".to_owned());

        unsafe {
            assert!(input.is_empty());
            assert!(input.match_string("", 0));
            assert!(!input.match_string("a", 0));
        }
    }

    #[test]
    fn parts() {
        let input = StringInput::new("asdasdf".to_owned());

        unsafe {
            assert!(!input.is_empty());
            assert!(input.match_string("asd", 0));
            assert!(input.match_string("asdf", 3));
        }
    }

    #[test]
    fn len() {
        assert_eq!(StringInput::new("asdasdf".to_owned()).len(), 7);
    }

    #[test]
    fn slice() {
        let input = StringInput::new("asdasdf".to_owned());

        unsafe {
            assert_eq!(input.slice(1, 3), "sd");
        }
    }

    #[test]
    fn line_col() {
        let input = StringInput::new("a\rb\nc\r\nd嗨".to_owned());

        unsafe {
            assert_eq!(input.line_col(0), (1, 1));
            assert_eq!(input.line_col(1), (1, 2));
            assert_eq!(input.line_col(2), (1, 3));
            assert_eq!(input.line_col(3), (1, 4));
            assert_eq!(input.line_col(4), (2, 1));
            assert_eq!(input.line_col(5), (2, 2));
            assert_eq!(input.line_col(6), (2, 3));
            assert_eq!(input.line_col(7), (3, 1));
            assert_eq!(input.line_col(8), (3, 2));
            assert_eq!(input.line_col(11), (3, 3));
        }
    }

    #[test]
    fn line_of() {
        let input = StringInput::new("a\rb\nc\r\nd嗨".to_owned());

        unsafe {
            assert_eq!(input.line_of(0), "a\rb");
            assert_eq!(input.line_of(1), "a\rb");
            assert_eq!(input.line_of(2), "a\rb");
            assert_eq!(input.line_of(3), "a\rb");
            assert_eq!(input.line_of(4), "c");
            assert_eq!(input.line_of(5), "c");
            assert_eq!(input.line_of(6), "c");
            assert_eq!(input.line_of(7), "d嗨");
            assert_eq!(input.line_of(8), "d嗨");
            assert_eq!(input.line_of(11), "d嗨");
        }
    }

    #[test]
    fn line_of_empty() {
        let input = StringInput::new("".to_owned());

        unsafe {
            assert_eq!(input.line_of(0), "");
        }
    }

    #[test]
    fn line_of_new_line() {
        let input = StringInput::new("\n".to_owned());

        unsafe {
            assert_eq!(input.line_of(0), "");
        }
    }

    #[test]
    fn skip_empty() {
        let input = StringInput::new("".to_owned());

        unsafe {
            assert_eq!(input.skip(0, 0), Some(0));
            assert_eq!(input.skip(1, 0), None);
        }
    }

    #[test]
    fn skip() {
        let input = StringInput::new("d嗨".to_owned());

        unsafe {
            assert_eq!(input.skip(0, 0), Some(0));
            assert_eq!(input.skip(1, 0), Some(1));
            assert_eq!(input.skip(1, 1), Some(3));
        }
    }

    #[test]
    fn match_range() {
        let input = StringInput::new("b".to_owned());

        unsafe {
            assert!(input.match_range('a'..'c', 0).is_some());
            assert!(input.match_range('b'..'b', 0).is_some());
            assert!(input.match_range('a'..'a', 0).is_none());
            assert!(input.match_range('c'..'c', 0).is_none());
            assert!(input.match_range('a'..'嗨', 0).is_some());
        }
    }

    #[test]
    fn match_insensitive() {
        let input = StringInput::new("AsdASdF".to_owned());

        unsafe {
            assert!(input.match_insensitive("asd", 0));
            assert!(input.match_insensitive("asdf", 3));
        }
    }
}
