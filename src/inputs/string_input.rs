// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::str;

use super::super::Input;

/// A `struct` useful for matching in-memory `String`s.
///
/// # Examples
///
/// ```
/// # use pest::Input;
/// # use pest::StringInput;
/// let mut input = StringInput::new("asdasdf");
///
/// assert!(input.match_string("asd"));
/// assert!(input.match_string("asdf"));
/// assert!(!input.match_string("nope"));
/// ```
pub struct StringInput<'a> {
    string: &'a str,
    pos: usize,
}

impl<'a> StringInput<'a> {
    /// Creates a new `StringInput` from a `&str`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Input;
    /// # use pest::StringInput;
    /// let mut input = StringInput::new("asd");
    ///
    /// assert_eq!(input.len(), 3);
    /// ```
    pub fn new(string: &'a str) -> StringInput<'a> {
        StringInput {
            string: string,
            pos: 0,
        }
    }
}

impl<'a> Input<'a> for StringInput<'a> {
    #[inline]
    fn len(&self) -> usize {
        self.string.len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.string.is_empty()
    }

    #[inline]
    fn pos(&self) -> usize {
        self.pos
    }

    #[inline]
    fn set_pos(&mut self, pos: usize) {
        self.pos = pos
    }

    #[inline]
    fn slice(&self, start: usize, end: usize) -> &'a str {
        &self.string[start..end]
    }

    #[inline]
    fn line_col(&self, pos: usize) -> (usize, usize) {
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
                            line_col = (line_col.0 + 1, 1);
                        } else {
                            pos -= 2;
                            line_col = (line_col.0 + 1, 1);
                        }
                    } else {
                        pos -= 1;
                        line_col = (line_col.0 + 1, 1);
                    }
                }
                Some('\n') => {
                    pos -= 1;
                    line_col = (line_col.0 + 1, 1);
                },
                Some(c)    => {
                    pos -= c.len_utf8();
                    line_col = (line_col.0, line_col.1 + 1);
                },
                None       => unreachable!(),
            }
        }

        line_col
    }

    #[inline]
    fn match_string(&mut self, string: &str) -> bool {
        let to = self.pos + string.len();

        if to <= self.string.len() {
            let slice = unsafe { self.string.slice_unchecked(self.pos, to) };
            let result = slice == string;

            if result {
                self.pos = to;
            }

            result
        } else {
            false
        }
    }

    #[inline]
    fn match_range(&mut self, left: char, right: char) -> bool {
        let len = left.len_utf8();

        if len != right.len_utf8() {
            panic!("ranges should have same-sized UTF-8 limits");
        }

        let to = self.pos + len;

        if to <= self.string.len() {
            if let Ok(string) = str::from_utf8(&self.string.as_bytes()[self.pos..to]) {
                let c = string.chars().next().unwrap();

                let result = left <= c && c <= right;

                if result {
                    self.pos += len;
                }

                result
            } else {
                false
            }
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::Input;
    use super::StringInput;

    #[test]
    fn empty() {
        let mut input = StringInput::new("");

        assert!(input.is_empty());
        assert!(input.match_string(""));
        assert!(!input.match_string("a"));
    }

    #[test]
    fn parts() {
        let mut input = StringInput::new("asdasdf");

        assert!(!input.is_empty());
        assert!(input.match_string("asd"));
        assert!(input.match_string("asdf"));
    }

    #[test]
    fn len() {
        assert_eq!(StringInput::new("asdasdf").len(), 7);
    }

    #[test]
    fn pos() {
        let mut input = StringInput::new("asdasdf");

        assert_eq!(input.pos(), 0);
        assert!(input.match_string("asd"));
        assert_eq!(input.pos(), 3);
        assert!(input.match_string("asdf"));
        assert_eq!(input.pos(), 7);

        input.set_pos(3);

        assert_eq!(input.pos(), 3);
        assert!(input.match_string("asdf"));
        assert_eq!(input.pos(), 7);
    }

    #[test]
    fn slice() {
        let input = StringInput::new("asdasdf");

        assert_eq!(input.slice(1, 3), "sd");
    }

    #[test]
    fn line_col() {
        let input = StringInput::new("a\rb\nc\r\nd嗨");

        assert_eq!(input.line_col(0), (1, 1));
        assert_eq!(input.line_col(1), (1, 2));
        assert_eq!(input.line_col(2), (2, 1));
        assert_eq!(input.line_col(3), (2, 2));
        assert_eq!(input.line_col(4), (3, 1));
        assert_eq!(input.line_col(5), (3, 2));
        assert_eq!(input.line_col(6), (4, 1));
        assert_eq!(input.line_col(7), (4, 1));
        assert_eq!(input.line_col(8), (4, 2));
        assert_eq!(input.line_col(11), (4, 3));
    }

    #[test]
    fn match_range() {
        let mut input = StringInput::new("bbbb");

        assert!(input.match_range('a', 'c'));
        assert!(input.match_range('b', 'b'));
        assert!(!input.match_range('a', 'a'));
        assert!(!input.match_range('c', 'c'));

        assert_eq!(input.pos(), 2);
    }
}
