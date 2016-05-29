// pest. Smart PEGs in Rust
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
/// assert!(input.matches("asd"));
/// assert!(input.matches("asdf"));
/// assert!(!input.matches("nope"));
/// ```
pub struct StringInput {
    string: String,
    pos: usize
}

impl StringInput {
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
    pub fn new(string: &str) -> StringInput {
        StringInput {
            string: string.to_owned(),
            pos : 0
        }
    }
}

impl Input for StringInput {
    #[inline]
    fn len(&self) -> usize {
        self.string.len()
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
    fn slice(&self, start: usize, end: usize) -> &str {
        &self.string[start..end]
    }

    #[inline]
    fn matches(&mut self, string: &str) -> bool {
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
    fn between(&mut self, left: char, right: char) -> bool {
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

        assert!(input.matches(""));
        assert!(!input.matches("a"));
    }

    #[test]
    fn parts() {
        let mut input = StringInput::new("asdasdf");

        assert!(input.matches("asd"));
        assert!(input.matches("asdf"));
    }

    #[test]
    fn len() {
        assert_eq!(StringInput::new("asdasdf").len(), 7);
    }

    #[test]
    fn pos() {
        let mut input = StringInput::new("asdasdf");

        assert_eq!(input.pos(), 0);
        assert!(input.matches("asd"));
        assert_eq!(input.pos(), 3);
        assert!(input.matches("asdf"));
        assert_eq!(input.pos(), 7);

        input.set_pos(3);

        assert_eq!(input.pos(), 3);
        assert!(input.matches("asdf"));
        assert_eq!(input.pos(), 7);
    }

    #[test]
    fn between() {
        let mut input = StringInput::new("bbbb");

        assert!(input.between('a', 'c'));
        assert!(input.between('b', 'b'));
        assert!(!input.between('a', 'a'));
        assert!(!input.between('c', 'c'));

        assert_eq!(input.pos(), 2);
    }
}
