use std::ascii::AsciiExt;
use std::str;

use super::Input;

/// A `struct` useful for matching in-memory `String`s.
///
/// # Examples
///
/// ```
/// # use pest::{Input, StringInput};
/// let input = StringInput::new("asdasdf");
///
/// assert!(input.match_string("asd", 0));
/// assert!(input.match_string("asd", 3));
/// assert!(!input.match_string("asd", 4));
/// ```
pub struct StringInput<'a> {
    string: &'a str
}

impl<'a> StringInput<'a> {
    /// Creates a new `StringInput` from a `&str`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::{Input, StringInput};
    /// let input = StringInput::new("asd");
    ///
    /// assert_eq!(input.len(), 3);
    /// ```
    pub fn new(string: &str) -> StringInput {
        StringInput {
            string: string
        }
    }
}

impl<'a> Input for StringInput<'a> {
    #[inline]
    fn len(&self) -> usize {
        self.string.len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.string.is_empty()
    }

    #[inline]
    fn slice(&self, start: usize, end: usize) -> &str {
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
                        line_col = (line_col.0, line_col.1 + 1);
                    }
                }
                Some('\n') => {
                    pos -= 1;
                    line_col = (line_col.0 + 1, 1);
                },
                Some(c) => {
                    pos -= c.len_utf8();
                    line_col = (line_col.0, line_col.1 + 1);
                },
                None => unreachable!(),
            }
        }

        line_col
    }

    #[inline]
    fn line_of(&self, mut pos: usize) -> &str {
        if pos > self.string.len() {
            panic!("position out of bounds");
        }

        if unsafe { self.string.slice_unchecked(pos, pos + 1) == "\n" } {
            pos -= 1;
        }

        let start = self.string.char_indices()
                               .rev()
                               .skip_while(|&(i, _)| i > pos)
                               .find(|&(_, c)| c == '\n');
        let start = match start {
            Some((i, _)) => i + 1,
            None         => 0
        };

        let end = self.string.char_indices()
                             .skip_while(|&(i, _)| i < pos)
                             .find(|&(_, c)| c == '\n');
        let mut end = match end {
            Some((i, _)) => i,
            None         => self.string.len()
        };
        if end > 0 && unsafe { self.string.slice_unchecked(end - 1, end) == "\r" } {
            end -= 1;
        }

        unsafe { self.string.slice_unchecked(start, end) }
    }

    #[inline]
    fn match_string(&self, string: &str, pos: usize) -> bool {
        let to = pos + string.len();

        if to <= self.string.len() {
            let slice = unsafe { self.string.slice_unchecked(pos, to) };
            slice == string
        } else {
            false
        }
    }

    #[inline]
    fn match_insensitive(&self, string: &str, pos: usize) -> bool {
        let slice = unsafe { self.string.slice_unchecked(pos, self.string.len()) };

        if slice.is_char_boundary(string.len()) {
            let slice = unsafe { slice.slice_unchecked(0, string.len()) };
            slice.eq_ignore_ascii_case(string)
        } else {
            false
        }
    }

    #[inline]
    fn match_range(&self, left: char, right: char, pos: usize) -> bool {
        let len = left.len_utf8();

        if len != right.len_utf8() {
            panic!("ranges should have same-sized UTF-8 limits");
        }

        let to = pos + len;

        if to <= self.string.len() {
            if let Ok(string) = str::from_utf8(&self.string.as_bytes()[pos..to]) {
                let c = string.chars().next().unwrap();

                left <= c && c <= right
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
    use super::*;

    #[test]
    fn empty() {
        let input = StringInput::new("");

        assert!(input.is_empty());
        assert!(input.match_string("", 0));
        assert!(!input.match_string("a", 0));
    }

    #[test]
    fn parts() {
        let input = StringInput::new("asdasdf");

        assert!(!input.is_empty());
        assert!(input.match_string("asd", 0));
        assert!(input.match_string("asdf", 3));
    }

    #[test]
    fn len() {
        assert_eq!(StringInput::new("asdasdf").len(), 7);
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
        assert_eq!(input.line_col(2), (1, 3));
        assert_eq!(input.line_col(3), (1, 4));
        assert_eq!(input.line_col(4), (2, 1));
        assert_eq!(input.line_col(5), (2, 2));
        assert_eq!(input.line_col(6), (2, 3));
        assert_eq!(input.line_col(7), (3, 1));
        assert_eq!(input.line_col(8), (3, 2));
        assert_eq!(input.line_col(11), (3, 3));
    }

    #[test]
    fn line_of() {
        let input = StringInput::new("a\rb\nc\r\nd嗨");

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

    #[test]
    fn match_range() {
        let input = StringInput::new("b");

        assert!(input.match_range('a', 'c', 0));
        assert!(input.match_range('b', 'b', 0));
        assert!(!input.match_range('a', 'a', 0));
        assert!(!input.match_range('c', 'c', 0));
    }

    #[test]
    fn match_insensitive() {
        let input = StringInput::new("AsdASdF");

        assert!(input.match_insensitive("asd", 0));
        assert!(input.match_insensitive("asdf", 3));
    }
}
