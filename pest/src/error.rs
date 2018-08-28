// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! A `mod` containing error data structures.

use std::cmp;
use std::error;
use std::fmt;
use std::mem;

use RuleType;
use position::Position;
use span::Span;

/// A `struct` defining errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Error<R> {
    /// Variant of the error
    pub variant: ErrorVariant<R>,
    /// Location within the input string
    pub location: InputLocation,
    path: Option<String>,
    line: String,
    continued_line: Option<String>,
    start: (usize, usize),
    end: Option<(usize, usize)>
}

/// An `enum` describing `Error` variants.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ErrorVariant<R> {
    /// Generated parsing error with expected and unexpected `Rule`s
    ParsingError {
        /// Positive attempts
        positives: Vec<R>,
        /// Negative attempts
        negatives: Vec<R>
    },
    /// Custom error with a message
    CustomError {
        /// Short explanation
        message: String
    }
}

/// An `enum` describing where the `Error` has occurred.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InputLocation {
    /// `Error` was created by `Error::new_from_pos`
    Pos(usize),
    /// `Error` was created by `Error::new_from_span`
    Span((usize, usize))
}

impl<R: RuleType> Error<R> {
    /// Creates `Error` from `ErrorVariant` and `Possition`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::error::{Error, ErrorVariant};
    /// # use pest::Position;
    /// # #[allow(non_camel_case_types)]
    /// # #[allow(dead_code)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule {
    /// #     open_paren,
    /// #     closed_paren
    /// # }
    /// # let input = "";
    /// # let pos = Position::from_start(input);
    /// let error = Error::new_from_pos(
    ///     ErrorVariant::ParsingError {
    ///         positives: vec![Rule::open_paren],
    ///         negatives: vec![Rule::closed_paren]
    ///     },
    ///     pos
    /// );
    ///
    /// println!("{}", error);
    /// ```
    pub fn new_from_pos(variant: ErrorVariant<R>, pos: Position) -> Error<R> {
        Error {
            variant,
            location: InputLocation::Pos(pos.pos()),
            path: None,
            line: pos.line_of().to_owned(),
            continued_line: None,
            start: pos.line_col(),
            end: None
        }
    }

    /// Creates `Error` from `ErrorVariant` and `Span`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::error::{Error, ErrorVariant};
    /// # use pest::{Position, Span};
    /// # #[allow(non_camel_case_types)]
    /// # #[allow(dead_code)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule {
    /// #     open_paren,
    /// #     closed_paren
    /// # }
    /// # let input = "";
    /// # let start = Position::from_start(input);
    /// # let end = start.clone();
    /// # let span = start.span(&end);
    /// let error = Error::new_from_span(
    ///     ErrorVariant::ParsingError {
    ///         positives: vec![Rule::open_paren],
    ///         negatives: vec![Rule::closed_paren]
    ///     },
    ///     span
    /// );
    ///
    /// println!("{}", error);
    /// ```
    pub fn new_from_span(variant: ErrorVariant<R>, span: Span) -> Error<R> {
        let continued_line = if span.start_pos().line_col().0 != span.end_pos().line_col().0 {
            Some(span.end_pos().line_of().to_owned())
        } else {
            None
        };

        Error {
            variant,
            location: InputLocation::Span((span.start(), span.end())),
            path: None,
            line: span.start_pos().line_of().to_owned(),
            continued_line,
            start: span.start_pos().line_col(),
            end: Some(span.end_pos().line_col())
        }
    }

    /// Returns `Error` variant with `path` which is shown when formatted with `Display`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::error::{Error, ErrorVariant};
    /// # use pest::Position;
    /// # #[allow(non_camel_case_types)]
    /// # #[allow(dead_code)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule {
    /// #     open_paren,
    /// #     closed_paren
    /// # }
    /// # let input = "";
    /// # let pos = Position::from_start(input);
    /// Error::new_from_pos(
    ///     ErrorVariant::ParsingError {
    ///         positives: vec![Rule::open_paren],
    ///         negatives: vec![Rule::closed_paren]
    ///     },
    ///     pos
    /// ).with_path("file.rs");
    /// ```
    pub fn with_path(mut self, path: &str) -> Error<R> {
        self.path = Some(path.to_owned());

        self
    }

    /// Renames all `Rule`s from a `ParsingError` `variant`. It does nothing when called on
    /// `CustomError` `varriant`.
    ///
    /// Useful in order to rename verbose rules or have detailed per-`Rule` formatting.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::error::{Error, ErrorVariant};
    /// # use pest::Position;
    /// # #[allow(non_camel_case_types)]
    /// # #[allow(dead_code)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule {
    /// #     open_paren,
    /// #     closed_paren
    /// # }
    /// # let input = "";
    /// # let pos = Position::from_start(input);
    /// Error::new_from_pos(
    ///     ErrorVariant::ParsingError {
    ///         positives: vec![Rule::open_paren],
    ///         negatives: vec![Rule::closed_paren]
    ///     },
    ///     pos
    /// ).renamed_rules(|rule| {
    ///     match *rule {
    ///         Rule::open_paren => "(".to_owned(),
    ///         Rule::closed_paren => "closed paren".to_owned()
    ///     }
    /// });
    /// ```
    pub fn renamed_rules<F>(mut self, f: F) -> Error<R>
    where
        F: FnMut(&R) -> String
    {
        let variant = match self.variant {
            ErrorVariant::ParsingError {
                positives,
                negatives
            } => {
                let message = Error::parsing_error_message(&positives, &negatives, f);
                ErrorVariant::CustomError { message }
            }
            variant => variant
        };

        self.variant = variant;

        self
    }

    fn spacing(&self) -> String {
        let line = if let Some((line, _)) = self.end {
            cmp::max(self.start.0, line)
        } else {
            self.start.0
        };

        let line_str_len = format!("{}", line).len();

        let mut spacing = String::new();
        for _ in 0..line_str_len {
            spacing.push(' ');
        }

        spacing
    }

    fn underline(&self) -> String {
        let mut underline = String::new();

        let mut start = self.start.1;
        let end = if let Some((_, mut end)) = self.end {
            let inverted_cols = start > end;
            if inverted_cols {
                mem::swap(&mut start, &mut end);
                start -= 1;
                end += 1;
            }

            Some(end)
        } else {
            None
        };
        let offset = start - 1;

        for _ in 0..offset {
            underline.push(' ');
        }

        if let Some(end) = end {
            if end - start > 1 {
                underline.push('^');
                for _ in 2..(end - start) {
                    underline.push('-');
                }
                underline.push('^');
            } else {
                underline.push('^');
            }
        } else {
            underline.push_str("^---")
        }

        underline
    }

    fn message(&self) -> String {
        match self.variant {
            ErrorVariant::ParsingError {
                ref positives,
                ref negatives
            } => Error::parsing_error_message(positives, negatives, |r| format!("{:?}", r)),
            ErrorVariant::CustomError { ref message } => {
                message.clone()
            }
        }
    }

    fn parsing_error_message<F>(positives: &[R], negatives: &[R], mut f: F) -> String
    where
        F: FnMut(&R) -> String
    {
        match (negatives.is_empty(), positives.is_empty()) {
            (false, false) => format!(
                "unexpected {}; expected {}",
                Error::enumerate(negatives, &mut f),
                Error::enumerate(positives, &mut f)
            ),
            (false, true) => format!("unexpected {}", Error::enumerate(negatives, &mut f)),
            (true, false) => format!("expected {}", Error::enumerate(positives, &mut f)),
            (true, true) => "unknown parsing error".to_owned()
        }
    }

    fn enumerate<F>(rules: &[R], f: &mut F) -> String
    where
        F: FnMut(&R) -> String
    {
        match rules.len() {
            1 => f(&rules[0]),
            2 => format!("{} or {}", f(&rules[0]), f(&rules[1])),
            l => {
                let separated = rules
                    .iter()
                    .take(l - 1)
                    .map(|r| f(r))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}, or {}", separated, f(&rules[l - 1]))
            }
        }
    }

    pub(crate) fn format(&self) -> String {
        let spacing = self.spacing();
        let path = self.path.as_ref().map(|path| format!("{}:", path)).unwrap_or_default();

        if let (Some(end), &Some(ref continued_line)) = (self.end, &self.continued_line) {
            let has_line_gap = end.0 - self.start.0 > 1;
            if has_line_gap {
                format!(
                    "{s    }--> {p}{ls}:{c}\n\
                     {s    } |\n\
                     {ls:w$} | {line}\n\
                     {s    } | ...\n\
                     {le:w$} | {continued_line}\n\
                     {s    } | {underline}\n\
                     {s    } |\n\
                     {s    } = {message}",
                    s = spacing,
                    w = spacing.len(),
                    p = path,
                    ls = self.start.0,
                    le = end.0,
                    c = self.start.1,
                    line = self.line,
                    continued_line = continued_line,
                    underline = self.underline(),
                    message = self.message()
                )
            } else {
                format!(
                    "{s    }--> {p}{ls}:{c}\n\
                     {s    } |\n\
                     {ls:w$} | {line}\n\
                     {le:w$} | {continued_line}\n\
                     {s    } | {underline}\n\
                     {s    } |\n\
                     {s    } = {message}",
                    s = spacing,
                    w = spacing.len(),
                    p = path,
                    ls = self.start.0,
                    le = end.0,
                    c = self.start.1,
                    line = self.line,
                    continued_line = continued_line,
                    underline = self.underline(),
                    message = self.message()
                )
            }
        } else {
            format!(
                "{s}--> {p}{l}:{c}\n\
                 {s} |\n\
                 {l} | {line}\n\
                 {s} | {underline}\n\
                 {s} |\n\
                 {s} = {message}",
                s = spacing,
                p = path,
                l = self.start.0,
                c = self.start.1,
                line = self.line,
                underline = self.underline(),
                message = self.message()
            )
        }
    }
}

impl<R: RuleType> fmt::Display for Error<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

impl<'i, R: RuleType> error::Error for Error<R> {
    fn description(&self) -> &str {
        match self.variant {
            ErrorVariant::ParsingError { .. } => "parsing error",
            ErrorVariant::CustomError { ref message } => message
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::position;

    #[test]
    fn display_parsing_error_mixed() {
        let input = b"ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2, 3],
                negatives: vec![4, 5, 6]
            },
            pos
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd",
                "  |  ^---",
                "  |",
                "  = unexpected 4, 5, or 6; expected 1, 2, or 3",
            ].join("\n")
        );
    }

    #[test]
    fn display_parsing_error_positives() {
        let input = b"ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2],
                negatives: vec![]
            },
            pos
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd",
                "  |  ^---",
                "  |",
                "  = expected 1 or 2",
            ].join("\n")
        );
    }

    #[test]
    fn display_parsing_error_negatives() {
        let input = b"ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![4, 5, 6]
            },
            pos
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd",
                "  |  ^---",
                "  |",
                "  = unexpected 4, 5, or 6",
            ].join("\n")
        );
    }

    #[test]
    fn display_parsing_error_unknown() {
        let input = b"ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![]
            },
            pos
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd",
                "  |  ^---",
                "  |",
                "  = unknown parsing error",
            ].join("\n")
        );
    }

    #[test]
    fn display_custom_pos() {
        let input = b"ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned()
            },
            pos
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd",
                "  |  ^---",
                "  |",
                "  = error: big one",
            ].join("\n")
        );
    }

    #[test]
    fn display_custom_span_two_lines() {
        let input = b"ab\ncd\nefgh";
        let start = unsafe { position::new(input, 4) };
        let end = unsafe { position::new(input, 9) };
        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned()
            },
            start.span(&end)
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd",
                "3 | efgh",
                "  |  ^^",
                "  |",
                "  = error: big one",
            ].join("\n")
        );
    }

    #[test]
    fn display_custom_span_three_lines() {
        let input = b"ab\ncd\nefgh";
        let start = unsafe { position::new(input, 1) };
        let end = unsafe { position::new(input, 9) };
        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned()
            },
            start.span(&end)
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 1:2",
                "  |",
                "1 | ab",
                "  | ...",
                "3 | efgh",
                "  |  ^^",
                "  |",
                "  = error: big one",
            ].join("\n")
        );
    }

    #[test]
    fn display_custom_span_two_lines_inverted_cols() {
        let input = b"abcdef\ngh";
        let start = unsafe { position::new(input, 5) };
        let end = unsafe { position::new(input, 8) };
        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned()
            },
            start.span(&end)
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 1:6",
                "  |",
                "1 | abcdef",
                "2 | gh",
                "  | ^----^",
                "  |",
                "  = error: big one",
            ].join("\n")
        );
    }

    #[test]
    fn mapped_parsing_error() {
        let input = b"ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2, 3],
                negatives: vec![4, 5, 6]
            },
            pos
        ).renamed_rules(|n| format!("{}", n + 1));

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd",
                "  |  ^---",
                "  |",
                "  = unexpected 5, 6, or 7; expected 2, 3, or 4",
            ].join("\n")
        );
    }

    #[test]
    fn error_with_path() {
        let input = b"ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2, 3],
                negatives: vec![4, 5, 6]
            },
            pos
        ).with_path("file.rs");

        assert_eq!(
            format!("{}", error),
            vec![
                " --> file.rs:2:2",
                "  |",
                "2 | cd",
                "  |  ^---",
                "  |",
                "  = unexpected 4, 5, or 6; expected 1, 2, or 3",
            ].join("\n")
        );
    }
}
