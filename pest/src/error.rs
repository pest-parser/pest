// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Types for different kinds of parsing failures.

use std::cmp;
use std::error;
use std::fmt;
use std::mem;

use position::Position;
use span::Span;
use RuleType;

/// Parse-related error type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Error<R> {
    /// Variant of the error
    pub variant: ErrorVariant<R>,
    /// Location within the input string
    pub location: InputLocation,
    /// Line/column within the input string
    pub line_col: LineColLocation,
    path: Option<String>,
    line: String,
    continued_line: Option<String>,
}

/// Different kinds of parsing errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ErrorVariant<R> {
    /// Generated parsing error with expected and unexpected `Rule`s
    ParsingError {
        /// Positive attempts
        positives: Vec<R>,
        /// Negative attempts
        negatives: Vec<R>,
    },
    /// Custom error with a message
    CustomError {
        /// Short explanation
        message: String,
    },
}

/// Where an `Error` has occurred.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InputLocation {
    /// `Error` was created by `Error::new_from_pos`
    Pos(usize),
    /// `Error` was created by `Error::new_from_span`
    Span((usize, usize)),
}

/// Line/column where an `Error` has occurred.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LineColLocation {
    /// Line/column pair if `Error` was created by `Error::new_from_pos`
    Pos((usize, usize)),
    /// Line/column pairs if `Error` was created by `Error::new_from_span`
    Span((usize, usize), (usize, usize)),
}

impl<R: RuleType> Error<R> {
    /// Creates `Error` from `ErrorVariant` and `Position`.
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
    #[allow(clippy::needless_pass_by_value)]
    pub fn new_from_pos(variant: ErrorVariant<R>, pos: Position) -> Error<R> {
        Error {
            variant,
            location: InputLocation::Pos(pos.pos()),
            path: None,
            line: visualize_whitespace(pos.line_of()),
            continued_line: None,
            line_col: LineColLocation::Pos(pos.line_col()),
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
    #[allow(clippy::needless_pass_by_value)]
    pub fn new_from_span(variant: ErrorVariant<R>, span: Span) -> Error<R> {
        let end = span.end_pos();

        let mut end_line_col = end.line_col();
        // end position is after a \n, so we want to point to the visual lf symbol
        if end_line_col.1 == 1 {
            let mut visual_end = end.clone();
            visual_end.skip_back(1);
            let lc = visual_end.line_col();
            end_line_col = (lc.0, lc.1 + 1);
        };

        let mut line_iter = span.lines();
        let start_line = visualize_whitespace(line_iter.next().unwrap_or(""));
        let continued_line = line_iter.last().map(visualize_whitespace);

        Error {
            variant,
            location: InputLocation::Span((span.start(), end.pos())),
            path: None,
            line: start_line,
            continued_line,
            line_col: LineColLocation::Span(span.start_pos().line_col(), end_line_col),
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

    /// Renames all `Rule`s if this is a [`ParsingError`]. It does nothing when called on a
    /// [`CustomError`].
    ///
    /// Useful in order to rename verbose rules or have detailed per-`Rule` formatting.
    ///
    /// [`ParsingError`]: enum.ErrorVariant.html#variant.ParsingError
    /// [`CustomError`]: enum.ErrorVariant.html#variant.CustomError
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
        F: FnMut(&R) -> String,
    {
        let variant = match self.variant {
            ErrorVariant::ParsingError {
                positives,
                negatives,
            } => {
                let message = Error::parsing_error_message(&positives, &negatives, f);
                ErrorVariant::CustomError { message }
            }
            variant => variant,
        };

        self.variant = variant;

        self
    }

    fn start(&self) -> (usize, usize) {
        match self.line_col {
            LineColLocation::Pos(line_col) => line_col,
            LineColLocation::Span(start_line_col, _) => start_line_col,
        }
    }

    fn spacing(&self) -> String {
        let line = match self.line_col {
            LineColLocation::Pos((line, _)) => line,
            LineColLocation::Span((start_line, _), (end_line, _)) => cmp::max(start_line, end_line),
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

        let mut start = self.start().1;
        let end = match self.line_col {
            LineColLocation::Span(_, (_, mut end)) => {
                let inverted_cols = start > end;
                if inverted_cols {
                    mem::swap(&mut start, &mut end);
                    start -= 1;
                    end += 1;
                }

                Some(end)
            }
            _ => None,
        };
        let offset = start - 1;
        let line_chars = self.line.chars();

        for c in line_chars.take(offset) {
            match c {
                '\t' => underline.push('\t'),
                _ => underline.push(' '),
            }
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
                ref negatives,
            } => Error::parsing_error_message(positives, negatives, |r| format!("{:?}", r)),
            ErrorVariant::CustomError { ref message } => message.clone(),
        }
    }

    fn parsing_error_message<F>(positives: &[R], negatives: &[R], mut f: F) -> String
    where
        F: FnMut(&R) -> String,
    {
        match (negatives.is_empty(), positives.is_empty()) {
            (false, false) => format!(
                "unexpected {}; expected {}",
                Error::enumerate(negatives, &mut f),
                Error::enumerate(positives, &mut f)
            ),
            (false, true) => format!("unexpected {}", Error::enumerate(negatives, &mut f)),
            (true, false) => format!("expected {}", Error::enumerate(positives, &mut f)),
            (true, true) => "unknown parsing error".to_owned(),
        }
    }

    fn enumerate<F>(rules: &[R], f: &mut F) -> String
    where
        F: FnMut(&R) -> String,
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
        let path = self
            .path
            .as_ref()
            .map(|path| format!("{}:", path))
            .unwrap_or_default();

        let pair = (self.line_col.clone(), &self.continued_line);
        if let (LineColLocation::Span(_, end), &Some(ref continued_line)) = pair {
            let has_line_gap = end.0 - self.start().0 > 1;
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
                    ls = self.start().0,
                    le = end.0,
                    c = self.start().1,
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
                    ls = self.start().0,
                    le = end.0,
                    c = self.start().1,
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
                l = self.start().0,
                c = self.start().1,
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
            ErrorVariant::CustomError { ref message } => message,
        }
    }
}

fn visualize_whitespace(input: &str) -> String {
    input.to_owned().replace('\r', "␍").replace('\n', "␊")
}

#[cfg(test)]
mod tests {
    use super::super::position;
    use super::*;

    #[test]
    fn display_parsing_error_mixed() {
        let input = "ab\ncd\nef";
        let pos = position::Position::new(input, 4).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2, 3],
                negatives: vec![4, 5, 6],
            },
            pos,
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd␊",
                "  |  ^---",
                "  |",
                "  = unexpected 4, 5, or 6; expected 1, 2, or 3",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_parsing_error_positives() {
        let input = "ab\ncd\nef";
        let pos = position::Position::new(input, 4).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2],
                negatives: vec![],
            },
            pos,
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd␊",
                "  |  ^---",
                "  |",
                "  = expected 1 or 2",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_parsing_error_negatives() {
        let input = "ab\ncd\nef";
        let pos = position::Position::new(input, 4).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![4, 5, 6],
            },
            pos,
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd␊",
                "  |  ^---",
                "  |",
                "  = unexpected 4, 5, or 6",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_parsing_error_unknown() {
        let input = "ab\ncd\nef";
        let pos = position::Position::new(input, 4).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            pos,
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd␊",
                "  |  ^---",
                "  |",
                "  = unknown parsing error",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_custom_pos() {
        let input = "ab\ncd\nef";
        let pos = position::Position::new(input, 4).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned(),
            },
            pos,
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd␊",
                "  |  ^---",
                "  |",
                "  = error: big one",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_custom_span_two_lines() {
        let input = "ab\ncd\nefgh";
        let start = position::Position::new(input, 4).unwrap();
        let end = position::Position::new(input, 9).unwrap();
        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned(),
            },
            start.span(&end),
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd␊",
                "3 | efgh",
                "  |  ^^",
                "  |",
                "  = error: big one",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_custom_span_three_lines() {
        let input = "ab\ncd\nefgh";
        let start = position::Position::new(input, 1).unwrap();
        let end = position::Position::new(input, 9).unwrap();
        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned(),
            },
            start.span(&end),
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 1:2",
                "  |",
                "1 | ab␊",
                "  | ...",
                "3 | efgh",
                "  |  ^^",
                "  |",
                "  = error: big one",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_custom_span_two_lines_inverted_cols() {
        let input = "abcdef\ngh";
        let start = position::Position::new(input, 5).unwrap();
        let end = position::Position::new(input, 8).unwrap();
        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned(),
            },
            start.span(&end),
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 1:6",
                "  |",
                "1 | abcdef␊",
                "2 | gh",
                "  | ^----^",
                "  |",
                "  = error: big one",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_custom_span_end_after_newline() {
        let input = "abcdef\n";
        let start = position::Position::new(input, 0).unwrap();
        let end = position::Position::new(input, 7).unwrap();
        assert!(start.at_start());
        assert!(end.at_end());

        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: big one".to_owned(),
            },
            start.span(&end),
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 1:1",
                "  |",
                "1 | abcdef␊",
                "  | ^-----^",
                "  |",
                "  = error: big one",
            ]
            .join("\n")
        );
    }

    #[test]
    fn display_custom_span_empty() {
        let input = "";
        let start = position::Position::new(input, 0).unwrap();
        let end = position::Position::new(input, 0).unwrap();
        assert!(start.at_start());
        assert!(end.at_end());

        let error: Error<u32> = Error::new_from_span(
            ErrorVariant::CustomError {
                message: "error: empty".to_owned(),
            },
            start.span(&end),
        );

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 1:1",
                "  |",
                "1 | ",
                "  | ^",
                "  |",
                "  = error: empty",
            ]
            .join("\n")
        );
    }

    #[test]
    fn mapped_parsing_error() {
        let input = "ab\ncd\nef";
        let pos = position::Position::new(input, 4).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2, 3],
                negatives: vec![4, 5, 6],
            },
            pos,
        )
        .renamed_rules(|n| format!("{}", n + 1));

        assert_eq!(
            format!("{}", error),
            vec![
                " --> 2:2",
                "  |",
                "2 | cd␊",
                "  |  ^---",
                "  |",
                "  = unexpected 5, 6, or 7; expected 2, 3, or 4",
            ]
            .join("\n")
        );
    }

    #[test]
    fn error_with_path() {
        let input = "ab\ncd\nef";
        let pos = position::Position::new(input, 4).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2, 3],
                negatives: vec![4, 5, 6],
            },
            pos,
        )
        .with_path("file.rs");

        assert_eq!(
            format!("{}", error),
            vec![
                " --> file.rs:2:2",
                "  |",
                "2 | cd␊",
                "  |  ^---",
                "  |",
                "  = unexpected 4, 5, or 6; expected 1, 2, or 3",
            ]
            .join("\n")
        );
    }

    #[test]
    fn underline_with_tabs() {
        let input = "a\txbc";
        let pos = position::Position::new(input, 2).unwrap();
        let error: Error<u32> = Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![1, 2, 3],
                negatives: vec![4, 5, 6],
            },
            pos,
        )
        .with_path("file.rs");

        assert_eq!(
            format!("{}", error),
            vec![
                " --> file.rs:1:3",
                "  |",
                "1 | a	xbc",
                "  |  	^---",
                "  |",
                "  = unexpected 4, 5, or 6; expected 1, 2, or 3",
            ]
            .join("\n")
        );
    }
}
