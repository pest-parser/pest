// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::error;
use std::fmt;
use std::mem;

use RuleType;
use position::Position;
use span::Span;

/// An `enum` which defines possible errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error<'i, R> {
    /// Generated parsing error with expected and unexpected `Rule`s and a position
    ParsingError {
        /// Positive attempts
        positives: Vec<R>,
        /// Negative attempts
        negatives: Vec<R>,
        /// Deepest position of attempts
        pos: Position<'i>
    },
    /// Custom error with a message and a position
    CustomErrorPos {
        /// Short explanation
        message: String,
        /// Error `Position` for formatting
        pos: Position<'i>
    },
    /// Custom error with a message and a span defined by a start and end position
    CustomErrorSpan {
        /// Short explanation
        message: String,
        /// Error `Span` for formatting
        span: Span<'i>
    }
}

impl<'i, R: RuleType> Error<'i, R> {
    /// Renames all `Rule`s from a `ParsingError` variant returning a `CustomErrorPos`. It does
    /// nothing when called on `CustomErrorPos` and `CustomErrorSpan` variants.
    ///
    /// Useful in order to rename verbose rules or have detailed per-`Rule` formatting.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Error;
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
    /// Error::ParsingError {
    ///     positives: vec![Rule::open_paren],
    ///     negatives: vec![Rule::closed_paren],
    ///     pos: pos
    /// }.renamed_rules(|rule| {
    ///     match *rule {
    ///         Rule::open_paren => "(".to_owned(),
    ///         Rule::closed_paren => "closed paren".to_owned()
    ///     }
    /// });
    /// ```
    pub fn renamed_rules<F>(self, f: F) -> Error<'i, R>
    where
        F: FnMut(&R) -> String
    {
        match self {
            Error::ParsingError {
                positives,
                negatives,
                pos
            } => {
                let message = parsing_error_message(&positives, &negatives, f);
                Error::CustomErrorPos { message, pos }
            }
            error => error
        }
    }
}

fn message<'i, R: fmt::Debug>(error: &Error<'i, R>) -> String {
    match *error {
        Error::ParsingError {
            ref positives,
            ref negatives,
            ..
        } => parsing_error_message(positives, negatives, |r| format!("{:?}", r)),
        Error::CustomErrorPos { ref message, .. } | Error::CustomErrorSpan { ref message, .. } => {
            message.to_owned()
        }
    }
}

fn parsing_error_message<R: fmt::Debug, F>(positives: &[R], negatives: &[R], mut f: F) -> String
where
    F: FnMut(&R) -> String
{
    match (negatives.is_empty(), positives.is_empty()) {
        (false, false) => format!(
            "unexpected {}; expected {}",
            enumerate(negatives, &mut f),
            enumerate(positives, &mut f)
        ),
        (false, true) => format!("unexpected {}", enumerate(negatives, &mut f)),
        (true, false) => format!("expected {}", enumerate(positives, &mut f)),
        (true, true) => "unknown parsing error".to_owned()
    }
}

fn enumerate<R: fmt::Debug, F>(rules: &[R], f: &mut F) -> String
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

fn underline(start: usize, end: Option<usize>, offset: usize) -> String {
    let mut underline = String::new();

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

fn format<'i, R: fmt::Debug>(error: &Error<'i, R>) -> String {
    let pos = match *error {
        Error::ParsingError { ref pos, .. } | Error::CustomErrorPos { ref pos, .. } => pos.clone(),
        Error::CustomErrorSpan { ref span, .. } => span.clone().split().0.clone()
    };
    let (line, col) = pos.line_col();
    let line_str_len = format!("{}", line).len();

    let mut spacing = String::new();
    for _ in 0..line_str_len {
        spacing.push(' ');
    }

    let mut result = format!("{}--> {}:{}\n", spacing, line, col);
    result.push_str(&format!("{} |\n", spacing));

    let underlined;

    if let Error::CustomErrorSpan { ref span, .. } = *error {
        let mut start = span.start_pos().line_col();
        let mut end = span.end_pos().line_col();

        if start.1 > end.1 {
            mem::swap(&mut start.1, &mut end.1);
            start.1 -= 1;
            end.1 += 1;
        }

        underlined = underline(start.1, Some(end.1), start.1 - 1);

        if start.0 != end.0 {
            result.push_str(&format!("{} | ", start.0));

            let pos = span.start_pos();
            let line = pos.line_of();
            result.push_str(&format!("{}\n", line));

            if end.0 - start.0 > 1 {
                result.push_str(&format!("{} | ...\n", spacing));
            }

            result.push_str(&format!("{} | ", end.0));

            let pos = span.end_pos();
            let line = pos.line_of();
            result.push_str(&format!("{}\n", line));
            result.push_str(&format!("{} | {}\n", spacing, underlined));

            result.push_str(&format!("{} |\n", spacing));
            result.push_str(&format!("{} = {}", spacing, message(error)));

            return result;
        }
    } else {
        underlined = underline(pos.pos(), None, col - 1);
    }

    result.push_str(&format!("{} | ", line));

    let line = pos.line_of();
    result.push_str(&format!("{}\n", line));
    result.push_str(&format!("{} | {}\n", spacing, underlined));

    result.push_str(&format!("{} |\n", spacing));
    result.push_str(&format!("{} = {}", spacing, message(error)));

    result
}

impl<'i, R: fmt::Debug> fmt::Display for Error<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format(self))
    }
}

impl<'i, R: fmt::Debug> error::Error for Error<'i, R> {
    fn description(&self) -> &str {
        match *self {
            Error::ParsingError { .. } => "parsing error",
            Error::CustomErrorPos { ref message, .. }
            | Error::CustomErrorSpan { ref message, .. } => message
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::position;

    #[test]
    fn display_parsing_error_mixed() {
        let input = "ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::ParsingError {
            positives: vec![1, 2, 3],
            negatives: vec![4, 5, 6],
            pos
        };

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
        let input = "ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::ParsingError {
            positives: vec![1, 2],
            negatives: vec![],
            pos
        };

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
        let input = "ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::ParsingError {
            positives: vec![],
            negatives: vec![4, 5, 6],
            pos
        };

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
        let input = "ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::ParsingError {
            positives: vec![],
            negatives: vec![],
            pos
        };

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
        let input = "ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<&str> = Error::CustomErrorPos {
            message: "error: big one".to_owned(),
            pos
        };

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
        let input = "ab\ncd\nefgh";
        let start = unsafe { position::new(input, 4) };
        let end = unsafe { position::new(input, 9) };
        let error: Error<&str> = Error::CustomErrorSpan {
            message: "error: big one".to_owned(),
            span: start.span(&end)
        };

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
        let input = "ab\ncd\nefgh";
        let start = unsafe { position::new(input, 1) };
        let end = unsafe { position::new(input, 9) };
        let error: Error<&str> = Error::CustomErrorSpan {
            message: "error: big one".to_owned(),
            span: start.span(&end)
        };

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
    fn display_custom_span_two_lines_inverted() {
        let input = "abcdef\ngh";
        let start = unsafe { position::new(input, 5) };
        let end = unsafe { position::new(input, 8) };
        let error: Error<&str> = Error::CustomErrorSpan {
            message: "error: big one".to_owned(),
            span: start.span(&end)
        };

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
        let input = "ab\ncd\nef";
        let pos = unsafe { position::new(input, 4) };
        let error: Error<u32> = Error::ParsingError {
            positives: vec![1, 2, 3],
            negatives: vec![4, 5, 6],
            pos
        }.renamed_rules(|n| format!("{}", n + 1));

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
}
