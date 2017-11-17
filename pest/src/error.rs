// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::error;
use std::fmt;

use inputs::{Input, position, Position, Span};
use RuleType;

/// An `enum` which defines possible errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error<R, I: Input> {
    /// Generated parsing error with expected and unexpected `Rule`s and a position
    ParsingError {
        /// Positive attempts
        positives: Vec<R>,
        /// Negative attempts
        negatives: Vec<R>,
        /// Deepest position of attempts
        pos: Position<I>
    },
    /// Custom error with a message and a position
    CustomErrorPos {
        /// Short explanation
        message: String,
        /// Error `Position` for formatting
        pos: Position<I>
    },
    /// Custom error with a message and a span defined by a start and end position
    CustomErrorSpan {
        /// Short explanation
        message: String,
        /// Error `Span` for formatting
        span: Span<I>
    }
}

impl <R: RuleType, I: Input> Error<R, I> {
    /// Renames all `Rule`s from a `ParsingError` variant returning a `CustomErrorPos`. It does
    /// nothing when called on `CustomErrorPos` and `CustomErrorSpan` variants.
    ///
    /// Useful in order to rename verbose rules or have detailed per-`Rule` formatting.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest::Error;
    /// # use pest::inputs::{Position, StringInput};
    /// # #[allow(non_camel_case_types)]
    /// # #[allow(dead_code)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule {
    /// #     open_paren,
    /// #     closed_paren
    /// # }
    /// # let input = Rc::new(StringInput::new("".to_owned()));
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
    pub fn renamed_rules<F>(self, f: F) -> Error<R, I>
    where
        F: FnMut(&R) -> String
    {
        match self {
            Error::ParsingError { positives, negatives, pos } => {
                let message = parsing_error_message(&positives, &negatives, f);
                Error::CustomErrorPos {
                    message,
                    pos
                }
            }
            error => error
        }
    }
}

fn message<R: fmt::Debug, I: Input>(error: &Error<R, I>) -> String {
    match *error {
        Error::ParsingError { ref positives, ref negatives, .. } => {
            parsing_error_message(positives, negatives, |r| format!("{:?}", r))
        }
        Error::CustomErrorPos { ref message, .. } | Error::CustomErrorSpan { ref message, .. } => {
            message.to_owned()
        }
    }
}

fn parsing_error_message<R: fmt::Debug, F>(
    positives: &[R],
    negatives: &[R],
    mut f: F
) -> String
where
    F: FnMut(&R) -> String
{
    match (negatives.is_empty(), positives.is_empty()) {
        (false, false) => {
            format!(
                "unexpected {}; expected {}",
                enumerate(negatives, &mut f),
                enumerate(positives, &mut f)
            )
        }
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
            let separated = rules.iter()
                                 .take(l - 1)
                                 .map(|r| f(r))
                                 .collect::<Vec<_>>()
                                 .join(", ");
            format!("{}, or {}", separated, f(&rules[l - 1]))
        }
    }
}

fn underline<R: fmt::Debug, I: Input>(error: &Error<R, I>, offset: usize) -> String {
    let mut underline = String::new();

    for _ in 0..offset { underline.push(' '); }

    match *error {
        Error::CustomErrorSpan { ref span, .. } => {
            if span.end() - span.start() > 1 {
                underline.push('^');
                for _ in 2..(span.end() - span.start()) { underline.push('-'); }
                underline.push('^');
            } else {
                underline.push('^');
            }
        }
        _ => underline.push_str("^---")
    };

    underline
}

fn format<R: fmt::Debug, I: Input>(error: &Error<R, I>) -> String {
    let pos = match *error {
        Error::ParsingError { ref pos, .. } | Error::CustomErrorPos { ref pos, .. } => pos.clone(),
        Error::CustomErrorSpan { ref span, .. } => span.clone().split().0.clone()
    };
    let (line, col) = pos.line_col();
    let line_str_len = format!("{}", line).len();

    let mut spacing = String::new();
    for _ in 0..line_str_len { spacing.push(' '); }

    let filename = position::into_input(&pos).file_name();

    let mut result = match filename {
        Some(filename) => format!("{}--> {}:{}:{}\n", spacing, filename.to_string_lossy(), line,
                                  col),
        None => format!("{}--> {}:{}\n", spacing, line, col)
    };

    result.push_str(&format!("{} |\n", spacing));
    result.push_str(&format!("{} | ", line));

    let line = pos.line_of();
    result.push_str(&format!("{}\n", line));
    result.push_str(&format!("{} | {}\n", spacing, underline(error, col - 1)));
    result.push_str(&format!("{} |\n", spacing));
    result.push_str(&format!("{} = {}", spacing, message(error)));

    result
}

impl<R: fmt::Debug, I: Input> fmt::Display for Error<R, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format(self))
    }
}

impl<R: fmt::Debug, I: Input> error::Error for Error<R, I> {
    fn description(&self) -> &str {
        match *self {
            Error::ParsingError { ref positives, ref negatives, .. } => {
                "parsing error"
            }
            Error::CustomErrorPos { ref message, .. } | Error::CustomErrorSpan { ref message, .. } => {
                message
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::*;
    use super::super::inputs::{position, StringInput};

    #[test]
    fn display_parsing_error_mixed() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = unsafe { position::new(Rc::new(input), 4) };
        let error: Error<u32, _> = Error::ParsingError {
            positives: vec![1, 2, 3],
            negatives: vec![4, 5, 6],
            pos: pos
        };

        assert_eq!(format!("{}", error), vec![
            " --> 2:2",
            "  |",
            "2 | cd",
            "  |  ^---",
            "  |",
            "  = unexpected 4, 5, or 6; expected 1, 2, or 3"
        ].join("\n"));
    }

    #[test]
    fn display_parsing_error_positives() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = unsafe { position::new(Rc::new(input), 4) };
        let error: Error<u32, _> = Error::ParsingError {
            positives: vec![1, 2],
            negatives: vec![],
            pos: pos
        };

        assert_eq!(format!("{}", error), vec![
            " --> 2:2",
            "  |",
            "2 | cd",
            "  |  ^---",
            "  |",
            "  = expected 1 or 2"
        ].join("\n"));
    }

    #[test]
    fn display_parsing_error_negatives() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = unsafe { position::new(Rc::new(input), 4) };
        let error: Error<u32, _> = Error::ParsingError {
            positives: vec![],
            negatives: vec![4, 5, 6],
            pos: pos
        };

        assert_eq!(format!("{}", error), vec![
            " --> 2:2",
            "  |",
            "2 | cd",
            "  |  ^---",
            "  |",
            "  = unexpected 4, 5, or 6"
        ].join("\n"));
    }

    #[test]
    fn display_parsing_error_unknown() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = unsafe { position::new(Rc::new(input), 4) };
        let error: Error<u32, _> = Error::ParsingError {
            positives: vec![],
            negatives: vec![],
            pos: pos
        };

        assert_eq!(format!("{}", error), vec![
            " --> 2:2",
            "  |",
            "2 | cd",
            "  |  ^---",
            "  |",
            "  = unknown parsing error"
        ].join("\n"));
    }

    #[test]
    fn display_custom() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = unsafe { position::new(Rc::new(input), 4) };
        let error: Error<&str, _> = Error::CustomErrorPos {
            message: "error: big one".to_owned(),
            pos: pos
        };

        assert_eq!(format!("{}", error), vec![
            " --> 2:2",
            "  |",
            "2 | cd",
            "  |  ^---",
            "  |",
            "  = error: big one"
        ].join("\n"));
    }

    #[test]
    fn mapped_parsing_error() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = unsafe { position::new(Rc::new(input), 4) };
        let error: Error<u32, _> = Error::ParsingError {
            positives: vec![1, 2, 3],
            negatives: vec![4, 5, 6],
            pos: pos
        }.renamed_rules(|n| format!("{}", n + 1));

        assert_eq!(format!("{}", error), vec![
            " --> 2:2",
            "  |",
            "2 | cd",
            "  |  ^---",
            "  |",
            "  = unexpected 5, 6, or 7; expected 2, 3, or 4"
        ].join("\n"));
    }
}
