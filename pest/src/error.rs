// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::hash::{Hash, Hasher};

use super::inputs::{Input, Position, Span};

/// An `enum` which defines possible errors.
#[derive(Debug)]
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

fn message<R: fmt::Debug, I: Input>(error: &Error<R, I>) -> String {
    match *error {
        Error::ParsingError { ref positives, ref negatives, .. } => {
            match (negatives.is_empty(), positives.is_empty()) {
                (false, false) => {
                    format!(
                        "unexpected {}; expected {}",
                        enumerate(negatives),
                        enumerate(positives)
                    )
                }
                (false, true) => format!("unexpected {}", enumerate(negatives)),
                (true, false) => format!("expected {}", enumerate(positives)),
                (true, true) => "inexplicit parsing error".to_owned()
            }
        }
        Error::CustomErrorPos { ref message, .. } => message.to_owned(),
        Error::CustomErrorSpan { ref message, .. } => message.to_owned()
    }
}

fn enumerate<R: fmt::Debug>(rules: &Vec<R>) -> String {
    match rules.len() {
        1 => format!("{:?}", rules[0]),
        2 => format!("{:?} or {:?}", rules[0], rules[1]),
        l => {
            let separated = rules.iter()
                                 .take(l - 1)
                                 .map(|r| format!("{:?}", r))
                                 .collect::<Vec<_>>()
                                 .join(", ");
            format!("{}, or {:?}", separated, rules[l - 1])
        }
    }
}

fn underline<R: fmt::Debug, I: Input>(error: &Error<R, I>, offset: usize) -> String {
    let mut underline = String::new();

    for _ in 0..offset { underline.push(' '); }

    match *error {
        Error::CustomErrorSpan { ref span, .. } => {
            underline.push('^');
            for _ in 2..(span.end() - span.start()) { underline.push('-'); }
            underline.push('^');
        }
        _ => underline.push_str("^---")
    };

    underline
}

// TODO: Replace None with filename.
fn format<R: fmt::Debug, I: Input>(error: &Error<R, I>) -> String {
    let pos = match *error {
        Error::ParsingError { ref pos, .. } => pos.clone(),
        Error::CustomErrorPos { ref pos, .. } => pos.clone(),
        Error::CustomErrorSpan { ref span, .. } => span.clone().split().0.clone()
    };
    let (line, col) = pos.line_col();
    let line_str_len = format!("{}", line).len();

    let mut spacing = String::new();
    for _ in 0..line_str_len { spacing.push(' '); }

    let filename: Option<String> = None; // Will be replaced.

    let mut result = match filename {
        Some(filename) => format!("{}--> {}:{}:{}\n", spacing, filename, line, col),
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

// We don't want to enforce derivable traits on the Input which forces to implement them manually.

impl<R: Clone, I: Input> Clone for Error<R, I> {
    fn clone(&self) -> Error<R, I> {
        match *self {
            Error::ParsingError { ref positives, ref negatives, ref pos } => {
                Error::ParsingError {
                    positives: positives.clone(),
                    negatives: negatives.clone(),
                    pos: pos.clone()
                }
            }
            Error::CustomErrorPos { ref message, ref pos } => {
                Error::CustomErrorPos {
                    message: message.clone(),
                    pos: pos.clone()
                }
            }
            Error::CustomErrorSpan { ref message, ref span } => {
                Error::CustomErrorSpan {
                    message: message.clone(),
                    span: span.clone()
                }
            }
        }
    }
}

impl<R: PartialEq, I: Input> PartialEq for Error<R, I> {
    fn eq(&self, other: &Error<R, I>) -> bool {
        match *self {
            Error::ParsingError { ref positives, ref negatives, ref pos } => {
                match *other {
                    Error::ParsingError {
                        positives: ref other_positives,
                        negatives: ref other_negatives,
                        pos: ref other_pos
                    } => {
                        positives == other_positives && negatives == other_negatives &&
                            pos == other_pos
                    }
                    _ => false
                }
            }
            Error::CustomErrorPos { ref message, ref pos } => {
                match *other {
                    Error::CustomErrorPos {
                        message: ref other_message,
                        pos: ref other_pos
                    } => {
                        message == other_message && pos == other_pos
                    }
                    _ => false
                }
            }
            Error::CustomErrorSpan { ref message, ref span } => {
                match *other {
                    Error::CustomErrorSpan {
                        message: ref other_message,
                        span: ref other_span
                    } => {
                        message == other_message && span == other_span
                    }
                    _ => false
                }
            }
        }
    }
}

impl<R: Eq, I: Input> Eq for Error<R, I> {}

impl<R: Hash, I: Input> Hash for Error<R, I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Error::ParsingError { ref positives, ref negatives, ref pos } => {
                positives.hash(state);
                negatives.hash(state);
                pos.hash(state);
            }
            Error::CustomErrorPos { ref message, ref pos } => {
                message.hash(state);
                pos.hash(state);
            }
            Error::CustomErrorSpan { ref message, ref span } => {
                message.hash(state);
                span.hash(state);
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
    fn display_parsing_error() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = position::new(Rc::new(input), 4);
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
    fn display_custom() {
        let input = StringInput::new("ab\ncd\nef".to_owned());
        let pos = position::new(Rc::new(input), 4);
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
}
