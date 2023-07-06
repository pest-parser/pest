// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Predefined tree nodes

use alloc::{format, vec};

use crate::{
    error::{Error, ErrorVariant},
    parser_state::constrain_idxs,
    Position, RuleType, Span, Stack,
};

use super::TypedNode;

/// Match any character.
#[inline]
pub fn any<'i, R: RuleType>(
    mut input: Position<'i>,
) -> Result<(Position<'i>, Span<'i>, char), Error<R>> {
    let original_input = input.clone();
    let mut c: char = ' ';
    match input.match_char_by(|ch| {
        c = ch;
        true
    }) {
        true => {
            let span = original_input.span(&input);
            Ok((input, span, c))
        }
        false => Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        )),
    }
}

/// Match start of input.
pub fn soi<'i, R: RuleType>(input: Position<'i>) -> Result<Position<'i>, Error<R>> {
    if input.at_start() {
        Ok(input)
    } else {
        Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        ))
    }
}

/// Match end of input.
pub fn eoi<'i, R: RuleType>(input: Position<'i>) -> Result<Position<'i>, Error<R>> {
    if input.at_end() {
        Ok(input)
    } else {
        Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        ))
    }
}

/// match a single end of line.
pub fn new_line<'i, R: RuleType>(
    mut input: Position<'i>,
) -> Result<(Position<'i>, Span<'i>), Error<R>> {
    let start = input.clone();
    if input.match_string("\n") {
        let span = start.span(&input);
        Ok((input, span))
    } else if input.match_string("\r") {
        let span = start.span(&input);
        Ok((input, span))
    } else if input.match_string("\r\n") {
        let span = start.span(&input);
        Ok((input, span))
    } else {
        Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        ))
    }
}

/// Match given string.
pub fn string<'i, R: RuleType>(
    mut input: Position<'i>,
    content: &'static str,
) -> Result<(Position<'i>, Span<'i>), Error<R>> {
    let start = input.clone();
    if input.match_string(content) {
        let span = start.span(&input);
        Ok((input, span))
    } else {
        Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        ))
    }
}

/// Match given string.
pub fn insensitive<'i, R: RuleType>(
    mut input: Position<'i>,
    content: &'static str,
) -> Result<(Position<'i>, Span<'i>), Error<R>> {
    let start = input.clone();
    if input.match_insensitive(content) {
        let span = start.span(&input);
        Ok((input, span))
    } else {
        Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        ))
    }
}

/// Skips until one of the given `strings`.
pub fn skip_until<'i, R: RuleType>(
    mut input: Position<'i>,
    strings: &[&str],
) -> Result<(Position<'i>, Span<'i>), Error<R>> {
    let start = input.clone();
    match input.skip_until(strings) {
        true => Ok((input, start.span(&input))),
        false => Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            start,
        )),
    }
}

/// Match a character in the range `[min, max]`.
/// Inclusively both below and above.
pub fn range<'i, R: RuleType>(
    mut input: Position<'i>,
    min: char,
    max: char,
) -> Result<(Position<'i>, Span<'i>, char), Error<R>> {
    let start = input.clone();
    match input.match_range(min..max) {
        true => {
            let span = start.span(&input);
            let content = span.as_str().chars().next().unwrap();
            Ok((input, span, content))
        }
        false => Err(Error::<R>::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        )),
    }
}

/// Match a part of the stack.
pub fn peek_stack_slice<'i, R: RuleType>(
    input: Position<'i>,
    start: i32,
    end: Option<i32>,
    stack: &mut Stack<Span<'i>>,
) -> Result<(Position<'i>, Span<'i>), Error<R>> {
    let range = match constrain_idxs(start, end, stack.len()) {
        Some(range) => range,
        None => {
            return Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: format!(
                        "Stack slice [{}, {:?}] out of bound [0, {}].",
                        start,
                        end,
                        stack.len()
                    ),
                },
                input,
            ))
        }
    };
    // return true if an empty sequence is requested
    if range.end <= range.start {
        return Ok((input, input.span(&input)));
    }

    let mut matching_pos = input.clone();
    let result = {
        let mut iter_b2t = stack[range].iter();
        let matcher = |span: &Span<'_>| matching_pos.match_string(span.as_str());
        iter_b2t.all(matcher)
    };
    if result {
        Ok((matching_pos, input.span(&matching_pos)))
    } else {
        Err(Error::new_from_pos(
            ErrorVariant::CustomError {
                message: format!("Does not match stack slice."),
            },
            input,
        ))
    }
}

/// Positive predicate
pub fn positive<'i, R: RuleType, N: TypedNode<'i, R>>(
    input: Position<'i>,
    stack: &mut Stack<Span<'i>>,
) -> Result<(), Error<R>> {
    let (_input, _res) = N::try_new(input, stack)?;
    Ok(())
}

/// Negative predicate
pub fn negative<'i, R: RuleType, N: TypedNode<'i, R>>(
    input: Position<'i>,
    stack: &mut Stack<Span<'i>>,
) -> Result<(), Error<R>> {
    match N::try_new(input, stack) {
        Ok(_) => Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![],
            },
            input,
        )),
        Err(_) => Ok(()),
    }
}
