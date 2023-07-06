// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Predefined tree nodes

use alloc::format;

use crate::{
    error::{Error, ErrorVariant},
    Position, RuleType, Span,
};

macro_rules! var {
    () => {
        ErrorVariant::CustomError {
            message: format!("{}", file!()),
        }
    };
}

/// Match any character.
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
        false => Err(Error::new_from_pos(var!(), input)),
    }
}

/// Match start of input.
pub fn soi<'i, R: RuleType>(input: Position<'i>) -> Result<Position<'i>, Error<R>> {
    if input.at_start() {
        Ok(input)
    } else {
        Err(Error::new_from_pos(var!(), input))
    }
}

/// Match end of input.
pub fn eoi<'i, R: RuleType>(input: Position<'i>) -> Result<Position<'i>, Error<R>> {
    if input.at_end() {
        Ok(input)
    } else {
        Err(Error::new_from_pos(var!(), input))
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
        Err(Error::new_from_pos(var!(), input))
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
        Err(Error::<R>::new_from_pos(var!(), input))
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
        false => Err(Error::<R>::new_from_pos(var!(), input)),
    }
}

#[cfg(test)]
mod tests {
    use crate::Position;
    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
    enum Rule {}

    #[test]
    fn string() {
        let input = "x";
        let input = Position::from_start(input);
        let _res = super::string::<Rule>(input, "x").unwrap();
    }
}
