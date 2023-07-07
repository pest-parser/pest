// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Predefined tree nodes

use core::{any::type_name, fmt, marker::PhantomData};

use alloc::{borrow::ToOwned, format, vec::Vec};

use crate::{
    error::{Error, ErrorVariant},
    parser_state::constrain_idxs,
    Debug, Position, RuleType, Span, Stack,
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
            ErrorVariant::CustomError {
                message: "Expected any character, but got EOI.".to_owned(),
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
            ErrorVariant::CustomError {
                message: "Not at the start of input.".to_owned(),
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
            ErrorVariant::CustomError {
                message: "Not at the end of input.".to_owned(),
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
            ErrorVariant::CustomError {
                message: "Expected NEWLINE.".to_owned(),
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
            ErrorVariant::CustomError {
                message: format!("Expected exact \"{}\"", content),
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
            ErrorVariant::CustomError {
                message: format!("Expected insensitive \"{}\"", content),
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
            ErrorVariant::CustomError {
                message: format!("{:?} not found.", strings),
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
            ErrorVariant::CustomError {
                message: format!("Character in range '{}'..'{}' not found.", min, max),
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
            ErrorVariant::CustomError {
                message: format!("Unexpected {}.", type_name::<N>()),
            },
            input,
        )),
        Err(_) => Ok(()),
    }
}

/// Match any character
#[derive(Debug)]
pub struct ANY<'i> {
    /// Pair span
    pub span: Span<'i>,
    /// Matched character
    pub content: char,
}
impl<'i, R: RuleType> TypedNode<'i, R> for ANY<'i> {
    #[inline]
    fn try_new(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let (input, span, content) = any(input)?;
        Ok((input, Self { span, content }))
    }
}

/// Match start of input
pub struct SOI<'i> {
    _phantom: PhantomData<&'i str>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for SOI<'i> {
    #[inline]
    fn try_new(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let input = soi(input)?;
        Ok((
            input,
            Self {
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i> Debug for SOI<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SOI").finish()
    }
}

/// Match end of input
pub struct EOI<'i> {
    _phantom: PhantomData<&'i str>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for EOI<'i> {
    #[inline]
    fn try_new(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let input = eoi(input)?;
        Ok((
            input,
            Self {
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i> Debug for EOI<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EOI").finish()
    }
}

/// Match a new line
#[derive(Debug)]
pub struct NEWLINE<'i> {
    /// Pair span
    pub span: Span<'i>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for NEWLINE<'i> {
    #[inline]
    fn try_new(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let (input, span) = new_line(input)?;
        Ok((input, Self { span }))
    }
}

/// Peek all in stack
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub struct PEEK_ALL<'i> {
    /// Pair span
    pub span: Span<'i>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for PEEK_ALL<'i> {
    #[inline]
    fn try_new(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let (input, span) = peek_stack_slice(input, 0, None, stack)?;
        Ok((input, Self { span }))
    }
}

/// Optionally match `T`.
pub struct Opt<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Matched content.
    pub content: Option<T>,
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Opt<'i, R, T> {
    #[inline]
    fn try_new(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        match T::try_new(input, stack) {
            Ok((input, inner)) => Ok((
                input,
                Self {
                    content: Some(inner),
                    _phantom: PhantomData,
                },
            )),
            Err(_) => Ok((
                input,
                Self {
                    content: None,
                    _phantom: PhantomData,
                },
            )),
        }
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> Debug for Opt<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Opt")
            .field("content", &self.content)
            .finish()
    }
}

/// Repeatably match `T`
pub struct Rep<
    'i,
    R: RuleType,
    T: TypedNode<'i, R>,
    const INNER_SPACES: bool,
    COMMENT: TypedNode<'i, R>,
    WHITESPACE: TypedNode<'i, R>,
> {
    /// Matched pairs
    pub content: Vec<T>,
    _phantom: PhantomData<(&'i R, &'i COMMENT, &'i WHITESPACE)>,
}
impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R>,
        const INNER_SPACES: bool,
        COMMENT: TypedNode<'i, R>,
        WHITESPACE: TypedNode<'i, R>,
    > TypedNode<'i, R> for Rep<'i, R, T, INNER_SPACES, COMMENT, WHITESPACE>
{
    #[inline]
    fn try_new(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let mut vec = Vec::<T>::new();
        let mut input = input;
        if INNER_SPACES {
            let mut i = 0;
            loop {
                if i != 0 {
                    let mut flag = false;
                    while flag {
                        flag = false;
                        while let Ok((remained, _)) = WHITESPACE::try_new(input, stack) {
                            input = remained;
                            flag = true;
                        }
                        while let Ok((remained, _)) = COMMENT::try_new(input, stack) {
                            input = remained;
                            flag = true;
                        }
                    }
                }
                if let Ok((next, elem)) = T::try_new(input, stack) {
                    input = next;
                    vec.push(elem);
                } else {
                    break;
                }
                i += 1;
            }
        } else {
            while let Ok((next, elem)) = T::try_new(input, stack) {
                input = next;
                vec.push(elem);
            }
        }
        Ok((
            input,
            Self {
                content: vec,
                _phantom: PhantomData,
            },
        ))
    }
}
impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R>,
        const INNER_SPACES: bool,
        COMMENT: TypedNode<'i, R>,
        WHITESPACE: TypedNode<'i, R>,
    > Debug for Rep<'i, R, T, INNER_SPACES, COMMENT, WHITESPACE>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rep")
            .field("content", &self.content)
            .finish()
    }
}

/// Drops the top of the stack
pub struct DROP<'i> {
    _phantom: PhantomData<&'i str>,
}

impl<'i, R: RuleType> TypedNode<'i, R> for DROP<'i> {
    #[inline]
    fn try_new(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        match stack.pop() {
            Some(_) => Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            )),
            None => Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Nothing to drop.".to_owned(),
                },
                input,
            )),
        }
    }
}
impl<'i> Debug for DROP<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Drop").finish()
    }
}
