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
use std::eprintln;

use alloc::{borrow::ToOwned, format, string::String, vec, vec::Vec};

use crate::{
    error::{Error, ErrorVariant},
    parser_state::constrain_idxs,
    Debug, Position, RuleType, Span, Stack,
};

use super::{
    typed_node::NeverFailedTypedNode, wrapper::StringWrapper, ParsableTypedNode, SubRule, TypedNode,
};

const DEBUG_LOG: bool = false;

/// Match given string.
pub struct Str<'i, R: RuleType, T: StringWrapper> {
    _phantom: PhantomData<(&'i R, &'i T)>,
}
impl<'i, R: RuleType, T: StringWrapper> StringWrapper for Str<'i, R, T> {
    const CONTENT: &'static str = T::CONTENT;
}
impl<'i, R: RuleType, T: StringWrapper> TypedNode<'i, R> for Str<'i, R, T> {
    fn try_parse_with<const _A: bool>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        if input.match_string(Self::CONTENT) {
            Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            ))
        } else {
            Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: format!("Expected exact \"{}\"", Self::CONTENT),
                },
                input,
            ))
        }
    }
}
impl<'i, R: RuleType, T: StringWrapper> Debug for Str<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Str").finish()
    }
}

/// Match given string case insensitively.
pub struct Insens<'i, R: RuleType, T: StringWrapper> {
    /// Matched content.
    pub content: &'i str,
    _phantom: PhantomData<(&'i R, &'i T)>,
}
impl<'i, R: RuleType, T: StringWrapper> StringWrapper for Insens<'i, R, T> {
    const CONTENT: &'static str = T::CONTENT;
}
impl<'i, R: RuleType, T: StringWrapper> TypedNode<'i, R> for Insens<'i, R, T> {
    fn try_parse_with<const _A: bool>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let start = input.clone();
        if input.match_insensitive(Self::CONTENT) {
            let span = start.span(&input);
            Ok((
                input,
                Self {
                    content: span.as_str(),
                    _phantom: PhantomData,
                },
            ))
        } else {
            Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: format!("Expected insensitive \"{}\"", Self::CONTENT),
                },
                input,
            ))
        }
    }
}
impl<'i, R: RuleType, T: StringWrapper> Debug for Insens<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Insens").finish()
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
pub struct Range<'i, R: RuleType, const MIN: char, const MAX: char> {
    /// Matched character.
    pub content: char,
    _phantom: PhantomData<&'i R>,
}

impl<'i, R: RuleType, const MIN: char, const MAX: char> TypedNode<'i, R>
    for Range<'i, R, MIN, MAX>
{
    fn try_parse_with<const _A: bool>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let start = input.clone();
        match input.match_range(MIN..MAX) {
            true => {
                let span = start.span(&input);
                let content = span.as_str().chars().next().unwrap();
                Ok((
                    input,
                    Self {
                        content,
                        _phantom: PhantomData,
                    },
                ))
            }
            false => Err(Error::<R>::new_from_pos(
                ErrorVariant::CustomError {
                    message: format!("Character in range '{}'..'{}' not found.", MIN, MAX),
                },
                input,
            )),
        }
    }
}

impl<'i, R: RuleType, const MIN: char, const MAX: char> Debug for Range<'i, R, MIN, MAX> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Range")
            .field("content", &self.content)
            .finish()
    }
}

/// Match a part of the stack.
fn peek_stack_slice<'i, R: RuleType>(
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

/// Positive predicate.
pub struct Positive<'i, R: RuleType, N: TypedNode<'i, R>> {
    _phantom: PhantomData<(&'i R, &'i N)>,
}
impl<'i, R: RuleType, N: TypedNode<'i, R>> TypedNode<'i, R> for Positive<'i, R, N> {
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        match N::try_parse_with::<ATOMIC>(input, stack) {
            Ok((_input, _res)) => Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            )),
            Err(err) => Err(Error::<R>::new_from_pos(
                ErrorVariant::CustomError {
                    message: format!("Positive predicate failed:\n{}", stack_error(err)),
                },
                input,
            )),
        }
    }
}
impl<'i, R: RuleType, N: TypedNode<'i, R>> Debug for Positive<'i, R, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Positive").finish()
    }
}

/// Negative predicate.
pub struct Negative<'i, R: RuleType, N: TypedNode<'i, R>> {
    _phantom: PhantomData<(&'i R, &'i N)>,
}
impl<'i, R: RuleType, N: TypedNode<'i, R>> TypedNode<'i, R> for Negative<'i, R, N> {
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        match N::try_parse_with::<ATOMIC>(input, stack) {
            Ok(_) => Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: format!("Unexpected {}.", type_name::<N>()),
                },
                input,
            )),
            Err(_) => Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            )),
        }
    }
}
impl<'i, R: RuleType, N: TypedNode<'i, R>> Debug for Negative<'i, R, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Negative").finish()
    }
}

/// Match any character.
#[derive(Debug)]
pub struct ANY<'i> {
    /// Pair span.
    pub span: Span<'i>,
    /// Matched character.
    pub content: char,
}
impl<'i, R: RuleType> TypedNode<'i, R> for ANY<'i> {
    #[inline]
    fn try_parse_with<const _A: bool>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let original_input = input.clone();
        let mut c: char = ' ';
        match input.match_char_by(|ch| {
            c = ch;
            true
        }) {
            true => {
                let span = original_input.span(&input);
                Ok((input, Self { span, content: c }))
            }
            false => Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Expected any character, but got EOI.".to_owned(),
                },
                input,
            )),
        }
    }
}

/// Match start of input.
pub struct SOI<'i> {
    _phantom: PhantomData<&'i str>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for SOI<'i> {
    #[inline]
    fn try_parse_with<const _A: bool>(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        if input.at_start() {
            Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            ))
        } else {
            Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Not at the start of input.".to_owned(),
                },
                input,
            ))
        }
    }
}
impl<'i> Debug for SOI<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SOI").finish()
    }
}

/// Match end of input.
pub struct EOI<'i> {
    _phantom: PhantomData<&'i str>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for EOI<'i> {
    #[inline]
    fn try_parse_with<const _A: bool>(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        if input.at_end() {
            Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            ))
        } else {
            Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Not at the end of input.".to_owned(),
                },
                input,
            ))
        }
    }
}
impl<'i> Debug for EOI<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EOI").finish()
    }
}

/// Match a new line.
#[derive(Debug)]
pub struct NEWLINE<'i> {
    /// Pair span.
    pub span: Span<'i>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for NEWLINE<'i> {
    #[inline]
    fn try_parse_with<const _A: bool>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let start = input.clone();
        if input.match_string("\r\n") {
            let span = start.span(&input);
            Ok((input, Self { span }))
        } else if input.match_string("\n") {
            let span = start.span(&input);
            Ok((input, Self { span }))
        } else if input.match_string("\r") {
            let span = start.span(&input);
            Ok((input, Self { span }))
        } else {
            Err(Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Expected NEWLINE.".to_owned(),
                },
                input,
            ))
        }
    }
}

/// Peek all in stack.
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub struct PEEK_ALL<'i> {
    /// Pair span.
    pub span: Span<'i>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for PEEK_ALL<'i> {
    #[inline]
    fn try_parse_with<const _A: bool>(
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
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        match T::try_parse_with::<ATOMIC>(input, stack) {
            Ok((input, inner)) => Ok((
                input,
                Self {
                    content: Some(inner),
                    _phantom: PhantomData,
                },
            )),
            Err(_err) => {
                if DEBUG_LOG {
                    eprintln!("Match nothing because of following error:\n{}", _err);
                }
                Ok((
                    input,
                    Self {
                        content: None,
                        _phantom: PhantomData,
                    },
                ))
            }
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

/// Ignore comments or white spaces if there is any.
/// Never fail.
pub struct Ign<'i, R: RuleType, COMMENT: TypedNode<'i, R>, WHITESPACE: TypedNode<'i, R>> {
    _phantom: PhantomData<(&'i R, &'i COMMENT, &'i WHITESPACE)>,
}

impl<'i, R: RuleType, COMMENT: TypedNode<'i, R>, WHITESPACE: TypedNode<'i, R>>
    NeverFailedTypedNode<'i> for Ign<'i, R, COMMENT, WHITESPACE>
{
    #[inline]
    fn parse_with<const ATOMIC: bool>(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> (Position<'i>, Self) {
        if ATOMIC {
            return (
                input,
                Self {
                    _phantom: PhantomData,
                },
            );
        }
        let mut flag = true;
        while flag {
            flag = false;
            while let Ok((remained, _)) = WHITESPACE::try_parse_with::<true>(input, stack) {
                input = remained;
                flag = true;
            }
            while let Ok((remained, _)) = COMMENT::try_parse_with::<true>(input, stack) {
                input = remained;
                flag = true;
            }
        }
        (
            input,
            Self {
                _phantom: PhantomData,
            },
        )
    }
}
impl<'i, R: RuleType, COMMENT: TypedNode<'i, R>, WHITESPACE: TypedNode<'i, R>> TypedNode<'i, R>
    for Ign<'i, R, COMMENT, WHITESPACE>
{
    #[inline]
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        Ok(Self::parse_with::<ATOMIC>(input, stack))
    }
}
impl<'i, R: RuleType, COMMENT: TypedNode<'i, R>, WHITESPACE: TypedNode<'i, R>> Debug
    for Ign<'i, R, COMMENT, WHITESPACE>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ign").finish()
    }
}

/// Repeatably match `T`.
pub struct Rep<'i, R: RuleType, T: TypedNode<'i, R>, IGNORED: NeverFailedTypedNode<'i>> {
    /// Matched pairs.
    pub content: Vec<T>,
    _phantom: PhantomData<(&'i R, &'i IGNORED)>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>, IGNORED: NeverFailedTypedNode<'i>> TypedNode<'i, R>
    for Rep<'i, R, T, IGNORED>
{
    #[inline]
    fn try_parse_with<const ATOMIC: bool>(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let mut vec = Vec::<T>::new();

        {
            let mut i: usize = 0;
            loop {
                if !ATOMIC && i != 0 {
                    let (next, _) = IGNORED::parse_with::<false>(input, stack);
                    input = next;
                }
                match T::try_parse_with::<ATOMIC>(input, stack) {
                    Ok((next, elem)) => {
                        input = next;
                        vec.push(elem);
                    }
                    Err(_err) => {
                        if DEBUG_LOG {
                            eprintln!(
                                "Repetition of {} stopped on following error (matched {} times):\n{}",
                                ::core::any::type_name::<T>(),
                                i,
                                _err
                            );
                        }
                        break;
                    }
                }
                i += 1;
                if i > 1024 {
                    return Err(Error::<R>::new_from_pos(
                        ErrorVariant::CustomError {
                            message: "Repeated too many times.".to_owned(),
                        },
                        input,
                    ));
                }
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
impl<'i, R: RuleType, T: TypedNode<'i, R>, IGNORED: NeverFailedTypedNode<'i>> Debug
    for Rep<'i, R, T, IGNORED>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rep")
            .field("content", &self.content)
            .finish()
    }
}

/// Drops the top of the stack.
pub struct DROP<'i> {
    _phantom: PhantomData<&'i str>,
}

impl<'i, R: RuleType> TypedNode<'i, R> for DROP<'i> {
    #[inline]
    fn try_parse_with<const _A: bool>(
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

/// Boxed node for `T`.
pub struct Box<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Boxed content.
    pub content: ::alloc::boxed::Box<T>,
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Box<'i, R, T> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let (input, res) = T::try_parse_with::<ATOMIC>(input, stack)?;
        Ok((
            input,
            Self {
                content: ::alloc::boxed::Box::new(res),
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> Debug for Box<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.content.fmt(f)
    }
}
impl<'i, R: RuleType, T: SubRule<R> + TypedNode<'i, R>> SubRule<R> for Box<'i, R, T> {
    const RULE: R = T::RULE;
}

/// Restore on error.
pub struct Restorable<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Matched content.
    pub content: Option<T>,
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Restorable<'i, R, T> {
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        stack.snapshot();
        match T::try_parse_with::<ATOMIC>(input, stack) {
            Ok((input, res)) => {
                stack.clear_snapshot();
                Ok((
                    input,
                    Self {
                        content: Some(res),
                        _phantom: PhantomData,
                    },
                ))
            }
            Err(err) => {
                stack.restore();
                Err(err)
            }
        }
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> Debug for Restorable<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.content.fmt(f)
    }
}

/// Always fail.
pub struct AlwaysFail<'i> {
    _phantom: PhantomData<&'i ()>,
}
/// A trait that only `AlwaysFail` implements.
pub trait AlwaysFailed: Debug {}
impl<'i> AlwaysFailed for AlwaysFail<'i> {}
impl<'i, R: RuleType, T: AlwaysFailed + SubRule<R>> TypedNode<'i, R> for T {
    #[inline]
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        Err(Error::new_from_pos(
            ErrorVariant::ParsingError {
                positives: vec![],
                negatives: vec![T::RULE],
            },
            input,
        ))
    }
}
impl<'i> Debug for AlwaysFail<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AlwaysFail").finish()
    }
}

/// Force inner token to be atomic
pub struct Atomic<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Matched content.
    pub content: T,
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Atomic<'i, R, T> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let (input, res) = T::try_parse_with::<true>(input, stack)?;
        Ok((
            input,
            Self {
                content: res,
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> Debug for Atomic<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.content.fmt(f)
    }
}

/// Force inner token to be atomic
pub struct NonAtomic<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Matched content.
    pub content: T,
    _phantom: PhantomData<(&'i R, &'i T)>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for NonAtomic<'i, R, T> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>> {
        let (input, res) = T::try_parse_with::<false>(input, stack)?;
        Ok((
            input,
            Self {
                content: res,
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> Debug for NonAtomic<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.content.fmt(f)
    }
}

/// Stack an error into a string.
pub fn stack_error<R: RuleType>(error: Error<R>) -> String {
    let s = format!("{}", error);
    s.split_terminator('\n')
        .map(|line| format!("    {}", line))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Stack errors into a string.
pub fn stack_errors<R: RuleType>(errors: Vec<Error<R>>) -> String {
    let messages: Vec<_> = errors.into_iter().map(stack_error).collect();
    let message = messages.join("\n    --------------------\n");
    message
}

#[cfg(test)]
mod tests {

    use super::super::{typed_node::SubRule, ParsableTypedNode, StringStorage};

    use super::*;

    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    enum Rule {
        Foo,
        RepFoo,
        WHITESPACE,
        COMMENT,
    }

    struct Foo;
    impl StringWrapper for Foo {
        const CONTENT: &'static str = "foo";
    }
    impl SubRule<Rule> for Foo {
        const RULE: Rule = Rule::Foo;
    }

    type WHITESPACE<'i> = Range<'i, Rule, ' ', ' '>;
    impl<'i> SubRule<Rule> for WHITESPACE<'i> {
        const RULE: Rule = Rule::WHITESPACE;
    }
    type COMMENT<'i> = Range<'i, Rule, '\t', '\t'>;
    impl<'i> SubRule<Rule> for COMMENT<'i> {
        const RULE: Rule = Rule::COMMENT;
    }
    type StrFoo<'i> = Str<'i, Rule, Foo>;
    impl<'i> SubRule<Rule> for StrFoo<'i> {
        const RULE: Rule = Rule::Foo;
    }
    #[test]
    fn string() {
        assert_eq!(StrFoo::CONTENT, Foo::CONTENT);
        let s = StrFoo::parse("foo").unwrap();
        assert_eq!(s.get_content(), "foo");
    }
    #[test]
    fn range() {
        WHITESPACE::parse(" ").unwrap();
        COMMENT::parse("\t").unwrap();
    }
    type Ignore<'i> = Ign<'i, Rule, COMMENT<'i>, WHITESPACE<'i>>;
    impl<'i> SubRule<Rule> for Ignore<'i> {
        const RULE: Rule = Rule::RepFoo;
    }
    #[test]
    fn ignore() {
        Ignore::parse(" \t  ").unwrap();
    }

    type R<'i> = Rep<'i, Rule, Str<'i, Rule, Foo>, Ignore<'i>>;
    impl<'i> SubRule<Rule> for R<'i> {
        const RULE: Rule = Rule::RepFoo;
    }
    #[test]
    fn repetition() {
        R::parse("foofoofoo").unwrap();
        R::parse("foo foo foo").unwrap();
        R::parse("foo foo\tfoo").unwrap();
    }
}
