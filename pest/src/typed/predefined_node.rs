// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Predefined tree nodes.

use core::{fmt, marker::PhantomData, ops::Deref};

use alloc::{vec, vec::Vec};

use crate::{error::Error, parser_state::constrain_idxs, Debug, Position, RuleType, Span, Stack};

use super::{
    error::Tracker,
    typed_node::{NeverFailedTypedNode, ParsableTypedNode},
    wrapper::{RuleWrapper, StringArrayWrapper, StringWrapper, TypeWrapper},
    TypedNode,
};

/// Match given string.
pub struct Str<'i, R: RuleType, T: StringWrapper> {
    _phantom: PhantomData<(&'i R, &'i T)>,
}
impl<'i, R: RuleType, T: StringWrapper> StringWrapper for Str<'i, R, T> {
    const CONTENT: &'static str = T::CONTENT;
}
impl<'i, R: RuleType, T: StringWrapper> TypedNode<'i, R> for Str<'i, R, T> {
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        if input.match_string(Self::CONTENT) {
            Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            ))
        } else {
            Err(Tracker::new_positive(Rule::RULE, input))
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
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
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
            Err(Tracker::new_positive(Rule::RULE, input))
        }
    }
}
impl<'i, R: RuleType, T: StringWrapper> Debug for Insens<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Insens").finish()
    }
}

/// Skips until one of the given `strings`
pub struct Skip<'i, R: RuleType, Strings: StringArrayWrapper> {
    /// Skipped span.
    pub span: Span<'i>,
    _phantom: PhantomData<(&'i R, &'i Strings)>,
}
impl<'i, R: RuleType, Strings: StringArrayWrapper> TypedNode<'i, R> for Skip<'i, R, Strings> {
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let start = input.clone();
        match input.skip_until(Strings::CONTENT) {
            true => {
                let span = start.span(&input);
                Ok((
                    input,
                    Self {
                        span,
                        _phantom: PhantomData,
                    },
                ))
            }
            false => Err(Tracker::new_positive(Rule::RULE, input)),
        }
    }
}
impl<'i, R: RuleType, Strings: StringArrayWrapper> Debug for Skip<'i, R, Strings> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Skip").finish()
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
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
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
            false => Err(Tracker::new_positive(Rule::RULE, input)),
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
#[inline]
fn peek_stack_slice<'i, R: RuleType, Rule: RuleWrapper<R>>(
    input: Position<'i>,
    start: i32,
    end: Option<i32>,
    stack: &mut Stack<Span<'i>>,
) -> Result<(Position<'i>, Span<'i>), Tracker<'i, R>> {
    let range = match constrain_idxs(start, end, stack.len()) {
        Some(range) => range,
        None => return Err(Tracker::SliceOutOfBound(start, end, input)),
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
        Err(Tracker::new_positive(Rule::RULE, input))
    }
}

/// Positive predicate.
pub struct Positive<'i, R: RuleType, N: TypedNode<'i, R>> {
    _phantom: PhantomData<(&'i R, &'i N)>,
}
impl<'i, R: RuleType, N: TypedNode<'i, R>> TypedNode<'i, R> for Positive<'i, R, N> {
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        match N::try_parse_with::<ATOMIC, Rule>(input, stack) {
            Ok((_input, _res)) => Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            )),
            Err(_) => Err(Tracker::new_positive(Rule::RULE, input)),
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
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        match N::try_parse_with::<ATOMIC, Rule>(input, stack) {
            Ok(_) => Err(Tracker::new_negative(Rule::RULE, input)),
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
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
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
            false => Err(Tracker::new_positive(Rule::RULE, input)),
        }
    }
}

/// Match start of input.
pub struct SOI<'i> {
    _phantom: PhantomData<&'i str>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for SOI<'i> {
    #[inline]
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        if input.at_start() {
            Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            ))
        } else {
            Err(Tracker::new_positive(Rule::RULE, input))
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
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        if input.at_end() {
            Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            ))
        } else {
            Err(Tracker::new_positive(Rule::RULE, input))
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
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
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
            Err(Tracker::new_positive(Rule::RULE, input))
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
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (input, span) = peek_stack_slice::<R, Rule>(input, 0, None, stack)?;
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
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        match T::try_parse_with::<ATOMIC, Rule>(input, stack) {
            Ok((input, inner)) => Ok((
                input,
                Self {
                    content: Some(inner),
                    _phantom: PhantomData,
                },
            )),
            Err(_err) => Ok((
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

/// Ignore comments or white spaces if there is any.
/// Never fail.
pub struct Ign<'i, R: RuleType, COMMENT: TypedNode<'i, R>, WHITESPACE: TypedNode<'i, R>> {
    _phantom: PhantomData<(&'i R, &'i COMMENT, &'i WHITESPACE)>,
}

impl<'i, R: RuleType, COMMENT: TypedNode<'i, R>, WHITESPACE: TypedNode<'i, R>>
    NeverFailedTypedNode<'i, R> for Ign<'i, R, COMMENT, WHITESPACE>
{
    #[inline]
    fn parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
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
            while let Ok((remained, _)) = WHITESPACE::try_parse_with::<true, Rule>(input, stack) {
                input = remained;
                flag = true;
            }
            while let Ok((remained, _)) = COMMENT::try_parse_with::<true, Rule>(input, stack) {
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
    fn try_parse_with<const _ATOMIC: bool, RULE: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        Ok(Self::parse_with::<true, RULE>(input, stack))
    }
}
impl<'i, R: RuleType, COMMENT: TypedNode<'i, R>, WHITESPACE: TypedNode<'i, R>> Debug
    for Ign<'i, R, COMMENT, WHITESPACE>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ign").finish()
    }
}

/// Match a sequence of two expressions.
pub struct Seq<
    'i,
    R: RuleType,
    T1: TypedNode<'i, R>,
    T2: TypedNode<'i, R>,
    IGNORED: NeverFailedTypedNode<'i, R>,
> {
    /// Matched first expression.
    pub first: T1,
    /// Matched second expression.
    pub second: T2,
    _phantom: PhantomData<(&'i R, &'i IGNORED)>,
}
impl<
        'i,
        R: RuleType,
        T1: TypedNode<'i, R>,
        T2: TypedNode<'i, R>,
        IGNORED: NeverFailedTypedNode<'i, R>,
    > TypedNode<'i, R> for Seq<'i, R, T1, T2, IGNORED>
{
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (next, first) = T1::try_parse_with::<ATOMIC, Rule>(input, stack)?;
        input = next;
        let (next, _) = IGNORED::parse_with::<ATOMIC, Rule>(input, stack);
        input = next;
        let (next, second) = T2::try_parse_with::<ATOMIC, Rule>(input, stack)?;
        input = next;

        Ok((
            input,
            Self {
                first,
                second,
                _phantom: PhantomData,
            },
        ))
    }
}
impl<
        'i,
        R: RuleType,
        T1: TypedNode<'i, R>,
        T2: TypedNode<'i, R>,
        IGNORED: NeverFailedTypedNode<'i, R>,
    > Debug for Seq<'i, R, T1, T2, IGNORED>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Seq")
            .field("first", &self.first)
            .field("second", &self.second)
            .finish()
    }
}

/// Match either of two expressions
pub enum Choice<'i, R: RuleType, T1: TypedNode<'i, R>, T2: TypedNode<'i, R>> {
    /// Matched first expression.
    First(T1, PhantomData<&'i R>),
    /// Matched second expression.
    Second(T2, PhantomData<&'i R>),
}
impl<'i, R: RuleType, T1: TypedNode<'i, R>, T2: TypedNode<'i, R>> TypedNode<'i, R>
    for Choice<'i, R, T1, T2>
{
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        match T1::try_parse_with::<ATOMIC, Rule>(input, stack) {
            Ok((input, first)) => Ok((input, Self::First(first, PhantomData))),
            Err(first) => match T2::try_parse_with::<ATOMIC, Rule>(input, stack) {
                Ok((input, second)) => Ok((input, Self::Second(second, PhantomData))),
                Err(second) => Err(first.merge(second)),
            },
        }
    }
}
impl<'i, R: RuleType, T1: TypedNode<'i, R>, T2: TypedNode<'i, R>> Debug for Choice<'i, R, T1, T2> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::First(first, _) => f.debug_tuple("First").field(first).finish(),
            Self::Second(second, _) => f.debug_tuple("Second").field(second).finish(),
        }
    }
}

/// Repeatably match `T`.
pub struct Rep<'i, R: RuleType, T: TypedNode<'i, R>, IGNORED: NeverFailedTypedNode<'i, R>> {
    /// Matched pairs.
    pub content: Vec<T>,
    _phantom: PhantomData<(&'i R, &'i IGNORED)>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>, IGNORED: NeverFailedTypedNode<'i, R>> TypedNode<'i, R>
    for Rep<'i, R, T, IGNORED>
{
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let mut vec = Vec::<T>::new();

        {
            let mut i: usize = 0;
            loop {
                if i != 0 {
                    let (next, _) = IGNORED::parse_with::<ATOMIC, Rule>(input, stack);
                    input = next;
                }
                match T::try_parse_with::<ATOMIC, Rule>(input, stack) {
                    Ok((next, elem)) => {
                        input = next;
                        vec.push(elem);
                    }
                    Err(_err) => {
                        break;
                    }
                }
                i += 1;
                if i > 1024 {
                    return Err(Tracker::RepeatTooManyTimes(input));
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
impl<'i, R: RuleType, T: TypedNode<'i, R>, IGNORED: NeverFailedTypedNode<'i, R>> Debug
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
    fn try_parse_with<const _A: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        match stack.pop() {
            Some(_) => Ok((
                input,
                Self {
                    _phantom: PhantomData,
                },
            )),
            None => Err(Tracker::EmptyStack(input)),
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
impl<'i, R: RuleType, T: TypedNode<'i, R>> Deref for Box<'i, R, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.content.deref()
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Box<'i, R, T> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (input, res) = T::try_parse_with::<ATOMIC, Rule>(input, stack)?;
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

/// Restore on error.
pub struct Restorable<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Matched content.
    pub content: Option<T>,
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Restorable<'i, R, T> {
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        stack.snapshot();
        match T::try_parse_with::<ATOMIC, Rule>(input, stack) {
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
impl<'i, R: RuleType, T: AlwaysFailed> TypedNode<'i, R> for T {
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        Err(Tracker::new(input))
    }
}
impl<'i> Debug for AlwaysFail<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AlwaysFail").finish()
    }
}

/// Force inner token to be atomic.
pub struct Atomic<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Matched content.
    pub content: T,
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Atomic<'i, R, T> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (input, res) = T::try_parse_with::<true, Rule>(input, stack)?;
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

/// Force inner token to be atomic.
pub struct NonAtomic<'i, R: RuleType, T: TypedNode<'i, R>> {
    /// Matched content.
    pub content: T,
    _phantom: PhantomData<(&'i R, &'i T)>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for NonAtomic<'i, R, T> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (input, res) = T::try_parse_with::<false, Rule>(input, stack)?;
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

/// Match an expression and push it.
pub struct Push<'i, R: RuleType, T: TypedNode<'i, R>> {
    _phantom: PhantomData<(&'i R, &'i T)>,
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Push<'i, R, T> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (input, _) = T::try_parse_with::<ATOMIC, Rule>(input, stack)?;
        Ok((
            input,
            Self {
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> Debug for Push<'i, R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Push").finish()
    }
}

/// Match `START`..`END` of the stack.
pub struct PeekSlice2<'i, R: RuleType, const START: i32, const END: i32> {
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, const START: i32, const END: i32> TypedNode<'i, R>
    for PeekSlice2<'i, R, START, END>
{
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (input, _) = peek_stack_slice::<R, Rule>(input, START, Some(END), stack)?;
        Ok((
            input,
            Self {
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i, R: RuleType, const START: i32, const END: i32> Debug for PeekSlice2<'i, R, START, END> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PeekSlice2").finish()
    }
}

/// Match `START`..`END` of the stack.
pub struct PeekSlice1<'i, R: RuleType, const START: i32> {
    _phantom: PhantomData<&'i R>,
}
impl<'i, R: RuleType, const START: i32> TypedNode<'i, R> for PeekSlice1<'i, R, START> {
    #[inline]
    fn try_parse_with<const ATOMIC: bool, Rule: RuleWrapper<R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Tracker<'i, R>> {
        let (input, _) = peek_stack_slice::<R, Rule>(input, START, None, stack)?;
        Ok((
            input,
            Self {
                _phantom: PhantomData,
            },
        ))
    }
}
impl<'i, R: RuleType, const START: i32> Debug for PeekSlice1<'i, R, START> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PeekSlice1").finish()
    }
}

#[cfg(test)]
mod tests {

    use super::super::Storage;

    use super::*;

    macro_rules! make_rules {
        ($($ids:ident,)*) => {
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            enum Rule {
                $($ids),*
            }
            mod rule_wrappers {
                $(
                    pub struct $ids {}
                    impl super::RuleWrapper<super::Rule> for $ids {
                        const RULE:super::Rule = super::Rule::$ids;
                    }
                )*
            }
        };
    }

    make_rules! {
        Foo,
        RepFoo,
        WHITESPACE,
        COMMENT,
        EOI,
    }

    struct Foo;
    impl StringWrapper for Foo {
        const CONTENT: &'static str = "foo";
    }
    impl RuleWrapper<Rule> for Foo {
        const RULE: Rule = Rule::Foo;
    }

    type WHITESPACE<'i> = super::Rule<
        'i,
        Rule,
        rule_wrappers::WHITESPACE,
        rule_wrappers::EOI,
        Range<'i, Rule, ' ', ' '>,
    >;
    type COMMENT<'i> = super::Rule<
        'i,
        Rule,
        rule_wrappers::WHITESPACE,
        rule_wrappers::EOI,
        Range<'i, Rule, '\t', '\t'>,
    >;
    type StrFoo<'i> =
        super::Rule<'i, Rule, rule_wrappers::Foo, rule_wrappers::EOI, Str<'i, Rule, Foo>>;
    #[test]
    fn string() {
        assert_eq!(<StrFoo<'_> as TypeWrapper>::Inner::CONTENT, Foo::CONTENT);
        let s = StrFoo::parse("foo").unwrap();
        assert_eq!(s.get_content(), "foo");
    }
    #[test]
    fn range() {
        WHITESPACE::parse(" ").unwrap();
        COMMENT::parse("\t").unwrap();
    }
    type Ignore<'i> = Ign<'i, Rule, COMMENT<'i>, WHITESPACE<'i>>;
    #[test]
    fn ignore() {
        super::Rule::<Rule, rule_wrappers::RepFoo, rule_wrappers::EOI, Ignore>::parse(" \t  ")
            .unwrap();
    }

    type R<'i> = super::Rule<
        'i,
        Rule,
        rule_wrappers::RepFoo,
        rule_wrappers::EOI,
        Rep<'i, Rule, Str<'i, Rule, Foo>, Ignore<'i>>,
    >;
    #[test]
    fn repetition() {
        R::parse("foofoofoo").unwrap();
        R::parse("foo foo foo").unwrap();
        R::parse("foo foo\tfoo").unwrap();
    }
}
