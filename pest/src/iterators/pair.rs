// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::ptr;
use std::rc::Rc;

use super::pairs::{self, Pairs};
use super::queueable_token::QueueableToken;
use super::token_iterator::{self, TokenIterator};
use super::super::inputs::{span, Span};
use super::super::RuleType;

/// A `struct` containing a matching pair of `Token`s and everything between them.
///
/// A matching `Token` pair is formed by a `Token::Start` and a subsequent `Token::End` with the
/// same `Rule`, with the condition that all `Token`s between them can form such pairs as well.
/// This is similar to the [brace matching problem](https://en.wikipedia.org/wiki/Brace_matching) in
/// editors.
#[derive(Clone)]
pub struct Pair<'i, R> {
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    start: usize,
    __phantom: ::std::marker::PhantomData<&'i str>
}

pub fn new<'i, R: RuleType>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    start: usize
) -> Pair<'i, R> {
    Pair {
        queue,
        input,
        start,
        __phantom: ::std::marker::PhantomData
    }
}

impl<'i, R: RuleType> Pair<'i, R> {
    /// Returns the `Rule` of the `Pair`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "";
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.as_rule(), Rule::a);
    /// ```
    #[inline]
    pub fn as_rule(&self) -> R {
        match self.queue[self.pair()] {
            QueueableToken::End { rule, .. } => rule,
            _ => unreachable!()
        }
    }

    /// Captures a `&str` slice from the `Input` defined by the token `Pair`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::ab ...
    /// #     state.rule(Rule::ab, pos, |_, p| p.match_string("ab"))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.as_str(), "ab");
    /// ```
    #[inline]
    pub fn as_str(&self) -> &'i str {
        let start = self.pos(self.start);
        let end = self.pos(self.pair());

        unsafe { self.input.slice_unchecked(start, end) }
    }

    /// Returns the `Span` defined by the `Pair`, consuming it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::ab ...
    /// #     state.rule(Rule::ab, pos, |_, p| p.match_string("ab"))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.into_span().as_str(), "ab");
    /// ```
    #[inline]
    pub fn into_span(self) -> Span<'i> {
        let start = self.pos(self.start);
        let end = self.pos(self.pair());

        span::new(self.input, start, end)
    }

    /// Returns the inner `Pairs` between the `Pair`, consuming it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "";
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().next().unwrap();
    ///
    /// assert!(pair.into_inner().next().is_none());
    /// ```
    #[inline]
    pub fn into_inner(self) -> Pairs<'i, R> {
        let pair = self.pair();

        pairs::new(
            self.queue,
            self.input,
            self.start + 1,
            pair - 1
        )
    }

    /// Converts the `Pair` into a `TokenIterator`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "";
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().next().unwrap();
    /// let tokens: Vec<_> = pair.tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn tokens(self) -> TokenIterator<'i, R> {
        let end = self.pair();

        token_iterator::new(
            self.queue,
            self.input,
            self.start,
            end + 1
        )
    }

    fn pair(&self) -> usize {
        match self.queue[self.start] {
            QueueableToken::Start { pair, .. } => pair,
            _ => unreachable!()
        }
    }

    fn pos(&self, index: usize) -> usize {
        match self.queue[index] {
            QueueableToken::Start { pos, .. } | QueueableToken::End { pos, .. } => pos
        }
    }
}

impl<'i, R: RuleType> fmt::Debug for Pair<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pair {{ rule: {:?}, span: {:?}, inner: {:?} }}",
               self.as_rule(), self.clone().into_span(), self.clone().into_inner())
    }
}

impl<'i, R: RuleType> fmt::Display for Pair<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rule = self.as_rule();
        let start = self.pos(self.start);
        let end = self.pos(self.pair());
        let mut pairs = self.clone().into_inner().peekable();

        if pairs.peek().is_none() {
            write!(f, "{:?}({}, {})", rule, start, end)
        } else {
            write!(f, "{:?}({}, {}, [{}])", rule, start, end,
                   pairs.map(|pair| format!("{}", pair))
                        .collect::<Vec<_>>()
                        .join(", "))
        }
    }
}

impl<'i, R: PartialEq> PartialEq for Pair<'i, R> {
    fn eq(&self, other: &Pair<'i, R>) -> bool {
        Rc::ptr_eq(&self.queue, &other.queue) && ptr::eq(self.input, other.input) &&
        self.start == other.start
    }
}

impl<'i, R: Eq> Eq for Pair<'i, R> {}

impl<'i, R: Hash> Hash for Pair<'i, R> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.queue as *const Vec<QueueableToken<R>>).hash(state);
        (&*self.input as *const str).hash(state);
        self.start.hash(state);
    }
}
