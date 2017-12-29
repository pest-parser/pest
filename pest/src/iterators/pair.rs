// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use super::pairs::{self, Pairs};
use super::queueable_token::QueueableToken;
use super::token_iterator::{self, TokenIterator};
use super::super::inputs::{Input, span, Span};
use super::super::RuleType;

/// A `struct` containing a matching pair of `Token`s and everything between them.
///
/// A matching `Token` pair is formed by a `Token::Start` and a subsequent `Token::End` with the
/// same `Rule`, with the condition that all `Token`s between them can form such pairs as well.
/// This is similar to the [brace matching problem](https://en.wikipedia.org/wiki/Brace_matching) in
/// editors.
#[derive(Clone)]
pub struct Pair<R, I: Input> {
    queue: Rc<Vec<QueueableToken<R>>>,
    input: Rc<I>,
    start: usize
}

pub fn new<R: RuleType, I: Input>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: Rc<I>,
    start: usize
) -> Pair<R, I> {
    Pair {
        queue,
        input,
        start
    }
}

impl<R: RuleType, I: Input> Pair<R, I> {
    /// Returns the `Rule` of the `Pair`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("".to_owned()));
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
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::ab ...
    /// #     state.rule(Rule::ab, pos, |_, p| p.match_string("ab"))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.as_str(), "ab");
    /// ```
    #[inline]
    pub fn as_str(&self) -> &str {
        let start = self.pos(self.start);
        let end = self.pos(self.pair());

        unsafe { self.input.slice(start, end) }
    }

    /// Returns the `Span` defined by the `Pair`, consuming it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = Rc::new(StringInput::new("ab".to_owned()));
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::ab ...
    /// #     state.rule(Rule::ab, pos, |_, p| p.match_string("ab"))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.into_span().as_str(), "ab");
    /// ```
    #[inline]
    pub fn into_span(self) -> Span<I> {
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
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("".to_owned()));
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().next().unwrap();
    ///
    /// assert!(pair.into_inner().next().is_none());
    /// ```
    #[inline]
    pub fn into_inner(self) -> Pairs<R, I> {
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
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("".to_owned()));
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().next().unwrap();
    /// let tokens: Vec<_> = pair.tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn tokens(self) -> TokenIterator<R, I> {
        let end = self.pair();

        token_iterator::new(
            self.queue,
            self.input,
            self.start,
            end + 1
        )
    }

    /// Return the line and column number of the token in the input stream.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("a".to_owned()));
    /// let pair = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(unsafe { pair.line_col() }, (1,1));
    /// ```
    pub unsafe fn line_col(&self) -> (usize, usize) {
        self.input.line_col(self.pos(0))
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

impl<R: RuleType, I: Input> fmt::Debug for Pair<R, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pair {{ rule: {:?}, span: {:?}, inner: {:?} }}",
               self.as_rule(), self.clone().into_span(), self.clone().into_inner())
    }
}

impl<R: RuleType, I: Input> fmt::Display for Pair<R, I> {
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

impl<R: PartialEq, I: Input> PartialEq for Pair<R, I> {
    fn eq(&self, other: &Pair<R, I>) -> bool {
        Rc::ptr_eq(&self.queue, &other.queue) && Rc::ptr_eq(&self.input, &other.input) &&
        self.start == other.start
    }
}

impl<R: Eq, I: Input> Eq for Pair<R, I> {}

impl<R: Hash, I: Input> Hash for Pair<R, I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.queue as *const Vec<QueueableToken<R>>).hash(state);
        (&*self.input as *const I).hash(state);
        self.start.hash(state);
    }
}
