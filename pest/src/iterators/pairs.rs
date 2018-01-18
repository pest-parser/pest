// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::ptr;

use super::flat_pairs::{self, FlatPairs};
use super::pair::{self, Pair};
use super::queueable_token::QueueableToken;
use super::token_iterator::{self, TokenIterator};
use super::super::RuleType;

/// A `struct` containing `Pairs`. It is created by [`pest::state`](../fn.state.html) and
/// [`Pair::into_inner`](struct.Pair.html#method.into_inner).
#[derive(Clone)]
pub struct Pairs<'i, R> {
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    start: usize,
    end: usize,
}

pub fn new<'i, R: RuleType>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    start: usize,
    end: usize
) -> Pairs<'i, R> {
    Pairs {
        queue,
        input,
        start,
        end,
    }
}

impl<'i, R: RuleType> Pairs<'i, R> {
    /// Flattens the `Pairs`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a,
    ///     b
    /// }
    ///
    /// let input = "";
    /// let pairs = pest::state(input, |state, pos| {
    ///     // generating nested Token pair with Rule::b inside Rule::a
    /// #     state.rule(Rule::a, pos, |state, pos| {
    /// #         state.rule(Rule::b, pos, |_, p| Ok(p))
    /// #     })
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.flatten().into_iter().collect();
    ///
    /// assert_eq!(tokens.len(), 4);
    /// ```
    #[inline]
    pub fn flatten(self) -> FlatPairs<'i, R> {
        flat_pairs::new(
            self.queue,
            self.input,
            self.start,
            self.end
        )
    }

    /// Converts the `Pairs` into a `TokenIterator`.
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
    /// let pairs = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn tokens(self) -> TokenIterator<'i, R> {
        token_iterator::new(
            self.queue,
            self.input,
            self.start,
            self.end
        )
    }

    fn pair(&self) -> usize {
        match self.queue[self.start] {
            QueueableToken::Start { pair, .. } => pair,
            _ => unreachable!()
        }
    }
}

impl<'i, R: RuleType> Iterator for Pairs<'i, R> {
    type Item = Pair<'i, R>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let pair = pair::new(
            self.queue.clone(),
            self.input.clone(),
            self.start
        );

        self.start = self.pair() + 1;

        Some(pair)
    }
}

impl<'i, R: RuleType> fmt::Debug for Pairs<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pairs {{ pairs: {:?} }}", self.clone().collect::<Vec<_>>())
    }
}

impl<'i, R: RuleType> fmt::Display for Pairs<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.clone()
                              .map(|pair| format!("{}", pair))
                              .collect::<Vec<_>>()
                              .join(", "))
    }
}

impl<'i, R: PartialEq> PartialEq for Pairs<'i, R> {
    fn eq(&self, other: &Pairs<'i, R>) -> bool {
        Rc::ptr_eq(&self.queue, &other.queue) && ptr::eq(self.input, other.input) &&
        self.start == other.start && self.end == other.end
    }
}

impl<'i, R: Eq> Eq for Pairs<'i, R> {}

impl<'i, R: Hash> Hash for Pairs<'i, R> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.queue as *const Vec<QueueableToken<R>>).hash(state);
        (self.input as *const str).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::macros::tests::*;
    use super::super::super::Parser;

    #[test]
    fn pairs_debug() {
        let pairs = AbcParser::parse(Rule::a, "abcde").unwrap();

        assert_eq!(
            format!("{:?}", pairs),
            "Pairs { pairs: [\
                Pair { rule: a, span: Span { start: 0, end: 3 }, inner: Pairs { pairs: [\
                    Pair { rule: b, span: Span { start: 1, end: 2 }, \
                        inner: Pairs { pairs: [] } }] } }, \
                    Pair { rule: c, span: Span { start: 4, end: 5 }, \
                        inner: Pairs { pairs: [] } }] }".to_owned()
        );
    }

    #[test]
    fn pairs_display() {
        let pairs = AbcParser::parse(Rule::a, "abcde").unwrap();

        assert_eq!(format!("{}", pairs), "[a(0, 3, [b(1, 2)]), c(4, 5)]".to_owned());
    }
}
