// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use alloc::rc::Rc;
use alloc::vec::Vec;
use core::fmt;

use super::pair::{self, Pair};
use super::queueable_token::QueueableToken;
use super::tokens::{self, Tokens};
use crate::RuleType;

/// An iterator over [`Pair`]s. It is created by [`Pairs::flatten`].
///
/// [`Pair`]: struct.Pair.html
/// [`Pairs::flatten`]: struct.Pairs.html#method.flatten
pub struct FlatPairs<'i, R> {
    /// # Safety
    ///
    /// All `QueueableToken`s' `input_pos` must be valid character boundary indices into `input`.
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    start: usize,
    end: usize,
}

/// # Safety
///
/// All `QueueableToken`s' `input_pos` must be valid character boundary indices into `input`.
pub unsafe fn new<R: RuleType>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &str,
    start: usize,
    end: usize,
) -> FlatPairs<'_, R> {
    FlatPairs {
        queue,
        input,
        start,
        end,
    }
}

impl<'i, R: RuleType> FlatPairs<'i, R> {
    /// Returns the `Tokens` for these pairs.
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
    /// let pairs = pest::state(input, |state| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, |s| Ok(s))
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.flatten().tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn tokens(self) -> Tokens<'i, R> {
        tokens::new(self.queue, self.input, self.start, self.end)
    }

    fn next_start(&mut self) {
        self.start += 1;

        while self.start < self.end && !self.is_start(self.start) {
            self.start += 1;
        }
    }

    fn next_start_from_end(&mut self) {
        self.end -= 1;

        while self.end >= self.start && !self.is_start(self.end) {
            self.end -= 1;
        }
    }

    fn is_start(&self, index: usize) -> bool {
        match self.queue[index] {
            QueueableToken::Start { .. } => true,
            QueueableToken::End { .. } => false,
        }
    }
}

impl<'i, R: RuleType> Iterator for FlatPairs<'i, R> {
    type Item = Pair<'i, R>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let pair = unsafe { pair::new(Rc::clone(&self.queue), self.input, self.start) };
        self.next_start();

        Some(pair)
    }
}

impl<'i, R: RuleType> DoubleEndedIterator for FlatPairs<'i, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end <= self.start {
            return None;
        }

        self.next_start_from_end();

        let pair = unsafe { pair::new(Rc::clone(&self.queue), self.input, self.end) };

        Some(pair)
    }
}

impl<'i, R: RuleType> fmt::Debug for FlatPairs<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FlatPairs")
            .field("pairs", &self.clone().collect::<Vec<_>>())
            .finish()
    }
}

impl<'i, R: Clone> Clone for FlatPairs<'i, R> {
    fn clone(&self) -> FlatPairs<'i, R> {
        FlatPairs {
            queue: Rc::clone(&self.queue),
            input: self.input,
            start: self.start,
            end: self.end,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::macros::tests::*;
    use super::super::super::Parser;
    use alloc::vec;
    use alloc::vec::Vec;

    #[test]
    fn iter_for_flat_pairs() {
        let pairs = AbcParser::parse(Rule::a, "abcde").unwrap();

        assert_eq!(
            pairs.flatten().map(|p| p.as_rule()).collect::<Vec<Rule>>(),
            vec![Rule::a, Rule::b, Rule::c]
        );
    }

    #[test]
    fn double_ended_iter_for_flat_pairs() {
        let pairs = AbcParser::parse(Rule::a, "abcde").unwrap();
        assert_eq!(
            pairs
                .flatten()
                .rev()
                .map(|p| p.as_rule())
                .collect::<Vec<Rule>>(),
            vec![Rule::c, Rule::b, Rule::a]
        );
    }

    #[test]
    fn test_line_col() {
        let mut pairs = AbcParser::parse(Rule::a, "abcNe\nabcde").unwrap().flatten();

        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_str(), "abc");
        assert_eq!(pair.line_col(), (1, 1));
        assert_eq!(pair.line_col(), pair.as_span().start_pos().line_col());

        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_str(), "b");
        assert_eq!(pair.line_col(), (1, 2));
        assert_eq!(pair.line_col(), pair.as_span().start_pos().line_col());

        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_str(), "e");
        assert_eq!(pair.line_col(), (1, 5));
        assert_eq!(pair.line_col(), pair.as_span().start_pos().line_col());
    }
}
