// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::rc::Rc;

use super::pair::{self, Pair};
use super::queueable_token::QueueableToken;
use super::token_iterator::{self, TokenIterator};
use super::super::inputs::StrInput;
use super::super::RuleType;

/// A `struct` containing `Pairs`. It is created by
/// [`Pairs::flatten`](struct.Pairs.html#method.flatten).
pub struct FlatPairs<'i, R> {
    queue: Rc<Vec<QueueableToken<R>>>,
    input: Rc<StrInput<'i>>,
    start: usize,
    end: usize,
    __phantom: ::std::marker::PhantomData<&'i str>
}

pub fn new<'i, R: RuleType>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: Rc<StrInput<'i>>,
    start: usize,
    end: usize
) -> FlatPairs<'i, R> {
    FlatPairs {
        queue,
        input,
        start,
        end,
        __phantom: ::std::marker::PhantomData
    }
}

impl<'i, R: RuleType> FlatPairs<'i, R> {
    /// Converts the `FlatPairs` into a `TokenIterator`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # use pest::inputs::StrInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StrInput::new(""));
    /// let pairs = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.flatten().into_iter().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn into_iter(self) -> TokenIterator<'i, R> {
        token_iterator::new(
            self.queue,
            self.input,
            self.start,
            self.end
        )
    }

    fn next_start(&mut self) {
        self.start += 1;

        while self.start < self.end && !self.is_start() {
            self.start += 1;
        }
    }

    fn is_start(&self) -> bool {
        match self.queue[self.start] {
            QueueableToken::Start { .. } => true,
            QueueableToken::End { .. } => false
        }
    }
}

impl<'i, R: RuleType> Iterator for FlatPairs<'i, R> {
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

        self.next_start();

        Some(pair)
    }
}

impl<'i, R: RuleType> fmt::Debug for FlatPairs<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FlatPairs {{ pairs: {:?} }}", self.clone().collect::<Vec<_>>())
    }
}

impl<'i, R: Clone> Clone for FlatPairs<'i, R> {
    fn clone(&self) -> FlatPairs<'i, R> {
        FlatPairs {
            queue: self.queue.clone(),
            input: self.input.clone(),
            start: self.start,
            end: self.end,
            __phantom: ::std::marker::PhantomData,
        }
    }
}
