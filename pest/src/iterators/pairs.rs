// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::rc::Rc;

use super::flat_pairs::{self, FlatPairs};
use super::pair::{self, Pair};
use super::queueable_token::QueueableToken;
use super::token_iterator::{self, TokenIterator};
use super::super::inputs::Input;
use super::super::RuleType;

/// A `struct` containing `Pairs`. It is created by [`pest::state`](../fn.state.html) and
/// [`Pair::into_inner`](struct.Pair.html#method.into_inner).
pub struct Pairs<R, I: Input> {
    queue: Rc<Vec<QueueableToken<R>>>,
    input: Rc<I>,
    start: usize,
    end: usize
}

pub fn new<R: RuleType, I: Input>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: Rc<I>,
    start: usize,
    end: usize
) -> Pairs<R, I> {
    Pairs {
        queue,
        input,
        start,
        end
    }
}

impl<R: RuleType, I: Input> Pairs<R, I> {
    /// Flattens the `Pairs`.
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
    ///     a,
    ///     b
    /// }
    ///
    /// let input = Rc::new(StringInput::new("".to_owned()));
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
    pub fn flatten(self) -> FlatPairs<R, I> {
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
    /// # use pest::inputs::StringInput;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = Rc::new(StringInput::new("".to_owned()));
    /// let pairs = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    pub fn tokens(self) -> TokenIterator<R, I> {
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

impl<R: RuleType, I: Input> Iterator for Pairs<R, I> {
    type Item = Pair<R, I>;

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

impl<R: RuleType, I: Input> fmt::Debug for Pairs<R, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pairs {{ pairs: {:?} }}", self.clone().collect::<Vec<_>>())
    }
}

impl<R: Clone, I: Input> Clone for Pairs<R, I> {
    fn clone(&self) -> Pairs<R, I> {
        Pairs {
            queue: self.queue.clone(),
            input: self.input.clone(),
            start: self.start,
            end: self.end
        }
    }
}
