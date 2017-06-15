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
use super::super::inputs::Input;
use super::super::RuleType;

/// A `struct` containing `Pairs`. It is created by
/// [`Pairs::flatten`](struct.Pairs.html#method.flatten).
pub struct FlatPairs<R, I: Input> {
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
) -> FlatPairs<R, I> {
    FlatPairs {
        queue,
        input,
        start,
        end
    }
}

impl<R: RuleType, I: Input> FlatPairs<R, I> {
    /// Converts the `FlatPairs` into a `TokenIterator`.
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
    /// let input = Rc::new(StringInput::new("".to_owned()));
    /// let pairs = pest::state(input, |state, pos| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, pos, |_, p| Ok(p))
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.flatten().into_iter().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    pub fn into_iter(self) -> TokenIterator<R, I> {
        token_iterator::new(
            self.queue,
            self.input,
            self.start,
            self.end
        )
    }

    fn next_start(&mut self) {
        self.start += 1;

        while !self.is_start() {
            if self.start >= self.end {
                break;
            }

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

impl<R: RuleType, I: Input> Iterator for FlatPairs<R, I> {
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

        self.next_start();

        Some(pair)
    }
}

impl<R: RuleType, I: Input> fmt::Debug for FlatPairs<R, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FlatPairs {{ pairs: {:?} }}", self.clone().collect::<Vec<_>>())
    }
}

impl<R: Clone, I: Input> Clone for FlatPairs<R, I> {
    fn clone(&self) -> FlatPairs<R, I> {
        FlatPairs {
            queue: self.queue.clone(),
            input: self.input.clone(),
            start: self.start,
            end: self.end
        }
    }
}
