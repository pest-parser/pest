// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::rc::Rc;

use super::pair::{self, Pair};
use super::queueable_token::QueueableToken;
use super::token_iterator::{self, TokenIterator};
use super::super::inputs_private::Input;
use super::super::RuleType;

#[derive(Clone)]
pub struct Pairs<R: RuleType, I: Input> {
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
        queue: queue,
        input: input,
        start: start,
        end: end
    }
}

impl<R: RuleType, I: Input> Pairs<R, I> {
    pub fn into_iter(self) -> TokenIterator<R, I> {
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
        if self.start > self.end {
            None
        } else {
            let pair = pair::new(
                self.queue.clone(),
                self.input.clone(),
                self.start
            );

            self.start = self.pair() + 1;

            Some(pair)
        }
    }
}
