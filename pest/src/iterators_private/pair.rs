// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::rc::Rc;

use super::pairs::{self, Pairs};
use super::queueable_token::QueueableToken;
use super::token_iterator::{self, TokenIterator};
use super::super::inputs_private::{Input, span, Span};
use super::super::RuleType;

#[derive(Clone)]
pub struct Pair<R: RuleType, I: Input> {
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
        queue: queue,
        input: input,
        start: start
    }
}

impl<R: RuleType, I: Input> Pair<R, I> {
    pub fn rule(&self) -> R {
        match self.queue[self.pair()] {
            QueueableToken::End { rule, .. } => rule,
            _ => unreachable!()
        }
    }

    pub fn span(self) -> Span<I> {
        let start = self.pos(self.start);
        let end = self.pos(self.pair());

        span::new(self.input, start, end)
    }

    pub fn consume(self) -> Pairs<R, I> {
        let pair = self.pair();

        pairs::new(
            self.queue,
            self.input,
            self.start + 1,
            pair - 1
        )
    }

    pub fn into_iter(self) -> TokenIterator<R, I> {
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
            QueueableToken::Start { pos, .. } => pos,
            QueueableToken::End { pos, .. } => pos
        }
    }
}
