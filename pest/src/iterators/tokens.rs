// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::rc::Rc;

use super::queueable_token::QueueableToken;
use RuleType;
use position;
use token::Token;

/// A `struct` containing `Token`s. It is returned by either
/// [`Pair::into_iter`](struct.Pair.html#method.into_iter) or
/// [`Pairs::into_iter`](struct.Pairs.html#method.into_iter)
#[derive(Clone, Debug)]
pub struct Tokens<'i, R> {
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    index: usize,
    start: usize,
    end: usize
}

pub fn new<R: RuleType>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &str,
    start: usize,
    end: usize
) -> Tokens<R> {
    Tokens {
        queue,
        input,
        index: 0,
        start,
        end
    }
}

impl<'i, R: RuleType> Iterator for Tokens<'i, R> {
    type Item = Token<'i, R>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.end {
            return None;
        }

        let token = match self.queue[self.index] {
            QueueableToken::Start { pair, pos } => {
                let rule = match self.queue[pair] {
                    QueueableToken::End { rule, .. } => rule,
                    _ => unreachable!()
                };

                Token::Start {
                    rule,
                    // QueueableTokens are safely created.
                    pos: unsafe { position::new(self.input, pos) }
                }
            }
            QueueableToken::End { rule, pos } => {
                Token::End {
                    rule,
                    // QueueableTokens are safely created.
                    pos: unsafe { position::new(self.input, pos) }
                }
            }
        };

        self.index += 1;

        Some(token)
    }
}
