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
use core::str;

use super::queueable_token::QueueableToken;
use crate::position;
use crate::token::Token;
use crate::RuleType;

/// An iterator over [`Token`]s. It is created by [`Pair::tokens`] and [`Pairs::tokens`].
///
/// [`Token`]: ../enum.Token.html
/// [`Pair::tokens`]: struct.Pair.html#method.tokens
/// [`Pairs::tokens`]: struct.Pairs.html#method.tokens
#[derive(Clone)]
pub struct Tokens<'i, R> {
    /// # Safety:
    ///
    /// All `QueueableToken`s' `input_pos` must be valid character boundary indices into `input`.
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    start: usize,
    end: usize,
}

// TODO(safety): QueueableTokens must be valid indices into input.
pub fn new<R: RuleType>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &str,
    start: usize,
    end: usize,
) -> Tokens<'_, R> {
    if cfg!(debug_assertions) {
        for tok in queue.iter() {
            match *tok {
                QueueableToken::Start { input_pos, .. } | QueueableToken::End { input_pos, .. } => {
                    assert!(
                        input.get(input_pos..).is_some(),
                        "💥 UNSAFE `Tokens` CREATED 💥"
                    )
                }
            }
        }
    }

    Tokens {
        queue,
        input,
        start,
        end,
    }
}

impl<'i, R: RuleType> Tokens<'i, R> {
    fn create_token(&self, index: usize) -> Token<'i, R> {
        match self.queue[index] {
            QueueableToken::Start {
                end_token_index,
                input_pos,
            } => {
                let rule = match self.queue[end_token_index] {
                    QueueableToken::End { rule, .. } => rule,
                    _ => unreachable!(),
                };

                Token::Start {
                    rule,
                    // QueueableTokens are safely created.
                    pos: unsafe { position::Position::new_unchecked(self.input, input_pos) },
                }
            }
            QueueableToken::End {
                rule, input_pos, ..
            } => {
                Token::End {
                    rule,
                    // QueueableTokens are safely created.
                    pos: unsafe { position::Position::new_unchecked(self.input, input_pos) },
                }
            }
        }
    }
}

impl<'i, R: RuleType> Iterator for Tokens<'i, R> {
    type Item = Token<'i, R>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let token = self.create_token(self.start);

        self.start += 1;

        Some(token)
    }
}

impl<'i, R: RuleType> DoubleEndedIterator for Tokens<'i, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end <= self.start {
            return None;
        }

        let token = self.create_token(self.end - 1);

        self.end -= 1;

        Some(token)
    }
}

impl<'i, R: RuleType> fmt::Debug for Tokens<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::macros::tests::*;
    use super::super::super::Parser;
    use super::Token;
    use alloc::vec::Vec;

    #[test]
    fn double_ended_iter_for_tokens() {
        let pairs = AbcParser::parse(Rule::a, "abcde").unwrap();
        let mut tokens = pairs.clone().tokens().collect::<Vec<Token<'_, Rule>>>();
        tokens.reverse();
        let reverse_tokens = pairs.tokens().rev().collect::<Vec<Token<'_, Rule>>>();
        assert_eq!(tokens, reverse_tokens);
    }
}
