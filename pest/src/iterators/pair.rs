// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::ptr;
use std::rc::Rc;
use std::str;

use super::pairs::{self, Pairs};
use super::queueable_token::QueueableToken;
use super::tokens::{self, Tokens};
use span::{self, Span};
use RuleType;

/// A matching pair of [`Token`]s and everything between them.
///
/// A matching `Token` pair is formed by a `Token::Start` and a subsequent `Token::End` with the
/// same `Rule`, with the condition that all `Token`s between them can form such pairs as well.
/// This is similar to the [brace matching problem](https://en.wikipedia.org/wiki/Brace_matching) in
/// editors.
///
/// [`Token`]: ../enum.Token.html
#[derive(Clone)]
pub struct Pair<'i, R> {
    /// # Safety
    ///
    /// All `QueueableToken`s' `input_pos` must be valid character boundary indices into `input`.
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i str,
    /// Token index into `queue`.
    start: usize,
}

// TODO(safety): QueueableTokens must be valid indices into input.
pub fn new<R: RuleType>(queue: Rc<Vec<QueueableToken<R>>>, input: &str, start: usize) -> Pair<R> {
    if cfg!(debug_assertions) {
        for tok in queue.iter() {
            match *tok {
                QueueableToken::Start { input_pos, .. } | QueueableToken::End { input_pos, .. } => {
                    assert!(
                        input.get(input_pos..).is_some(),
                        "💥 UNSAFE `Pair` CREATED 💥"
                    )
                }
            }
        }
    }

    Pair {
        queue,
        input,
        start,
    }
}

impl<'i, R: RuleType> Pair<'i, R> {
    /// Returns the `Rule` of the `Pair`.
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
    /// let pair = pest::state(input, |state| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, |s| Ok(s))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.as_rule(), Rule::a);
    /// ```
    #[inline]
    pub fn as_rule(&self) -> R {
        match self.queue[self.pair()] {
            QueueableToken::End { rule, .. } => rule,
            _ => unreachable!(),
        }
    }

    /// Captures a slice from the `&str` defined by the token `Pair`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let pair = pest::state(input, |state| {
    ///     // generating Token pair with Rule::ab ...
    /// #     state.rule(Rule::ab, |s| s.match_string("ab"))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.as_str(), "ab");
    /// ```
    #[inline]
    pub fn as_str(&self) -> &'i str {
        let start = self.pos(self.start);
        let end = self.pos(self.pair());

        // Generated positions always come from Positions and are UTF-8 borders.
        &self.input[start..end]
    }

    /// Returns the `Span` defined by the `Pair`, consuming it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let pair = pest::state(input, |state| {
    ///     // generating Token pair with Rule::ab ...
    /// #     state.rule(Rule::ab, |s| s.match_string("ab"))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.into_span().as_str(), "ab");
    /// ```
    #[inline]
    #[deprecated(since = "2.0.0", note = "Please use `as_span` instead")]
    pub fn into_span(self) -> Span<'i> {
        self.as_span()
    }

    /// Returns the `Span` defined by the `Pair`, **without** consuming it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab";
    /// let pair = pest::state(input, |state| {
    ///     // generating Token pair with Rule::ab ...
    /// #     state.rule(Rule::ab, |s| s.match_string("ab"))
    /// }).unwrap().next().unwrap();
    ///
    /// assert_eq!(pair.as_span().as_str(), "ab");
    /// ```
    #[inline]
    pub fn as_span(&self) -> Span<'i> {
        let start = self.pos(self.start);
        let end = self.pos(self.pair());

        // Generated positions always come from Positions and are UTF-8 borders.
        unsafe { span::Span::new_unchecked(self.input, start, end) }
    }

    /// Returns the inner `Pairs` between the `Pair`, consuming it.
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
    /// let pair = pest::state(input, |state| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, |s| Ok(s))
    /// }).unwrap().next().unwrap();
    ///
    /// assert!(pair.into_inner().next().is_none());
    /// ```
    #[inline]
    pub fn into_inner(self) -> Pairs<'i, R> {
        let pair = self.pair();

        pairs::new(self.queue, self.input, self.start + 1, pair)
    }

    /// Returns the `Tokens` for the `Pair`.
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
    /// let pair = pest::state(input, |state| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, |s| Ok(s))
    /// }).unwrap().next().unwrap();
    /// let tokens: Vec<_> = pair.tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn tokens(self) -> Tokens<'i, R> {
        let end = self.pair();

        tokens::new(self.queue, self.input, self.start, end + 1)
    }

    fn pair(&self) -> usize {
        match self.queue[self.start] {
            QueueableToken::Start {
                end_token_index, ..
            } => end_token_index,
            _ => unreachable!(),
        }
    }

    fn pos(&self, index: usize) -> usize {
        match self.queue[index] {
            QueueableToken::Start { input_pos, .. } | QueueableToken::End { input_pos, .. } => {
                input_pos
            }
        }
    }
}

impl<'i, R: RuleType> fmt::Debug for Pair<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Pair")
            .field("rule", &self.as_rule())
            .field("span", &self.as_span())
            .field("inner", &self.clone().into_inner().collect::<Vec<_>>())
            .finish()
    }
}

impl<'i, R: RuleType> fmt::Display for Pair<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rule = self.as_rule();
        let start = self.pos(self.start);
        let end = self.pos(self.pair());
        let mut pairs = self.clone().into_inner().peekable();

        if pairs.peek().is_none() {
            write!(f, "{:?}({}, {})", rule, start, end)
        } else {
            write!(
                f,
                "{:?}({}, {}, [{}])",
                rule,
                start,
                end,
                pairs
                    .map(|pair| format!("{}", pair))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}

impl<'i, R: PartialEq> PartialEq for Pair<'i, R> {
    fn eq(&self, other: &Pair<'i, R>) -> bool {
        Rc::ptr_eq(&self.queue, &other.queue)
            && ptr::eq(self.input, other.input)
            && self.start == other.start
    }
}

impl<'i, R: Eq> Eq for Pair<'i, R> {}

impl<'i, R: Hash> Hash for Pair<'i, R> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.queue as *const Vec<QueueableToken<R>>).hash(state);
        (self.input as *const str).hash(state);
        self.start.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use macros::tests::*;
    use parser::Parser;

    #[test]
    fn pair_into_inner() {
        let pair = AbcParser::parse(Rule::a, "abcde").unwrap().next().unwrap(); // the tokens a(b())

        let pairs = pair.into_inner(); // the tokens b()

        assert_eq!(2, pairs.tokens().count());
    }
}
