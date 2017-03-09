// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::super::inputs::Input;

/// A `struct` that captures the essential data of a `Token` pair.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TokenData<Rule> {
    /// The `Rule` of the `Token` pair
    pub rule:  Rule,
    /// The start position
    pub start: usize,
    /// The end position
    pub end:   usize
}

impl<Rule> TokenData<Rule> {
    /// Captures a `&str` slice of an `Input` according to a `TokenData`'s `start` and `end`
    /// positions.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::{Input, StringInput};
    /// # use pest::tokens::TokenData;
    /// let input = StringInput::new("abcd");
    /// let data = TokenData {
    ///     rule:  (),
    ///     start: 1,
    ///     end:   3
    /// };
    ///
    /// assert_eq!(data.capture(&input), "bc");
    /// ```
    pub fn capture<'a, I: Input>(&self, input: &'a I) -> &'a str {
        input.slice(self.start, self.end)
    }

    /// Unsafely captures a `&str` slice of an `Input` according to a `TokenData`'s `start` and
    /// `end` positions without checking `char` boundaries.
    ///
    /// This offers a speedup and is guaranteed not to cause undefined behavior with pest-produced
    /// `TokenData` and matching `Input`, i.e. by using the same `Input` as the one used in the
    /// `Parser::parse` call.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::{Input, StringInput};
    /// # use pest::tokens::TokenData;
    /// let input = StringInput::new("abcd");
    /// let data = TokenData { // Only use with auto-generated TokenData.
    ///     rule:  (),
    ///     start: 1,
    ///     end:   3
    /// };
    ///
    /// assert_eq!(unsafe { data.capture_unchecked(&input) }, "bc");
    /// ```
    pub unsafe fn capture_unchecked<'a, I: Input>(&self, input: &'a I) -> &'a str {
        input.slice_unchecked(self.start, self.end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::super::inputs::StringInput;

    #[test]
    fn capture() {
        let input = StringInput::new("asdasdf");
        let data = TokenData { rule: (), start: 1, end: 3 };

        assert_eq!(data.capture(&input), "sd");
    }
}
