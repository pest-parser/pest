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
    // TODO: This should not check boundaries.
    pub fn capture<'a, I: Input>(&self, input: &'a I) -> &'a str {
        input.slice(self.start, self.end)
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
