// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `trait` that defines an input for a `Parser`.
pub trait Input {
    /// Returns length of an `Input`.
    fn len(&self) -> usize;

    /// Returns whether an `Input` is empty.
    fn is_empty(&self) -> bool;

    /// Slices an `Input`.
    fn slice(&self, start: usize, end: usize) -> &str;

    /// Returns the line and column numbers of a position for an `Input`.
    fn line_col(&self, pos: usize) -> (usize, usize);

    /// Returns the line of a position for an `Input`.
    fn line_of(&self, pos: usize) -> &str;

    /// Matches `string` to an `Input` at `pos` and returns whether it matched.
    fn match_string(&self, string: &str, pos: usize) -> bool;

    /// Matches `string` to an `Input` at `pos` case insensitively and returns whether it matched.
    fn match_insensitive(&self, string: &str, pos: usize) -> bool;

    /// Matches if `Input`'s `char` at `pos` is between `left` and `right`.
    fn match_range(&self, left: char, right: char, pos: usize) -> bool;
}
