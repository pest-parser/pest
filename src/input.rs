// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `trait` that defines an input for a `Parser`.
pub trait Input {
    /// Returns length of an `Input`.
    fn len(&self) -> usize;

    /// Returns current position of an `Input`.
    fn pos(&self) -> usize;

    /// Set current position of an `Input`.
    fn set_pos(&mut self, pos: usize);

    /// Slices an `Input`.
    fn slice(&self, start: usize, end: usize) -> &str;

    /// Returns the line and column of a position for an `Input`.
    fn line_col(&self, pos: usize) -> (usize, usize);

    /// Matches `string` to an `Input`, returns whether it matched, and advances the position with
    /// `string.len()` in case it did.
    fn matches(&mut self, string: &str) -> bool;

    /// Matches if an `Input`'s current `char` is between `left` and `right`, and advances the
    /// position with one `char` in case it did.
    fn between(&mut self, left: char, right: char) -> bool;
}
