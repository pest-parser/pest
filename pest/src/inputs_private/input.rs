// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ops::Range;

/// A `trait` that defines an input for a `Parser`.
pub trait Input {
    /// Returns length of the input.
    fn len(&self) -> usize;

    /// Returns whether the input is empty.
    fn is_empty(&self) -> bool;

    /// Slices the input.
    unsafe fn slice(&self, start: usize, end: usize) -> &str;

    /// Returns the line - and column number of the input at `pos`.
    unsafe fn line_col(&self, pos: usize) -> (usize, usize);

    /// Returns the line of the input at `pos`.
    unsafe fn line_of(&self, pos: usize) -> &str;

    /// Tries to skip `n` `char`s at `pos`. Returns `Some(pos)` with the new position or `None` if
    /// there are not enough `char` left to skip.
    unsafe fn skip(&self, n: usize, pos: usize) -> Option<usize>;

    /// Matches `string` at `pos` and returns whether it matched.
    unsafe fn match_string(&self, string: &str, pos: usize) -> bool;

    /// Matches `string` at `pos` case insensitively and returns whether it matched.
    unsafe fn match_insensitive(&self, string: &str, pos: usize) -> bool;

    /// Matches if the `char` is within the `range` and returns `Some(len)` with the matching
    /// `char`'s UTF-8 length if it matched or `None` otherwise.
    unsafe fn match_range(&self, range: Range<char>, pos: usize) -> Option<usize>;
}
