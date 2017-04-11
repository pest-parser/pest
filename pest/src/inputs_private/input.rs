// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Debug;
use std::ops::Range;

/// A `trait` that defines an input for a `Parser`. It should be implemented by custom input sources
/// as minimally and as efficiently possible. `unsafe` methods should not be called directly; in
/// order to parse the `Input` manually use [`Position`](struct.Position.html) and
/// [`ParserState`](../struct.ParserState.html).
///
/// Implementors should **NOT** introduce undefined behavior in these methods. Undefined behavior is
/// acceptable **ONLY** when the positions are either out of bounds or don't match UTF-8 indices,
/// since these cases are avoided by using the [`Position`](struct.Position.html) API.
pub trait Input: Debug {
    /// Returns length of the input.
    fn len(&self) -> usize;

    /// Returns whether the input is empty.
    fn is_empty(&self) -> bool;

    /// Slices the input.
    ///
    /// # Safety
    ///
    /// This method can cause undefined behavior when `start` or `end` are either out of bounds or
    /// don't match UTF-8 indices.
    unsafe fn slice(&self, start: usize, end: usize) -> &str;

    /// Returns the line - and column number of the input at `pos`.
    ///
    /// # Safety
    ///
    /// This method can cause undefined behavior when `pos` is either out of bounds or doesn't match
    /// UTF-8 indices.
    unsafe fn line_col(&self, pos: usize) -> (usize, usize);

    /// Returns the line of the input at `pos`.
    ///
    /// # Safety
    ///
    /// This method can cause undefined behavior when `pos` is either out of bounds or doesn't match
    /// UTF-8 indices.
    unsafe fn line_of(&self, pos: usize) -> &str;

    /// Tries to skip `n` `char`s at `pos`. Returns `Some(len)` with the UTF-8 length of the skipped
    /// `char`s position or `None` if there are not enough `char`s left to skip.
    ///
    /// # Safety
    ///
    /// This method can cause undefined behavior when `pos` is either out of bounds or doesn't match
    /// UTF-8 indices.
    unsafe fn skip(&self, n: usize, pos: usize) -> Option<usize>;

    /// Matches `string` at `pos` and returns whether it matched.
    ///
    /// # Safety
    ///
    /// This method can cause undefined behavior when `pos` is either out of bounds or doesn't match
    /// UTF-8 indices.
    unsafe fn match_string(&self, string: &str, pos: usize) -> bool;

    /// Matches `string` at `pos` case insensitively and returns whether it matched.
    ///
    /// # Safety
    ///
    /// This method can cause undefined behavior when `pos` is either out of bounds or doesn't match
    /// UTF-8 indices.
    unsafe fn match_insensitive(&self, string: &str, pos: usize) -> bool;

    /// Matches if the `char` is within the `range` and returns `Some(len)` with the matching
    /// `char`'s UTF-8 length if it matched or `None` otherwise.
    ///
    /// # Safety
    ///
    /// This method can cause undefined behavior when `pos` is either out of bounds or doesn't match
    /// UTF-8 indices.
    unsafe fn match_range(&self, range: Range<char>, pos: usize) -> Option<usize>;
}
