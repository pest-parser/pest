// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::Input;

/// A `trait` that defines a parser.
pub trait Parser<'a, T: Input<'a>> {
    type Rule;
    type Token;

    fn input(&self) -> &T;

    fn input_mut(&mut self) -> &mut T;

    /// Returns whether a `Parser` has reached its end.
    fn end(&self) -> bool;

    /// Returns whether a `Parser` has matched end-of-input.
    fn eoi_matched(&self) -> bool;

    /// Reset a `Parser`.
    fn reset(&mut self);

    /// Returns the queue of all matched `Token`s.
    fn queue(&self) -> &Vec<Self::Token>;

    /// Returns the mutable queue of all matched `Token`s.
    fn queue_mut(&mut self) -> &mut Vec<Self::Token>;

    /// Returns the current index within the queue. Used in `process!`.
    fn queue_index(&self) -> usize;

    /// Increments the current index within the queue. Used in `process!`.
    fn inc_queue_index(&self);

    /// Set the current index within the queue. Used in `process!`.
    fn set_queue_index(&self, index: usize);

    /// Skips white-space.
    fn skip_ws(&mut self);

    /// Skips comments.
    fn skip_com(&mut self);

    /// Returns whether a `Parser` is currently inside an atomic rule.
    fn is_atomic(&self) -> bool;

    /// Sets a `Parser` to atomic rule mode, barring comment & white-space skipping.
    fn set_atomic(&mut self, value: bool);

    /// Keeps track of rule failures. It gets called when a `Rule` fails at `pos`.
    fn track(&mut self, failed: Self::Rule, pos: usize);

    /// Returns the length of the tracked `Rule`s.
    fn tracked_len(&self) -> usize;

    /// Retuns a `Vec` of all expected `Rule`s at the deepest position where the parsing last
    /// stopped. It only returns leafs from the rule tree. Used for error reporting.
    fn expected(&mut self) -> (Vec<Self::Rule>, usize);
}
