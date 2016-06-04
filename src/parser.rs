// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `trait` that defines a parser.
pub trait Parser {
    type Rule;
    type Token;

    /// Matches `string`, returns whether it matched, and advances a parser with `string.len()` in
    /// case it did.
    fn matches(&mut self, string: &str) -> bool;

    /// Matches `char` between `left` and `right`, and advances a parser with one `char` in case
    /// it did.
    fn between(&mut self, left: char, right: char) -> bool;

    /// Tries to match `rule`, returns whether it matched, and advances a parser with in case it
    /// did. If `revert` is `true`, the parser will not advance.
    fn try<F>(&mut self, revert: bool, rule: F) -> bool where F: FnOnce(&mut Self) -> bool;

    /// Uses the precendence climbing algorithm to match rules. `pos` is the current position
    /// of the queue. `left` is the left-most starting position of the current rule. `prec` is the
    /// currently processed precedence. `last_op` is the last greedily parsed infix operator.
    /// `primary` is a closure defined in `grammar!` that parses a primary expression. `climb` is a
    /// closure defined in `grammar!` that returns the first `Rule` that was parsed (provided it
    /// was not silented) along with its precedence and right-associativity, or `None` if no
    /// operator passes. This operator triplet is also returned by the function when it greedily
    /// parses an operator useful for a higher precedence.
    fn prec_climb<F, G>(&mut self, pos: usize, left: usize, prec: u8,
                        last_op: Option<(Option<Self::Rule>, u8, bool)>, primary: &mut F,
                        climb: &mut G) -> (Option<(Option<Self::Rule>, u8, bool)>, Option<usize>)
        where F: FnMut(&mut Self) -> bool,
              G: FnMut(&mut Self) -> Option<(Option<Self::Rule>, u8, bool)>;

    /// Returns the current position of a `Parser`.
    fn pos(&self) -> usize;

    /// Sets the position of a `Parser`.
    fn set_pos(&mut self, pos: usize);

    /// Returns whether a `Parser` has reached it end.
    fn end(&self) -> bool;

    /// Reset a `Parser`.
    fn reset(&mut self);

    /// Slices a `Parser`'s `Input`.
    fn slice_input(&self, start: usize, end: usize) -> &str;

    /// Returns the queue of all matched `Token`s.
    fn queue(&self) -> &Vec<Self::Token>;

    /// Returns the mutable queue of all matched `Token`s.
    fn queue_mut(&mut self) -> &mut Vec<Self::Token>;

    /// Skips white-space.
    fn skip_ws(&mut self);

    /// Skips comments.
    fn skip_com(&mut self);

    /// Returns whether a `Parser` is currently inside an atomic rule.
    fn is_atomic(&self) -> bool;

    /// Sets a `Parser` to atomic rule mode, barring comment & white-space skipping.
    fn set_atomic(&mut self, value: bool);

    /// Keeps track of rule failures. It gets called when a rule fails at `pos`.
    fn track(&mut self, failed: Self::Rule, pos: usize);

    /// Retuns a `Vec` of all expected `Rule`s at the deepest position where the parsing last
    /// stopped. Used for error reporting.
    fn expected(&mut self) -> (Vec<Self::Rule>, usize);
}
