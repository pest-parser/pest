// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `trait` that defines a parser.
pub trait Parser {
    /// Matches `string`, returns whether it matched, and advances the parser with `string.len()`
    /// in case it did.
    fn matches(&mut self, string: &str) -> bool;

    /// Tries to match `rule`, returns whether it matched, and advances the parser with in case it
    /// did.
    fn try(&mut self, rule: Box<Fn(&mut Self) -> bool>) -> bool where Self: Sized;

    /// Returns whether a `Parser` has reached it end.
    fn end(&mut self) -> bool;

    /// Reset a `Parser`.
    fn reset(&mut self);
}
