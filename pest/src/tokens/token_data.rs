// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::hash::{Hash, Hasher};

use super::super::inputs::{Input, Span};

/// A `struct` that captures the essential data of a `Token` pair.
#[derive(Debug, Eq)]
pub struct TokenData<R, I: Input> {
    /// The `Rule` of the `Token` pair
    pub rule: R,
    /// The `Span` of the `Token` pair
    pub span: Span<I>
}

impl<R: Clone, I: Input> Clone for TokenData<R, I> {
    fn clone(&self) -> TokenData<R, I> {
        TokenData {
            rule: self.rule.clone(),
            span: self.span.clone()
        }
    }
}

impl<R: PartialEq, I: Input> PartialEq for TokenData<R, I> {
    fn eq(&self, other: &TokenData<R, I>) -> bool {
        self.rule == other.rule && self.span == other.span
    }
}

impl<R: Hash, I: Input> Hash for TokenData<R, I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rule.hash(state);
        self.span.hash(state);
    }
}
