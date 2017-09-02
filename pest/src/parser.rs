// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::rc::Rc;

use super::error::Error;
use super::inputs::{Input, StringInput};
use super::iterators::Pairs;
use super::RuleType;

/// A `trait` that defines a `Parser`.
pub trait Parser<R: RuleType> {
    /// Parses `input` starting from `rule`.
    fn parse<I: Input>(rule: R, input: Rc<I>) -> Result<Pairs<R, I>, Error<R, I>>;
    /// Parses an `input` &str starting from `rule`.
    fn parse_str(rule: R, input: &str) -> Result<Pairs<R, StringInput>, Error<R, StringInput>> {
        Self::parse(rule, Rc::new(StringInput::new_borrowed(input)))
    }
}
