// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::rc::Rc;

use error::Error;
use inputs::StrInput;
use iterators::Pairs;
use RuleType;

/// A `trait` that defines a `Parser`.
pub trait Parser<R: RuleType> {
    /// Parses an `StrInput` starting from `rule`.
    fn parse<'i>(rule: R, input: Rc<StrInput<'i>>) -> Result<Pairs<'i, R>, Error<'i, R>>;

    /// Parses an `&str` starting from `rule`.
    fn parse_str<'i>(rule: R, input: &'i str) -> Result<Pairs<'i, R>, Error<'i, R>> {
        Self::parse(rule, Rc::new(StrInput::new(input)))
    }
}
