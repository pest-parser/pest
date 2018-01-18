// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use RuleType;
use error::Error;
use iterators::Pairs;

/// A `trait` that defines a `Parser`.
pub trait Parser<R: RuleType> {
    /// Parses an `&str` starting from `rule`.
    fn parse(rule: R, input: &str) -> Result<Pairs<R>, Error<R>>;
}
