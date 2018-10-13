// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use error::Error;
use iterators::Pairs;
use RuleType;

/// A trait with a single method that parses strings.
pub trait Parser<R: RuleType> {
    /// Parses a `&str` starting from `rule`.
    fn parse(rule: R, input: &str) -> Result<Pairs<R>, Error<R>>;
}
