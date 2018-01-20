// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use RuleType;
use error::Error;
use iterators::Pairs;

/// A `trait` that defines a `Parser`.
pub trait Parser<R: RuleType> {
    /// Parses an `&str` starting from `rule`.
    fn parse(rule: R, input: &str) -> Result<Pairs<R>, Error<R>>;
}
