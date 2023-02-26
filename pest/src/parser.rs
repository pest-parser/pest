// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::error::Error;
use crate::iterators::Pairs;
use crate::{RuleType, StateCheckpoint};

/// A trait with a single method that parses strings.
pub trait Parser<R: RuleType, S: Default + StateCheckpoint = ()> {
    /// Parses a `&str` starting from `rule`.
    #[allow(clippy::perf)]
    fn parse(rule: R, input: &str) -> Result<Pairs<'_, R>, Error<R>>;
}

/// A trait with a single method that parses strings.
pub trait StateParser<R: RuleType, S: StateCheckpoint> {
    /// Parses a `&str` starting from `rule`.
    #[allow(clippy::perf)]
    fn parse_with_state(rule: R, input: &str, state: S) -> Result<(S, Pairs<'_, R>), Error<R>>;
}

impl<R: RuleType, S: StateCheckpoint + Default, T: StateParser<R, S>> Parser<R, S> for T {
    fn parse(rule: R, input: &str) -> Result<Pairs<'_, R>, Error<R>> {
        let (_, pairs) = Self::parse_with_state(rule, input, S::default())?;
        Ok(pairs)
    }
}
