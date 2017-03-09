// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Debug;

use super::inputs::Input;
use super::streams_private::parser_stream::ParserStream;

/// A `trait` that defines a `Parser`.
pub trait Parser<Rule: Debug + Eq + 'static> {
    /// Parses `input` starting from `rule` and returns a `ParserStream` of `Token`s.
    fn parse<I: Input>(rule: Rule, input: &I) -> ParserStream<Rule>;
}
