// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate futures;

use std::fmt::Debug;
use std::hash::Hash;

mod error;
mod inputs_private;
mod macros;
mod parser;
mod parser_state;
mod streams_private;
pub mod tokens;

/// A `mod` containing the `Input` `trait` and implementations.
pub mod inputs {
    pub use super::inputs_private::{Input, Position, Span, StringInput};
}

/// A `mod` containing `Stream` implementations used in `Token` processing.
pub mod streams {
    pub use super::streams_private::{ConsumedStream, ParserStream, TokenDataFuture, TokenStream,
                                     TailStream};
}

pub trait RuleType: Copy + Debug + Eq + Hash + Ord + Send + Sync {}
impl<T: Copy + Debug + Eq + Hash + Ord + Send + Sync> RuleType for T {}

pub use error::Error;
pub use parser::Parser;
pub use parser_state::{state, ParserState};
