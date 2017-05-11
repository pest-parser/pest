// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![doc(html_root_url = "https://docs.rs/pest/1.0.0")]

use std::fmt::Debug;
use std::hash::Hash;

mod error;
mod inputs_private;
mod iterators_private;
mod macros;
mod parser;
mod parser_state;
mod token;

/// A `mod` containing the `Input`-related constructs.
pub mod inputs {
    pub use super::inputs_private::{Input, Position, Span, StringInput};
}

/// A `mod` containing iterators and constructs to aid in parser output manipulation.
pub mod iterators {
    pub use super::iterators_private::{Pair, Pairs, TokenIterator};
}

/// A `trait` which parser rules must implement.
///
/// This trait is setup so that any struct that implements all of its required traits will
/// automatically implement this trait as well.
///
/// This is essentially a [trait alias](https://github.com/rust-lang/rfcs/pull/1733). When trait
/// aliases are implemented, this may be replaced by one.
pub trait RuleType: Copy + Debug + Eq + Hash + Ord {}
impl<T: Copy + Debug + Eq + Hash + Ord> RuleType for T {}

pub use error::Error;
pub use parser::Parser;
pub use parser_state::{state, ParserState};
pub use token::Token;
