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
pub mod inputs;
pub mod iterators;
mod macros;
mod parser;
mod parser_state;
mod token;

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
