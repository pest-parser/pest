// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! # pest. Elegant, efficient grammars
//!
//! pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser generator with
//! *simplicity* and *speed* in mind.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         exp = { paren ~ exp | [""] }
//!         paren = { ["("] ~ exp ~ [")"] }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("(())((())())()"));
//!
//! assert!(parser.exp());
//! assert!(parser.end());
//! # }
//! ```

#[macro_use]
mod grammar;
#[macro_use]
mod process;
#[macro_use]
mod parsers;
mod input;
mod inputs;
mod parser;

pub use input::Input;
pub use inputs::StringInput;
pub use parser::Parser;
pub use parsers::Token;
