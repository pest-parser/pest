// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! # pest. Smart PEGs in Rust
//!
//! pest is a parser generator that works with
//! [PEGs](https://en.wikipedia.org/wiki/Parsing_expression_grammar).
//!
//! It relies exclusively on macros to create an efficient parser at compile-time.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Queues;
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
mod parsers;
mod input;
mod inputs;
mod parser;

pub use input::Input;
pub use inputs::StringInput;
pub use parser::Parser;
pub use parsers::Queues;
pub use parsers::Token;
