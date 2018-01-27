// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! # pest. The Elegant Parser
//!
//! pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser built with
//! *simplicity* and *speed* in mind.
//!
//! ## Parser
//!
//! pest works mainly through a `trait`, `Parser`, which provides an interface to the parsing
//! functionality. Since `Parser` is a `trait`, parsing needs to be defined either though the
//! `#[derive(Parser)]` attribute, or manually through the [`Position API`](struct.Position.html).
//! The use of the `derive` is highly encouraged since this is the only way you can make use of
//! pest's PEG grammar, while manual parser definition can be used where highly specific or
//! efficient parsing is required.
//!
//! ## `#[derive(Parser)]`
//!
//! pest comes with a procedural macro crate--`pest_derive`--which needs to be included in
//! `Cargo.toml` in order to enable the `derive`.
//!
//! ```toml
//! pest_derive = "*"
//! ```
//!
//! ## `.pest` files
//!
//! Grammar definitions reside in custom `.pest` files located in the `src` directory. Their path is
//! relative to `src` and is specified between the `derive` attribute and an empty `struct` that
//! `Parser` will be derived on.
//!
//! Because of a limitation in procedural macros, there is no way for Cargo to know that a module
//! needs to be recompiled based on the file that the procedural macro is opening. This leads to the
//! case where modifying a `.pest` file without touching the file where the `derive` is does not
//! recompile it if it already has a working binary in the cache. To avoid this issue, the grammar
//! file can be included in a dummy `const` definition while debugging.
//!
//! ```ignore
//! #[cfg(debug_assertions)]
//! const _GRAMMAR: &'static str = include_str!("path/to/my_grammar.pest"); // relative to this file
//!
//! #[derive(Parser)]
//! #[grammar = "path/to/my_grammar.pest"] // relative to src
//! struct MyParser;
//! ```
//!
//! The grammar of `.pest` files is documented in the
//! [`pest_derive` crate](https://docs.rs/pest_derive/#Grammar).

#![doc(html_root_url = "https://docs.rs/pest")]

use std::fmt::Debug;
use std::hash::Hash;

mod error;
pub mod iterators;
mod macros;
mod parser;
mod parser_state;
mod position;
pub mod prec_climber;
mod span;
mod token;

/// A `trait` which parser rules must implement.
///
/// This trait is set up so that any struct that implements all of its required traits will
/// automatically implement this trait as well.
///
/// This is essentially a [trait alias](https://github.com/rust-lang/rfcs/pull/1733). When trait
/// aliases are implemented, this may be replaced by one.
pub trait RuleType: Copy + Debug + Eq + Hash + Ord {}
impl<T: Copy + Debug + Eq + Hash + Ord> RuleType for T {}

pub use error::Error;
pub use parser::Parser;
pub use parser_state::{state, Atomicity, Lookahead, ParserState};
pub use position::Position;
pub use span::Span;
pub use token::Token;
