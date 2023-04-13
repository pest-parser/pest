// pest. The Elegant Parser
// Copyright (c) 2018-2023 Dragoș Tiselice, Tomas Tauber
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
//! # pest debugger
//!
//! This crate contains the CLI debugger.

#![doc(
    html_logo_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg",
    html_favicon_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg"
)]
#![warn(missing_docs, rust_2018_idioms, unused_qualifications)]
#[cfg(feature = "cli")]
mod cli;

#[cfg(feature = "cli")]
fn main() -> rustyline::Result<()> {
    cli::main()
}

#[cfg(not(feature = "cli"))]
fn main() {
    eprintln!("pest_debugger is a CLI tool. To use it, build it with the `cli` feature.");
}
