// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate maplit;
#[cfg(test)]
#[macro_use]
extern crate pest;
#[cfg(not(test))]
extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fmt::Display;

pub mod parser;
pub mod ast;
pub mod validator;

pub fn unwrap_or_report<T, E>(result: Result<T, E>) -> T
    where
        E: IntoIterator,
        E::Item: Display
{
    result.unwrap_or_else(|e| {
        panic!(
            "grammar error\n\n".to_owned()
                + &e.into_iter()
                .map(|error| format!("{}", error))
                .collect::<Vec<_>>()
                .join("\n\n")
        )
    })
}
