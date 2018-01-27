#[cfg(test)]
#[macro_use]
extern crate pest;

#[cfg(not(test))]
extern crate pest;

extern crate maplit;

pub mod parser;
pub mod ast;
pub mod validator;

use std::fmt::Display;

pub fn unwrap_or_report<T, E>(res: Result<T, E>) -> T where E: IntoIterator, E::Item: Display {
    res.unwrap_or_else(|e| panic!(e.into_iter()
              .map(|error| { format!("{}", error) })
              .collect::<Vec<_>>()
              .join("\n\n")))
}
