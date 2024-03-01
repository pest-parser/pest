// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;
extern crate pest;
extern crate pest_derive;

#[cfg(feature = "grammar-extras")]
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../tests/opt.pest"]
struct TestOptParser;

#[test]
#[cfg(feature = "grammar-extras")]
fn test_opt_tag() {
    let successful_parse = TestOptParser::parse(Rule::expr, "*");
    assert!(successful_parse.is_ok());
    let pairs = successful_parse.unwrap();
    assert!(pairs.find_first_tagged("prefix").is_some());
    assert!(pairs.find_first_tagged("suffix").is_none());
}
