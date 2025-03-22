// pest. The Elegant Parser
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[macro_use]
extern crate pest;
extern crate pest_grammars;

use pest::Parser;
use pest_grammars::surround::*;
use pretty_assertions::assert_eq;

#[test]
fn surround_parenthesis() {
    parses_to! {
        parser: SurroundParser,
        input: "(hello world)",
        rule: Rule::Quote,
        tokens: [
            QuoteChars(1, 12)
        ]
    }
}

#[test]
fn surround_angle_brackets() {
    parses_to! {
        parser: SurroundParser,
        input: "<hello world>",
        rule: Rule::Quote,
        tokens: [
            QuoteChars(1, 12)
        ]
    }
}

#[test]
fn start_with_one_end_with_other() {
    fails_with! {
        parser: SurroundParser,
        input: "(hello world>",
        rule: Rule::Quote,
        positives: vec![],
        negatives: vec![],
        pos: 0
    }
}
