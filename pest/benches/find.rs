// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![feature(test)]

extern crate pest;
extern crate test;

use test::Bencher;

use pest::ParserState;

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    test_rule
}

#[bench]
fn find(b: &mut Bencher) {
    b.iter(|| {
        let state: Box<ParserState<Rule>> = ParserState::new("aaaaaaaaab");
        state.skip_until("b").unwrap()
    });
}

#[bench]
fn repeated_skip(b: &mut Bencher) {
    b.iter(|| {
        let state: Box<ParserState<Rule>> = ParserState::new("aaaaaaaaab");
        state
            .sequence(|state| {
                state.repeat(|state| {
                    state.sequence(|state| {
                        state.lookahead(false, |state| state.match_string("b"))
                            .and_then(|state| state.skip(1))
                    })
                }).and_then(|state| state.match_string("b"))
            }).unwrap()
    });
}
