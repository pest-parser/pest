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

use pest::Position;

#[bench]
fn find(b: &mut Bencher) {
    let pos = Position::from_start("aaaaaaaaab");

    b.iter(|| {
        pos.clone().skip_until("b").unwrap()
    });
}

#[bench]
fn repeated_skip(b: &mut Bencher) {
    let pos = Position::from_start("aaaaaaaaab");

    b.iter(|| {
        pos.clone().sequence(|pos| {
            pos.repeat(|pos| {
                pos.sequence(|pos| {
                    pos.lookahead(false, |pos| {
                        pos.match_string("b")
                    }).and_then(|pos| {
                        pos.skip(1)
                    })
                })
            }).and_then(|pos| {
                pos.match_string("b")
            })
        }).unwrap()
    });
}
