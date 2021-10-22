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
extern crate pest_meta;
extern crate test;

use pest_meta::{
    parser::{self, Rule},
    validator,
};
use std::fs::File;
use std::io::Read;
use test::Bencher;

#[bench]
fn validate_pairs(b: &mut Bencher) {
    let mut file = File::open("../meta/src/grammar.pest").unwrap();
    let mut data = String::new();
    file.read_to_string(&mut data).unwrap();

    let pairs = parser::parse(Rule::grammar_rules, &data).unwrap();
    b.iter(|| validator::validate_pairs(pairs.clone()));
}

#[bench]
fn validate_ast(b: &mut Bencher) {
    let mut file = File::open("../meta/src/grammar.pest").unwrap();
    let mut data = String::new();
    file.read_to_string(&mut data).unwrap();

    let pairs = parser::parse(Rule::grammar_rules, &data).unwrap();
    let rules = parser::consume_rules_with_spans(pairs).unwrap();
    b.iter(|| validator::validate_ast(&rules));
}
