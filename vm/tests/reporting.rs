// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate pest;
extern crate pest_meta;
#[macro_use]
extern crate pest_vm;

use pest_meta::parser::Rule;
use pest_meta::{optimizer, parser};
use pest_vm::Vm;

const GRAMMAR: &'static str = include_str!("reporting.pest");

fn vm() -> Vm {
    let pairs = parser::parse(Rule::grammar_rules, GRAMMAR).unwrap();
    let ast = parser::consume_rules(pairs).unwrap();
    Vm::new(optimizer::optimize(ast))
}

#[test]
fn choices() {
    fails_with! {
        parser: vm(),
        input: "x",
        rule: "choices",
        positives: vec!["a", "b", "c"],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn choices_no_progress() {
    fails_with! {
        parser: vm(),
        input: "x",
        rule: "choices_no_progress",
        positives: vec!["choices_no_progress"],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn choices_a_progress() {
    fails_with! {
        parser: vm(),
        input: "a",
        rule: "choices_a_progress",
        positives: vec!["a"],
        negatives: vec![],
        pos: 1
    };
}

#[test]
fn choices_b_progress() {
    fails_with! {
        parser: vm(),
        input: "b",
        rule: "choices_b_progress",
        positives: vec!["b"],
        negatives: vec![],
        pos: 1
    };
}

#[test]
fn nested() {
    fails_with! {
        parser: vm(),
        input: "x",
        rule: "level1",
        positives: vec!["a", "b", "c"],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn negative() {
    fails_with! {
        parser: vm(),
        input: "x",
        rule: "negative",
        positives: vec![],
        negatives: vec!["d"],
        pos: 0
    };
}

#[test]
fn negative_match() {
    fails_with! {
        parser: vm(),
        input: "x",
        rule: "negative_match",
        positives: vec!["b"],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn mixed() {
    fails_with! {
        parser: vm(),
        input: "x",
        rule: "mixed",
        positives: vec!["a"],
        negatives: vec!["d"],
        pos: 0
    };
}

#[test]
fn mixed_progress() {
    fails_with! {
        parser: vm(),
        input: "b",
        rule: "mixed_progress",
        positives: vec!["a"],
        negatives: vec![],
        pos: 1
    };
}
