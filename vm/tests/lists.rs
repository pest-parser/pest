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

const GRAMMAR: &'static str = include_str!("lists.pest");

fn vm() -> Vm {
    let pairs = parser::parse(Rule::grammar_rules, GRAMMAR).unwrap();
    let ast = parser::consume_rules(pairs).unwrap();
    Vm::new(optimizer::optimize(ast))
}

#[test]
fn item() {
    parses_to! {
        parser: vm(),
        input: "- a",
        rule: "lists",
        tokens: [
            item(2, 3)
        ]
    };
}

#[test]
fn items() {
    parses_to! {
        parser: vm(),
        input: "- a\n- b",
        rule: "lists",
        tokens: [
            item(2, 3),
            item(6, 7)
        ]
    };
}

#[test]
fn children() {
    parses_to! {
        parser: vm(),
        input: "  - b",
        rule: "children",
        tokens: [
            children(0, 5, [
                item(4, 5)
            ])
        ]
    };
}

#[test]
fn nested_item() {
    parses_to! {
        parser: vm(),
        input: "- a\n  - b",
        rule: "lists",
        tokens: [
            item(2, 3),
            children(4, 9, [
                item(8, 9)
            ])
        ]
    };
}

#[test]
fn nested_items() {
    parses_to! {
        parser: vm(),
        input: "- a\n  - b\n  - c",
        rule: "lists",
        tokens: [
            item(2, 3),
            children(4, 15, [
                item(8, 9),
                item(14, 15)
            ])
        ]
    };
}

#[test]
fn nested_two_levels() {
    parses_to! {
        parser: vm(),
        input: "- a\n  - b\n    - c",
        rule: "lists",
        tokens: [
            item(2, 3),
            children(4, 17, [
                item(8, 9),
                children(10, 17, [
                    item(16, 17)
                ])
            ])
        ]
    };
}

#[test]
fn nested_then_not() {
    parses_to! {
        parser: vm(),
        input: "- a\n  - b\n- c",
        rule: "lists",
        tokens: [
            item(2, 3),
            children(4, 9, [
                item(8, 9)
            ]),
            item(12, 13)
        ]
    };
}
