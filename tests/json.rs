// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![recursion_limit = "80"]

#[macro_use]
extern crate pest;

use pest::Parser;
use pest::Queues;
use pest::Input;
use pest::StringInput;

impl_rdp! {
    grammar! {
        object = { ["{"] ~ pair ~ ([","] ~ pair)* ~ ["}"] | ["{"] ~ ["}"] }
        pair = { string ~ [":"] ~ value }

        array = { ["["] ~ value ~ ([","] ~ value)* ~ ["]"] | ["["] ~ ["]"] }

        value = { string | number | object | array | ["true"] | ["false"] | ["null"] }

        string = { ["\""] ~ (escape | !(["\""] | ["\\"]) ~ any)* ~ ["\""] }
        escape = { ["\\"] ~ (["\""] | ["\\"] | ["/"] | ["b"] | ["f"] | ["n"] | ["r"] | ["t"] | unicode) }
        unicode = { ["u"] ~ hex ~ hex ~ hex ~ hex }
        hex = { ['0'..'9'] | ['a'..'f'] | ['A'..'F'] }

        number = { ["-"]? ~ int ~ ["."] ~ ['0'..'9']+ ~ exp? | ["-"]? ~ int ~ exp | ["-"]? ~ int }
        int = { ["0"] | ['1'..'9'] ~ ['0'..'9']* }
        exp = { (["E"] | ["e"]) ~ (["+"] | ["-"])? ~ int }

        ws = _{ [" "] | ["\t"] | ["\r"] | ["\n"] }
    }
}

#[test]
fn int_zero() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0")));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Rules::int(0, 1)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn int_long() {
    let mut parser = Rdp::new(Box::new(StringInput::new("234239923610")));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Rules::int(0, 12)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("e10")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Rules::exp(0, 3),
        Rules::int(1, 2)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_signed() {
    let mut parser = Rdp::new(Box::new(StringInput::new("E-0")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Rules::exp(0, 3),
        Rules::int(2, 1)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn number_neg_point_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("-13.0593e-10")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Rules::number(0, 12),
        Rules::int(1, 2),
        Rules::exp(8, 4),
        Rules::int(10, 2)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn number_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("593e-10")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Rules::number(0, 7),
        Rules::int(0, 3),
        Rules::exp(3, 4),
        Rules::int(5, 2)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn number_neg() {
    let mut parser = Rdp::new(Box::new(StringInput::new("-200")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Rules::number(0, 4),
        Rules::int(1, 3)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn unicode() {
    let mut parser = Rdp::new(Box::new(StringInput::new("u0a9F")));

    assert!(parser.unicode());
    assert!(parser.end());

    let queue = vec![
        Rules::unicode(0, 5),
        Rules::hex(1, 1),
        Rules::hex(2, 1),
        Rules::hex(3, 1),
        Rules::hex(4, 1)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn string() {
    let mut parser = Rdp::new(Box::new(StringInput::new("\"asd\\u0000\\\"\"")));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Rules::string(0, 13),
        Rules::escape(4, 6),
        Rules::unicode(5, 5),
        Rules::hex(6, 1),
        Rules::hex(7, 1),
        Rules::hex(8, 1),
        Rules::hex(9, 1),
        Rules::escape(10, 2)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn array_empty() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[ ]")));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Rules::array(0, 3)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn array() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[0.0e1, false, null, \"a\"]")));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Rules::array(0, 25),
        Rules::value(1, 5),
        Rules::number(1, 5),
        Rules::int(1, 1),
        Rules::exp(4, 2),
        Rules::int(5, 1),
        Rules::value(8, 5),
        Rules::value(15, 4),
        Rules::value(21, 3),
        Rules::string(21, 3)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn pair() {
    let mut parser = Rdp::new(Box::new(StringInput::new("\"a\" : 3")));

    assert!(parser.pair());
    assert!(parser.end());

    let queue = vec![
        Rules::pair(0, 7),
        Rules::string(0, 3),
        Rules::value(6, 1),
        Rules::number(6, 1),
        Rules::int(6, 1)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn object() {
    let mut parser = Rdp::new(Box::new(StringInput::new("{\"a\" : 3, \"b\" : [{}, 3.0e-10]}")));

    assert!(parser.object());
    assert!(parser.end());

    let queue = vec![
        Rules::object(0, 30),
        Rules::pair(1, 7),
        Rules::string(1, 3),
        Rules::value(7, 1),
        Rules::number(7, 1),
        Rules::int(7, 1),
        Rules::pair(10, 19),
        Rules::string(10, 3),
        Rules::value(16, 13),
        Rules::array(16, 13),
        Rules::value(17, 2),
        Rules::object(17, 2),
        Rules::value(21, 7),
        Rules::number(21, 7),
        Rules::int(21, 1),
        Rules::exp(24, 4),
        Rules::int(26, 2)
    ];

    assert!(parser.queue().iter().eq(&queue));
}
