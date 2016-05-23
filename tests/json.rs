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
        json = { value ~ eoi }

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
        Token { rule: Rule::int, pos: 0, len: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn int_long() {
    let mut parser = Rdp::new(Box::new(StringInput::new("234239923610")));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::int, pos: 0, len: 12 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("e10")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, pos: 0, len: 3 },
        Token { rule: Rule::int, pos: 1, len: 2 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_signed() {
    let mut parser = Rdp::new(Box::new(StringInput::new("E-0")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, pos: 0, len: 3 },
        Token { rule: Rule::int, pos: 2, len: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn number_neg_point_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("-13.0593e-10")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, pos: 0, len: 12 },
        Token { rule: Rule::int, pos: 1, len: 2 },
        Token { rule: Rule::exp, pos: 8, len: 4 },
        Token { rule: Rule::int, pos: 10, len: 2 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn number_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("593e-10")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, pos: 0, len: 7 },
        Token { rule: Rule::int, pos: 0, len: 3 },
        Token { rule: Rule::exp, pos: 3, len: 4 },
        Token { rule: Rule::int, pos: 5, len: 2 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn number_neg() {
    let mut parser = Rdp::new(Box::new(StringInput::new("-200")));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, pos: 0, len: 4 },
        Token { rule: Rule::int, pos: 1, len: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn unicode() {
    let mut parser = Rdp::new(Box::new(StringInput::new("u0a9F")));

    assert!(parser.unicode());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::unicode, pos: 0, len: 5 },
        Token { rule: Rule::hex, pos: 1, len: 1 },
        Token { rule: Rule::hex, pos: 2, len: 1 },
        Token { rule: Rule::hex, pos: 3, len: 1 },
        Token { rule: Rule::hex, pos: 4, len: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn string() {
    let mut parser = Rdp::new(Box::new(StringInput::new("\"asd\\u0000\\\"\"")));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::string, pos: 0, len: 13 },
        Token { rule: Rule::escape, pos: 4, len: 6 },
        Token { rule: Rule::unicode, pos: 5, len: 5 },
        Token { rule: Rule::hex, pos: 6, len: 1 },
        Token { rule: Rule::hex, pos: 7, len: 1 },
        Token { rule: Rule::hex, pos: 8, len: 1 },
        Token { rule: Rule::hex, pos: 9, len: 1 },
        Token { rule: Rule::escape, pos: 10, len: 2 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn array_empty() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[ ]")));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::array, pos: 0, len: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn array() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[0.0e1, false, null, \"a\"]")));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::array, pos: 0, len: 25 },
        Token { rule: Rule::value, pos: 1, len: 5 },
        Token { rule: Rule::number, pos: 1, len: 5 },
        Token { rule: Rule::int, pos: 1, len: 1 },
        Token { rule: Rule::exp, pos: 4, len: 2 },
        Token { rule: Rule::int, pos: 5, len: 1 },
        Token { rule: Rule::value, pos: 8, len: 5 },
        Token { rule: Rule::value, pos: 15, len: 4 },
        Token { rule: Rule::value, pos: 21, len: 3 },
        Token { rule: Rule::string, pos: 21, len: 3 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn pair() {
    let mut parser = Rdp::new(Box::new(StringInput::new("\"a\" : 3")));

    assert!(parser.pair());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::pair, pos: 0, len: 7 },
        Token { rule: Rule::string, pos: 0, len: 3 },
        Token { rule: Rule::value, pos: 6, len: 1 },
        Token { rule: Rule::number, pos: 6, len: 1 },
        Token { rule: Rule::int, pos: 6, len: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn object() {
    let mut parser = Rdp::new(Box::new(StringInput::new("{\"a\" : 3, \"b\" : [{}, 3.0e-10]}")));

    assert!(parser.object());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::object, pos: 0, len: 30 },
        Token { rule: Rule::pair, pos: 1, len: 7 },
        Token { rule: Rule::string, pos: 1, len: 3 },
        Token { rule: Rule::value, pos: 7, len: 1 },
        Token { rule: Rule::number, pos: 7, len: 1 },
        Token { rule: Rule::int, pos: 7, len: 1 },
        Token { rule: Rule::pair, pos: 10, len: 19 },
        Token { rule: Rule::string, pos: 10, len: 3 },
        Token { rule: Rule::value, pos: 16, len: 13 },
        Token { rule: Rule::array, pos: 16, len: 13 },
        Token { rule: Rule::value, pos: 17, len: 2 },
        Token { rule: Rule::object, pos: 17, len: 2 },
        Token { rule: Rule::value, pos: 21, len: 7 },
        Token { rule: Rule::number, pos: 21, len: 7 },
        Token { rule: Rule::int, pos: 21, len: 1 },
        Token { rule: Rule::exp, pos: 24, len: 4 },
        Token { rule: Rule::int, pos: 26, len: 2 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn fail_number() {
    let mut parser = Rdp::new(Box::new(StringInput::new("3f")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::eoi, Rule::exp], 1));
}

#[test]
fn fail_number_zero() {
    let mut parser = Rdp::new(Box::new(StringInput::new("03")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::eoi, Rule::exp], 1));
}

#[test]
fn fail_exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("3.4e")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::int], 4));
}

#[test]
fn fail_string() {
    let mut parser = Rdp::new(Box::new(StringInput::new("\"a")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::any, Rule::escape], 2));
}

#[test]
fn fail_string_unicode() {
    let mut parser = Rdp::new(Box::new(StringInput::new("\"\\u00p")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::hex], 5));
}

#[test]
fn fail_array() {
    let mut parser = Rdp::new(Box::new(StringInput::new("[1, ]")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![
        Rule::int,
        Rule::number,
        Rule::string,
        Rule::value,
        Rule::array,
        Rule::object
    ], 4));
}

#[test]
fn fail_object_pair() {
    let mut parser = Rdp::new(Box::new(StringInput::new("{\"a\" : }")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![
        Rule::int,
        Rule::number,
        Rule::string,
        Rule::value,
        Rule::array,
        Rule::object
    ], 7));
}

#[test]
fn fail_object_open() {
    let mut parser = Rdp::new(Box::new(StringInput::new("{\"a\" : 3")));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::exp], 8));
}
