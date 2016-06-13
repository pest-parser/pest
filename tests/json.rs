// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![recursion_limit = "80"]

#[macro_use]
extern crate pest;

use pest::prelude::*;

impl_rdp! {
    grammar! {
        json = { value ~ eoi }

        object = { ["{"] ~ pair ~ ([","] ~ pair)* ~ ["}"] | ["{"] ~ ["}"] }
        pair   = { string ~ [":"] ~ value }

        array = { ["["] ~ value ~ ([","] ~ value)* ~ ["]"] | ["["] ~ ["]"] }

        value = { string | number | object | array | ["true"] | ["false"] | ["null"] }

        string  = @{ ["\""] ~ (escape | !(["\""] | ["\\"]) ~ any)* ~ ["\""] }
        escape  =  { ["\\"] ~ (["\""] | ["\\"] | ["/"] | ["b"] | ["f"] | ["n"] | ["r"] | ["t"] | unicode) }
        unicode =  { ["u"] ~ hex ~ hex ~ hex ~ hex }
        hex     =  { ['0'..'9'] | ['a'..'f'] | ['A'..'F'] }

        number = @{ ["-"]? ~ int ~ ["."] ~ ['0'..'9']+ ~ exp? | ["-"]? ~ int ~ exp | ["-"]? ~ int }
        int    =  { ["0"] | ['1'..'9'] ~ ['0'..'9']* }
        exp    =  { (["E"] | ["e"]) ~ (["+"] | ["-"])? ~ int }

        whitespace = _{ [" "] | ["\t"] | ["\r"] | ["\n"] }
    }
}

#[test]
fn int_zero() {
    let mut parser = Rdp::new(StringInput::new("0"));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::int, start: 0, end: 1 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn int_long() {
    let mut parser = Rdp::new(StringInput::new("234239923610"));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::int, start: 0, end: 12 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp() {
    let mut parser = Rdp::new(StringInput::new("e10"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 3 },
        Token { rule: Rule::int, start: 1, end: 3 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_signed() {
    let mut parser = Rdp::new(StringInput::new("E-0"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::exp, start: 0, end: 3 },
        Token { rule: Rule::int, start: 2, end: 3 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn number_neg_point_exp() {
    let mut parser = Rdp::new(StringInput::new("-13.0593e-10"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 12 },
        Token { rule: Rule::int, start: 1, end: 3 },
        Token { rule: Rule::exp, start: 8, end: 12 },
        Token { rule: Rule::int, start: 10, end: 12 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn number_exp() {
    let mut parser = Rdp::new(StringInput::new("593e-10"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 7 },
        Token { rule: Rule::int, start: 0, end: 3 },
        Token { rule: Rule::exp, start: 3, end: 7 },
        Token { rule: Rule::int, start: 5, end: 7 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn number_neg() {
    let mut parser = Rdp::new(StringInput::new("-200"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::number, start: 0, end: 4 },
        Token { rule: Rule::int, start: 1, end: 4 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn unicode() {
    let mut parser = Rdp::new(StringInput::new("u0a9F"));

    assert!(parser.unicode());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::unicode, start: 0, end: 5 },
        Token { rule: Rule::hex, start: 1, end: 2 },
        Token { rule: Rule::hex, start: 2, end: 3 },
        Token { rule: Rule::hex, start: 3, end: 4 },
        Token { rule: Rule::hex, start: 4, end: 5 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn string() {
    let mut parser = Rdp::new(StringInput::new("\"asd\\u0000\\\"\""));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::string, start: 0, end: 13 },
        Token { rule: Rule::escape, start: 4, end: 10 },
        Token { rule: Rule::unicode, start: 5, end: 10 },
        Token { rule: Rule::hex, start: 6, end: 7 },
        Token { rule: Rule::hex, start: 7, end: 8 },
        Token { rule: Rule::hex, start: 8, end: 9 },
        Token { rule: Rule::hex, start: 9, end: 10 },
        Token { rule: Rule::escape, start: 10, end: 12 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn array_empty() {
    let mut parser = Rdp::new(StringInput::new("[ ]"));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::array, start: 0, end: 3 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn array() {
    let mut parser = Rdp::new(StringInput::new("[0.0e1, false, null, \"a\"]"));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::array, start: 0, end: 25 },
        Token { rule: Rule::value, start: 1, end: 6 },
        Token { rule: Rule::number, start: 1, end: 6 },
        Token { rule: Rule::int, start: 1, end: 2 },
        Token { rule: Rule::exp, start: 4, end: 6 },
        Token { rule: Rule::int, start: 5, end: 6 },
        Token { rule: Rule::value, start: 8, end: 13 },
        Token { rule: Rule::value, start: 15, end: 19 },
        Token { rule: Rule::value, start: 21, end: 24 },
        Token { rule: Rule::string, start: 21, end: 24 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn pair() {
    let mut parser = Rdp::new(StringInput::new("\"a\" : 3"));

    assert!(parser.pair());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::pair, start: 0, end: 7 },
        Token { rule: Rule::string, start: 0, end: 3 },
        Token { rule: Rule::value, start: 6, end: 7 },
        Token { rule: Rule::number, start: 6, end: 7 },
        Token { rule: Rule::int, start: 6, end: 7 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn object() {
    let mut parser = Rdp::new(StringInput::new("{\"a\" : 3, \"b\" : [{}, 3.0e-10]}"));

    assert!(parser.object());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::object, start: 0, end: 30 },
        Token { rule: Rule::pair, start: 1, end: 8 },
        Token { rule: Rule::string, start: 1, end: 4 },
        Token { rule: Rule::value, start: 7, end: 8 },
        Token { rule: Rule::number, start: 7, end: 8 },
        Token { rule: Rule::int, start: 7, end: 8 },
        Token { rule: Rule::pair, start: 10, end: 29 },
        Token { rule: Rule::string, start: 10, end: 13 },
        Token { rule: Rule::value, start: 16, end: 29 },
        Token { rule: Rule::array, start: 16, end: 29 },
        Token { rule: Rule::value, start: 17, end: 19 },
        Token { rule: Rule::object, start: 17, end: 19 },
        Token { rule: Rule::value, start: 21, end: 28 },
        Token { rule: Rule::number, start: 21, end: 28 },
        Token { rule: Rule::int, start: 21, end: 22 },
        Token { rule: Rule::exp, start: 24, end: 28 },
        Token { rule: Rule::int, start: 26, end: 28 }
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn fail_number() {
    let mut parser = Rdp::new(StringInput::new("3f"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::eoi], 1));
}

#[test]
fn fail_number_space() {
    let mut parser = Rdp::new(StringInput::new("3 3"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::eoi], 2));
}

#[test]
fn fail_number_zero() {
    let mut parser = Rdp::new(StringInput::new("03"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::eoi], 1));
}

#[test]
fn fail_exp() {
    let mut parser = Rdp::new(StringInput::new("3.4e"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![Rule::eoi], 3));
}

#[test]
fn fail_string() {
    let mut parser = Rdp::new(StringInput::new("\"a"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![
        Rule::number,
        Rule::string,
        Rule::array,
        Rule::object
    ], 0));
}

#[test]
fn fail_string_unicode() {
    let mut parser = Rdp::new(StringInput::new("\"\\u00p"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![
        Rule::number,
        Rule::string,
        Rule::array,
        Rule::object
    ], 0));
}

#[test]
fn fail_array() {
    let mut parser = Rdp::new(StringInput::new("[1, ]"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![
        Rule::number,
        Rule::string,
        Rule::array,
        Rule::object
    ], 4));
}

#[test]
fn fail_object_pair() {
    let mut parser = Rdp::new(StringInput::new("{\"a\" : }"));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![
        Rule::number,
        Rule::string,
        Rule::array,
        Rule::object
    ], 7));
}

#[test]
fn fail_object_open() {
    let mut parser = Rdp::new(StringInput::new("{\"a\" : "));

    assert!(!parser.json());

    assert_eq!(parser.expected(), (vec![
        Rule::number,
        Rule::string,
        Rule::array,
        Rule::object
    ], 7));
}
