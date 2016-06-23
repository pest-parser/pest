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

        number = @{ ["-"]? ~ int ~ (["."] ~ ['0'..'9']+ ~ exp? | exp)? }
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
        Token::new(Rule::int, 0, 1)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn int_long() {
    let mut parser = Rdp::new(StringInput::new("234239923610"));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::int, 0, 12)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp() {
    let mut parser = Rdp::new(StringInput::new("e10"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 3),
        Token::new(Rule::int, 1, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn exp_signed() {
    let mut parser = Rdp::new(StringInput::new("E-0"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::exp, 0, 3),
        Token::new(Rule::int, 2, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn number_neg_point_exp() {
    let mut parser = Rdp::new(StringInput::new("-13.0593e-10"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 12),
        Token::new(Rule::int, 1, 3),
        Token::new(Rule::exp, 8, 12),
        Token::new(Rule::int, 10, 12)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn number_exp() {
    let mut parser = Rdp::new(StringInput::new("593e-10"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 7),
        Token::new(Rule::int, 0, 3),
        Token::new(Rule::exp, 3, 7),
        Token::new(Rule::int, 5, 7)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn number_neg() {
    let mut parser = Rdp::new(StringInput::new("-200"));

    assert!(parser.number());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::number, 0, 4),
        Token::new(Rule::int, 1, 4)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn unicode() {
    let mut parser = Rdp::new(StringInput::new("u0a9F"));

    assert!(parser.unicode());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::unicode, 0, 5),
        Token::new(Rule::hex, 1, 2),
        Token::new(Rule::hex, 2, 3),
        Token::new(Rule::hex, 3, 4),
        Token::new(Rule::hex, 4, 5)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn string() {
    let mut parser = Rdp::new(StringInput::new("\"asd\\u0000\\\"\""));

    assert!(parser.string());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::string, 0, 13),
        Token::new(Rule::escape, 4, 10),
        Token::new(Rule::unicode, 5, 10),
        Token::new(Rule::hex, 6, 7),
        Token::new(Rule::hex, 7, 8),
        Token::new(Rule::hex, 8, 9),
        Token::new(Rule::hex, 9, 10),
        Token::new(Rule::escape, 10, 12)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn array_empty() {
    let mut parser = Rdp::new(StringInput::new("[ ]"));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::array, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn array() {
    let mut parser = Rdp::new(StringInput::new("[0.0e1, false, null, \"a\"]"));

    assert!(parser.array());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::array, 0, 25),
        Token::new(Rule::value, 1, 6),
        Token::new(Rule::number, 1, 6),
        Token::new(Rule::int, 1, 2),
        Token::new(Rule::exp, 4, 6),
        Token::new(Rule::int, 5, 6),
        Token::new(Rule::value, 8, 13),
        Token::new(Rule::value, 15, 19),
        Token::new(Rule::value, 21, 24),
        Token::new(Rule::string, 21, 24)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn pair() {
    let mut parser = Rdp::new(StringInput::new("\"a\" : 3"));

    assert!(parser.pair());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::pair, 0, 7),
        Token::new(Rule::string, 0, 3),
        Token::new(Rule::value, 6, 7),
        Token::new(Rule::number, 6, 7),
        Token::new(Rule::int, 6, 7)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn object() {
    let mut parser = Rdp::new(StringInput::new("{\"a\" : 3, \"b\" : [{}, 3.0e-10]}"));

    assert!(parser.object());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::object, 0, 30),
        Token::new(Rule::pair, 1, 8),
        Token::new(Rule::string, 1, 4),
        Token::new(Rule::value, 7, 8),
        Token::new(Rule::number, 7, 8),
        Token::new(Rule::int, 7, 8),
        Token::new(Rule::pair, 10, 29),
        Token::new(Rule::string, 10, 13),
        Token::new(Rule::value, 16, 29),
        Token::new(Rule::array, 16, 29),
        Token::new(Rule::value, 17, 19),
        Token::new(Rule::object, 17, 19),
        Token::new(Rule::value, 21, 28),
        Token::new(Rule::number, 21, 28),
        Token::new(Rule::int, 21, 22),
        Token::new(Rule::exp, 24, 28),
        Token::new(Rule::int, 26, 28)
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
